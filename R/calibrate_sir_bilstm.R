#' @keywords internal
"_PACKAGE"

# =============================================================================
# Internal State
# =============================================================================

# Internal env to track BiLSTM model state
#' @keywords internal
.bilstm_env <- new.env(parent = emptyenv())
.bilstm_env$model_loaded <- FALSE
.bilstm_env$model_dir    <- NULL
.bilstm_env$venv_name    <- "epiworldRcalibrate"

# =============================================================================
# Python bootstrap (private)
# =============================================================================

#' Ensure Python is usable and required modules are present.
#' - Respects RETICULATE_PYTHON if the user has set it.
#' - Otherwise creates/uses a private virtualenv 'epiworldRcalibrate'
#' - Installs numpy, joblib, torch (CPU) if missing.
#' @keywords internal
.ensure_python_ready <- function() {
  # If the user pinned a Python, respect it.
  user_py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (nzchar(user_py)) {
    try(reticulate::use_python(user_py, required = FALSE), silent = TRUE)
  } else {
    vname <- .bilstm_env$venv_name

    # Does our private env exist?
    envs <- tryCatch(reticulate::virtualenv_list(), error = function(e) character())
    if (!(vname %in% envs)) {
      message("Creating private Python env '", vname, "'...")
      reticulate::virtualenv_create(envname = vname, python = NULL)
    }

    # Activate it for this session
    reticulate::use_virtualenv(virtualenv = vname, required = FALSE)
  }

  # Now ensure required modules are present
  needs <- c(
    numpy = !reticulate::py_module_available("numpy"),
    joblib = !reticulate::py_module_available("joblib"),
    torch  = !reticulate::py_module_available("torch")
  )

  if (any(needs)) {
    pkgs <- names(needs)[needs]
    message("Installing missing Python packages: ", paste(pkgs, collapse = ", "))

    # Install numpy/joblib first if torch is also missing
    base_pkgs <- setdiff(pkgs, "torch")
    if (length(base_pkgs)) {
      reticulate::py_install(base_pkgs, envname = .bilstm_env$venv_name, pip = TRUE)
    }

    if ("torch" %in% pkgs) {
      # Prefer CPU wheels; fall back to default index if that fails
      ok <- TRUE
      tryCatch({
        reticulate::py_install(
          packages    = "torch",
          envname     = .bilstm_env$venv_name,
          pip         = TRUE,
          pip_options = c("--index-url", "https://download.pytorch.org/whl/cpu")
        )
      }, error = function(e) ok <<- FALSE)
      if (!ok) {
        reticulate::py_install("torch", envname = .bilstm_env$venv_name, pip = TRUE)
      }
    }
  }

  # Final sanity check
  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Could not initialize Python. ",
      "If you have a system Python, set RETICULATE_PYTHON to it and retry.",
      call. = FALSE
    )
  }
}

# =============================================================================
# Embedded Python (as a string)
# =============================================================================

#' Get Python model code
#' @keywords internal
.get_python_model_code <- function() {
  '
import torch
import torch.nn as nn
import joblib
import numpy as np

# Global variables to store model and scalers
_model = None
_scaler_additional = None
_scaler_targets = None
_device = torch.device("cpu")

class BiLSTMModel(nn.Module):
    """BiLSTM model architecture for SIR parameter estimation"""
    def __init__(self, input_dim, hidden_dim, num_layers, additional_dim, output_dim, dropout):
        super().__init__()
        self.bilstm = nn.LSTM(
            input_size=input_dim,
            hidden_size=hidden_dim,
            num_layers=num_layers,
            batch_first=True,
            dropout=dropout,
            bidirectional=True,
        )
        self.fc1 = nn.Linear(2 * hidden_dim + additional_dim, 64)
        self.fc2 = nn.Linear(64, output_dim)
        self.sigmoid = nn.Sigmoid()
        self.softplus = nn.Softplus()

    def forward(self, x, add_inputs):
        _, (h_n, _) = self.bilstm(x)
        h = torch.cat((h_n[-2], h_n[-1]), dim=1)
        h = torch.relu(self.fc1(torch.cat((h, add_inputs), dim=1)))
        out = self.fc2(h)
        out = torch.stack([
            self.sigmoid(out[:, 0]),    # ptran (0-1)
            self.softplus(out[:, 1]),   # crate (>0)
            self.softplus(out[:, 2]),   # R0 (>0)
        ], dim=1)
        return out

def preprocess_incidence_data(raw_counts):
    """
    Convert raw daily incidence counts to percentage changes.
    - Day 0: 0
    - Else: (c[i] - c[i-1]) / c[i-1] if c[i-1] > 0, else 0
    """
    raw_counts = np.asarray(raw_counts, dtype=float)
    percentage_changes = np.zeros_like(raw_counts)
    percentage_changes[1:] = np.where(
        raw_counts[:-1] != 0,
        (raw_counts[1:] - raw_counts[:-1]) / raw_counts[:-1],
        0
    )
    return percentage_changes

def load_model_components(model_path, scaler_add_path, scaler_tgt_path):
    """Load the trained model and scalers"""
    global _model, _scaler_additional, _scaler_targets

    try:
        _scaler_additional = joblib.load(scaler_add_path)
        _scaler_targets = joblib.load(scaler_tgt_path)

        _model = BiLSTMModel(
            input_dim=1,
            hidden_dim=160,
            num_layers=3,
            additional_dim=2,
            output_dim=3,
            dropout=0.5
        )

        state_dict = torch.load(model_path, map_location=_device)
        _model.load_state_dict(state_dict)
        _model.to(_device)
        _model.eval()
        return True

    except Exception as e:
        raise RuntimeError(f"Failed to load model components: {str(e)}")

def predict_sir_parameters(raw_incidence_counts, population_size, recovery_rate):
    """Predict [ptran, crate, R0] from raw incidence counts (length 61)"""
    global _model, _scaler_additional, _scaler_targets
    if _model is None:
        raise RuntimeError("Model not loaded. Call load_model_components() first.")

    raw_incidence_counts = np.asarray(raw_incidence_counts, dtype=float)
    if len(raw_incidence_counts) != 61:
        raise ValueError(f"Expected 61 daily counts, got {len(raw_incidence_counts)}")

    processed_incidence = preprocess_incidence_data(raw_incidence_counts)

    X_tensor = torch.tensor(
        processed_incidence.reshape(1, -1, 1),
        dtype=torch.float32,
        device=_device
    )

    add_inputs = np.array([[population_size, recovery_rate]], dtype=float)
    add_inputs_scaled = _scaler_additional.transform(add_inputs)
    add_tensor = torch.tensor(add_inputs_scaled, dtype=torch.float32, device=_device)

    with torch.no_grad():
        pred_scaled = _model(X_tensor, add_tensor).cpu().numpy()
        pred_original = _scaler_targets.inverse_transform(pred_scaled)

    return pred_original[0].tolist()

def cleanup_model():
    """Clean up model and scalers from memory"""
    global _model, _scaler_additional, _scaler_targets
    _model = None
    _scaler_additional = None
    _scaler_targets = None
    return True
'
}

# =============================================================================
# Helpers
# =============================================================================

# Validate model directory and get file paths
#' @keywords internal
.validate_model_directory <- function(model_dir) {
  if (!dir.exists(model_dir)) {
    stop("Model directory does not exist: ", model_dir, call. = FALSE)
  }
  base_dir <- normalizePath(model_dir, winslash = "/", mustWork = TRUE)

  file_paths <- list(
    model      = file.path(base_dir, "model4_bilstm_relchange_no_eps.pt"),
    scaler_add = file.path(base_dir, "scaler_additional_no_eps.pkl"),
    scaler_tgt = file.path(base_dir, "scaler_targets_no_eps.pkl")
  )

  missing <- names(file_paths)[!vapply(file_paths, file.exists, logical(1))]
  if (length(missing) > 0) {
    stop(
      "Required model files not found: ",
      paste(basename(unlist(file_paths[missing], use.names = FALSE)), collapse = ", "),
      call. = FALSE
    )
  }
  file_paths
}

# Get default model directory (inst/models)
#' @keywords internal
.get_model_directory <- function(model_dir = NULL) {
  if (!is.null(model_dir)) return(model_dir)
  pkg_model_dir <- system.file("models", package = "epiworldRcalibrate")
  if (pkg_model_dir == "") {
    stop(
      "Model directory not found. Ensure the package is installed with model assets, ",
      "or provide model_dir explicitly.",
      call. = FALSE
    )
  }
  pkg_model_dir
}

# Validate inputs
#' @keywords internal
.validate_sir_inputs <- function(daily_cases, population_size, recovery_rate) {
  if (!is.numeric(daily_cases)) stop("daily_cases must be numeric", call. = FALSE)
  if (length(daily_cases) != 61) {
    stop(
      "daily_cases must contain exactly 61 values (day 0 to day 60). Received: ",
      length(daily_cases), call. = FALSE
    )
  }
  if (any(daily_cases < 0)) stop("daily_cases cannot contain negative values", call. = FALSE)
  if (!is.numeric(population_size) || length(population_size) != 1)
    stop("population_size must be a single numeric value", call. = FALSE)
  if (population_size <= 0) stop("population_size must be positive", call. = FALSE)
  if (!is.numeric(recovery_rate) || length(recovery_rate) != 1)
    stop("recovery_rate must be a single numeric value", call. = FALSE)
  if (recovery_rate <= 0) stop("recovery_rate must be positive", call. = FALSE)
  invisible(TRUE)
}

# =============================================================================
# Core API
# =============================================================================

#' Initialize BiLSTM Model for SIR Parameter Estimation
#' @param model_dir Optional path to directory with model files.
#' @param force_reload Force reload even if already loaded.
#' @return (invisible) TRUE on success
#' @export
init_bilstm_model <- function(model_dir = NULL, force_reload = FALSE) {

  # Resolve model dir bundled in the package unless overridden
  model_dir <- .get_model_directory(model_dir)

  # Early exit if already loaded and same dir
  if (.bilstm_env$model_loaded && !force_reload && identical(.bilstm_env$model_dir, model_dir)) {
    message("BiLSTM model already loaded. Use force_reload=TRUE to reload.")
    return(invisible(TRUE))
  }

  # Check files exist
  file_paths <- .validate_model_directory(model_dir)

  # Make sure Python & modules are ready
  .ensure_python_ready()

  # Initialize Python namespace with our model code
  tryCatch({
    reticulate::py_run_string(.get_python_model_code())
    message("Python environment initialized.")
  }, error = function(e) {
    stop("Failed to initialize Python environment: ", e$message, call. = FALSE)
  })

  # Load model & scalers
  tryCatch({
    reticulate::py$load_model_components(
      model_path      = file_paths[["model"]],
      scaler_add_path = file_paths[["scaler_add"]],
      scaler_tgt_path = file_paths[["scaler_tgt"]]
    )
    .bilstm_env$model_loaded <- TRUE
    .bilstm_env$model_dir    <- model_dir
    message("BiLSTM model loaded successfully. Ready to estimate SIR parameters.")
    invisible(TRUE)
  }, error = function(e) {
    .bilstm_env$model_loaded <- FALSE
    stop("Failed to load model: ", e$message, call. = FALSE)
  })
}

#' Estimate SIR Parameters from 61-day incidence
#' @return named numeric vector: ptran, crate, R0
#' @export
estimate_sir_parameters <- function(daily_cases, population_size, recovery_rate) {
  if (!.bilstm_env$model_loaded) {
    stop("BiLSTM model not loaded. Please call init_bilstm_model() first.", call. = FALSE)
  }

  .validate_sir_inputs(daily_cases, population_size, recovery_rate)

  tryCatch({
    result <- reticulate::py$predict_sir_parameters(
      as.numeric(daily_cases),
      as.numeric(population_size),
      as.numeric(recovery_rate)
    )
    names(result) <- c("ptran", "crate", "R0")
    result
  }, error = function(e) {
    stop("Parameter estimation failed: ", e$message, call. = FALSE)
  })
}

#' Calibrate SIR Parameters (one-step)
#' @export
calibrate_sir <- function(daily_cases,
                          population_size,
                          recovery_rate,
                          model_dir = NULL,
                          auto_init = TRUE) {
  if (!.bilstm_env$model_loaded && auto_init) {
    message("Model not loaded. Initializing automatically...")
    init_bilstm_model(model_dir = model_dir)
  }
  estimate_sir_parameters(daily_cases, population_size, recovery_rate)
}

# =============================================================================
# Utilities
# =============================================================================

#' Demonstrate preprocessing (not required for prediction)
#' @export
show_preprocessing <- function(raw_counts) {
  if (!is.numeric(raw_counts)) stop("raw_counts must be numeric", call. = FALSE)

  python_code <- '
def show_preprocessing_temp(counts):
    import numpy as np
    counts = np.asarray(counts, dtype=float)
    processed = np.zeros_like(counts)
    processed[1:] = np.where(
        counts[:-1] != 0,
        (counts[1:] - counts[:-1]) / counts[:-1],
        0
    )
    return processed.tolist()
'
  tryCatch({
    reticulate::py_run_string(python_code)
    processed <- reticulate::py$show_preprocessing_temp(as.numeric(raw_counts))
    out <- data.frame(
      day = 0:(length(raw_counts) - 1),
      raw_count = raw_counts,
      percentage_change = round(processed, 4),
      stringsAsFactors = FALSE
    )
    if (length(raw_counts) != 61) {
      message("Note: For model predictions, you need exactly 61 days. Currently showing ",
              length(raw_counts), " days.")
    }
    out
  }, error = function(e) {
    stop("Preprocessing demonstration failed: ", e$message, call. = FALSE)
  })
}

#' Check model status
#' @export
check_model_status <- function() {
  list(
    loaded          = .bilstm_env$model_loaded,
    model_directory = .bilstm_env$model_dir,
    python_config   = tryCatch(utils::capture.output(reticulate::py_config()), error = function(e) NULL)
  )
}

#' Clean up model from memory
#' @export
cleanup_model <- function() {
  if (!.bilstm_env$model_loaded) {
    message("No model currently loaded.")
    return(invisible(TRUE))
  }
  ok <- TRUE
  tryCatch({
    reticulate::py$cleanup_model()
  }, error = function(e) {
    ok <<- FALSE
    warning("Error during Python cleanup: ", e$message, call. = FALSE)
  })
  .bilstm_env$model_loaded <- FALSE
  .bilstm_env$model_dir    <- NULL
  if (ok) message("Model cleaned up successfully.")
  invisible(TRUE)
}
