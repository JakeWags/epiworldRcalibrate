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

#' Find available Python installation
#' @keywords internal
.find_python <- function() {
  user_py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (nzchar(user_py) && file.exists(user_py)) return(user_py)

  py_config <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  if (!is.null(py_config) && !is.null(py_config$python))
    return(py_config$python)

  if (.Platform$OS.type == "windows") {
    candidates <- c("python.exe", "python3.exe",
                    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Python/*/python.exe"))
  } else {
    candidates <- c(
      "/usr/bin/python3", "/usr/local/bin/python3",
      "/usr/bin/python", "/usr/local/bin/python",
      "/opt/homebrew/bin/python3",
      paste0(Sys.getenv("HOME"), "/.pyenv/shims/python3")
    )
  }

  for (py in candidates) {
    if (file.exists(py)) return(py)
    if (grepl("\\*", py)) {
      matches <- Sys.glob(py)
      if (length(matches) > 0) return(matches[1])
    }
  }

  py_path <- tryCatch({
    if (.Platform$OS.type == "windows") {
      system2("where", "python", stdout = TRUE, stderr = FALSE)[1]
    } else {
      system2("which", "python3", stdout = TRUE, stderr = FALSE)[1]
    }
  }, error = function(e) NULL)

  if (!is.null(py_path) && nzchar(py_path) && file.exists(py_path))
    return(py_path)

  return(NULL)
}

#' Ensure Python + necessary modules are ready
#' @keywords internal
.ensure_python_ready <- function() {

  user_py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (nzchar(user_py)) {
    message("Using user-specified Python: ", user_py)
    reticulate::use_python(user_py, required = TRUE)
  } else {
    vname <- .bilstm_env$venv_name
    envs <- tryCatch(reticulate::virtualenv_list(), error = function(e) character())
    if (vname %in% envs) {
      reticulate::use_virtualenv(vname, required = FALSE)
    } else {
      python_path <- .find_python()
      if (is.null(python_path))
        stop("Could not find Python. Install Python 3.7+ or set RETICULATE_PYTHON.")

      reticulate::virtualenv_create(vname, python = python_path)
      reticulate::use_virtualenv(vname, required = FALSE)
    }
  }

  if (!reticulate::py_available(initialize = TRUE))
    stop("Python could not be initialized.")

  needs <- c(
    numpy = !reticulate::py_module_available("numpy"),
    sklearn = !reticulate::py_module_available("sklearn"),
    joblib = !reticulate::py_module_available("joblib"),
    torch = !reticulate::py_module_available("torch")
  )

  if (any(needs)) {
    pkgs <- names(needs)[needs]
    pkgs[pkgs == "sklearn"] <- "scikit-learn"

    non_torch <- setdiff(pkgs, "torch")
    if (length(non_torch)) reticulate::py_install(non_torch, pip = TRUE)

    if ("torch" %in% pkgs) {
      reticulate::py_install(
        "torch", pip = TRUE,
        pip_options = c("--index-url", "https://download.pytorch.org/whl/cpu")
      )
    }
  }

  critical <- c("numpy", "sklearn", "joblib", "torch")
  missing <- critical[!sapply(critical, reticulate::py_module_available)]
  if (length(missing))
    stop("Missing Python modules: ", paste(missing, collapse = ", "))
}

# =============================================================================
# Embedded Python
# =============================================================================

#' @keywords internal
.get_python_model_code <- function() {
  '
import torch
import torch.nn as nn
import joblib
import numpy as np

_model = None
_scaler_additional = None
_scaler_targets = None
_device = torch.device("cpu")

class BiLSTMModel(nn.Module):
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
            self.sigmoid(out[:, 0]),
            self.softplus(out[:, 1]),
            self.softplus(out[:, 2]),
        ], dim=1)
        return out

def preprocess_incidence_data(raw_counts):
    raw_counts = np.asarray(raw_counts, dtype=float)
    percentage_changes = np.zeros_like(raw_counts)
    percentage_changes[1:] = np.where(
        raw_counts[:-1] != 0,
        (raw_counts[1:] - raw_counts[:-1]) / raw_counts[:-1],
        0
    )
    return percentage_changes

def load_model_components(model_path, scaler_add_path, scaler_tgt_path):
    global _model, _scaler_additional, _scaler_targets

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

def predict_sir_parameters(raw_incidence_counts, population_size, recovery_rate):
    global _model, _scaler_additional, _scaler_targets
    if _model is None:
        raise RuntimeError("Model not loaded.")

    raw_incidence_counts = np.asarray(raw_incidence_counts, dtype=float)
    if len(raw_incidence_counts) != 61:
        raise ValueError(f"Expected 61 daily counts, got {len(raw_incidence_counts)}")

    processed_incidence = preprocess_incidence_data(raw_incidence_counts)

    X_tensor = torch.tensor(processed_incidence.reshape(1, -1, 1),
                            dtype=torch.float32, device=_device)

    add_inputs = np.array([[population_size, recovery_rate]], dtype=float)
    add_inputs_scaled = _scaler_additional.transform(add_inputs)
    add_tensor = torch.tensor(add_inputs_scaled,
                              dtype=torch.float32, device=_device)

    with torch.no_grad():
        pred_scaled = _model(X_tensor, add_tensor).cpu().numpy()
        pred_original = _scaler_targets.inverse_transform(pred_scaled)

    return pred_original[0].tolist()

def cleanup_model():
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

#' @keywords internal
.validate_model_directory <- function(model_dir) {
  if (!dir.exists(model_dir))
    stop("Model directory does not exist: ", model_dir)

  base_dir <- normalizePath(model_dir, winslash = "/", mustWork = TRUE)
  file_paths <- list(
    model = file.path(base_dir, "model4_bilstm_relchange_no_eps.pt"),
    scaler_add = file.path(base_dir, "scaler_additional_no_eps.pkl"),
    scaler_tgt = file.path(base_dir, "scaler_targets_no_eps.pkl")
  )

  missing <- names(file_paths)[!vapply(file_paths, file.exists, logical(1))]
  if (length(missing))
    stop("Missing model files: ", paste(missing, collapse = ", "))

  file_paths
}

#' @keywords internal
.get_model_directory <- function(model_dir = NULL) {
  if (!is.null(model_dir)) return(model_dir)

  pkg_model_dir <- system.file("models", package = "epiworldRcalibrate")
  if (pkg_model_dir == "") stop("Model directory not found.")
  pkg_model_dir
}

#' @keywords internal
.validate_sir_inputs <- function(daily_cases, population_size, recovery_rate) {
  if (!is.numeric(daily_cases)) stop("daily_cases must be numeric")
  if (length(daily_cases) != 61)
    stop("daily_cases must have length 61")
  if (any(daily_cases < 0)) stop("daily_cases cannot be negative")
  if (!is.numeric(population_size) || length(population_size) != 1)
    stop("population_size must be single numeric")
  if (population_size <= 0) stop("population_size must be positive")
  if (!is.numeric(recovery_rate) || length(recovery_rate) != 1)
    stop("recovery_rate must be single numeric")
  if (recovery_rate <= 0) stop("recovery_rate must be positive")
}

# =============================================================================
# Core API
# =============================================================================

#' Initialize BiLSTM Model for SIR Parameter Estimation
#'
#' @param model_dir Optional path to model directory. Defaults to the
#'   package's bundled model files.
#' @param force_reload Logical; reload even if already loaded.
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
init_bilstm_model <- function(model_dir = NULL, force_reload = FALSE) {

  model_dir <- .get_model_directory(model_dir)

  if (.bilstm_env$model_loaded &&
      !force_reload &&
      identical(.bilstm_env$model_dir, model_dir)) {
    message("Model already loaded.")
    return(invisible(TRUE))
  }

  file_paths <- .validate_model_directory(model_dir)
  .ensure_python_ready()

  reticulate::py_run_string(.get_python_model_code())

  reticulate::py$load_model_components(
    model_path = file_paths$model,
    scaler_add_path = file_paths$scaler_add,
    scaler_tgt_path = file_paths$scaler_tgt
  )

  .bilstm_env$model_loaded <- TRUE
  .bilstm_env$model_dir <- model_dir
  message("BiLSTM model loaded successfully.")
  invisible(TRUE)
}

#' Estimate SIR Parameters from 61-day incidence
#'
#' @param daily_cases Numeric vector of length 61 containing daily incidence
#'   counts for days 0 to 60.
#' @param population_size Single numeric value giving the total population size
#'   used in the SIR model.
#' @param recovery_rate Single numeric value giving the recovery rate parameter
#'   of the SIR model.
#'
#' @return Named numeric vector: `ptran`, `crate`, `R0`.
#' @export
estimate_sir_parameters <- function(daily_cases, population_size, recovery_rate) {
  if (!.bilstm_env$model_loaded)
    stop("Model not loaded. Call init_bilstm_model().")

  .validate_sir_inputs(daily_cases, population_size, recovery_rate)

  out <- reticulate::py$predict_sir_parameters(
    as.numeric(daily_cases),
    as.numeric(population_size),
    as.numeric(recovery_rate)
  )
  names(out) <- c("ptran", "crate", "R0")
  out
}

#' Calibrate SIR Parameters (one-step)
#'
#' This is a convenience wrapper that optionally initializes the BiLSTM model
#' and then calls [estimate_sir_parameters()] on the provided data.
#'
#' @param daily_cases Numeric vector of length 61 containing daily incidence
#'   counts (day 0 to day 60).
#' @param population_size Single numeric value giving the total population size.
#' @param recovery_rate Single numeric value giving the recovery rate parameter.
#' @param model_dir Optional path to the directory containing the trained
#'   BiLSTM model and scaler files. If `NULL`, the package's bundled assets
#'   are used.
#' @param auto_init Logical; if `TRUE` (default), automatically calls
#'   [init_bilstm_model()] when the model is not yet loaded.
#'
#' @return Named numeric vector: `ptran`, `crate`, `R0`.
#' @export
calibrate_sir <- function(daily_cases,
                          population_size,
                          recovery_rate,
                          model_dir = NULL,
                          auto_init = TRUE) {

  if (!.bilstm_env$model_loaded && auto_init)
    init_bilstm_model(model_dir)

  estimate_sir_parameters(daily_cases, population_size, recovery_rate)
}

# =============================================================================
# Utilities
# =============================================================================

#' Demonstrate preprocessing (not required for prediction)
#'
#' @param raw_counts Numeric vector of raw daily incidence counts.
#'
#' @return A data frame with columns `day`, `raw_count`, and `percentage_change`.
#' @export
show_preprocessing <- function(raw_counts) {
  if (!is.numeric(raw_counts))
    stop("raw_counts must be numeric")

  py_code <- '
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

  reticulate::py_run_string(py_code)
  processed <- reticulate::py$show_preprocessing_temp(as.numeric(raw_counts))

  data.frame(
    day = seq_along(raw_counts) - 1,
    raw_count = raw_counts,
    percentage_change = round(processed, 4),
    stringsAsFactors = FALSE
  )
}

#' Check model status
#'
#' @return A list describing model load status and Python config.
#' @export
check_model_status <- function() {
  list(
    loaded = .bilstm_env$model_loaded,
    model_directory = .bilstm_env$model_dir,
    python_config = tryCatch(
      utils::capture.output(reticulate::py_config()),
      error = function(e) NULL
    )
  )
}

#' Unload the model from memory
#'
#' @return Invisibly `TRUE` on success.
#' @export
cleanup_model <- function() {
  if (!.bilstm_env$model_loaded) {
    message("No model loaded.")
    return(invisible(TRUE))
  }

  try(reticulate::py$cleanup_model(), silent = TRUE)
  .bilstm_env$model_loaded <- FALSE
  .bilstm_env$model_dir <- NULL
  message("Model cleaned up.")
  invisible(TRUE)
}
