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

#' @keywords internal
.has_reticulate_conda <- function() {
  tryCatch({
    nzchar(reticulate::conda_binary("auto"))
  }, error = function(e) FALSE)
}

#' @keywords internal
.get_conda_env_names <- function() {
  envs <- tryCatch(reticulate::conda_list(conda = "auto"),
                   error = function(e) NULL)

  if (is.null(envs) || !is.data.frame(envs) || !"name" %in% names(envs)) {
    return(character())
  }

  unique(as.character(envs$name))
}

#' @keywords internal
.ensure_reticulate_conda <- function() {
  if (!.has_reticulate_conda()) {
    message("Installing Miniconda via reticulate...")
    reticulate::install_miniconda(force = FALSE, update = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
.activate_package_conda_env <- function(required = FALSE) {
  envname <- .bilstm_env$venv_name
  envs <- .get_conda_env_names()

  if (!(envname %in% envs)) {
    if (required) {
      stop(
        "Python environment '", envname, "' is not set up.\n",
        "Run epiworldRcalibrate::setup_python_deps() first.",
        call. = FALSE
      )
    }
    return(FALSE)
  }

  tryCatch({
    reticulate::use_condaenv(envname, required = required)
    TRUE
  }, error = function(e) {
    if (required) {
      stop(
        "Could not activate Python environment '", envname, "': ",
        conditionMessage(e),
        call. = FALSE
      )
    }
    FALSE
  })
}

#' Ensure Python + necessary modules are ready (no auto-install)
#'
#' This function checks that Python is available and that required packages
#' are importable. It does NOT install any packages automatically. If packages
#' are missing, it stops with an informative message directing the user to
#' \code{\link{setup_python_deps}}.
#'
#' @keywords internal
.ensure_python_ready <- function() {
  envname <- .bilstm_env$venv_name

  # If Python is already initialized in this R session, ensure it's the
  # package-managed environment.
  active_initialized <- tryCatch(reticulate::py_available(initialize = FALSE),
                                 error = function(e) FALSE)
  if (active_initialized) {
    active_py <- tryCatch(reticulate::py_config()$python,
                          error = function(e) NULL)
    if (is.null(active_py) || !nzchar(active_py) ||
        !grepl(envname, active_py, fixed = TRUE)) {
      stop(
        "Python is already initialized with a different interpreter.\n",
        "Please restart R and run epiworldRcalibrate::setup_python_deps() first.",
        call. = FALSE
      )
    }
  } else {
    .activate_package_conda_env(required = TRUE)
  }

  # Step 2: Verify Python is available
  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python could not be initialized.\n",
      "Please run:\n",
      "  epiworldRcalibrate::setup_python_deps()",
      call. = FALSE
    )
  }

  # Step 3: Verify required packages are importable (NO installation)
  message("Verifying Python packages...")
  verification <- .verify_python_imports(modules = .required_python_modules())

  for (pkg_name in names(verification$details)) {
    pkg_result <- verification$details[[pkg_name]]
    if (identical(pkg_result$status, "ok")) {
      message("[OK] ", pkg_name, " v", pkg_result$version)
    } else {
      message("[FAIL] ", pkg_name, ": ", pkg_result$error)
    }
  }

  if (!verification$ok) {
    stop(
      "The following required Python packages are not available in the active Python: ",
      paste(verification$failed, collapse = ", "), ".\n\n",
      "Please run the one-time setup function:\n\n",
      "  epiworldRcalibrate::setup_python_deps()\n\n",
      "If Python was already initialized with a different interpreter, restart R and try again.",
      call. = FALSE
    )
  }

  message("All required Python packages are ready!")
  invisible(TRUE)
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
import warnings
from sklearn.preprocessing import MinMaxScaler

# Suppress sklearn version warnings
warnings.filterwarnings("ignore", category=UserWarning, module="sklearn")

_model = None
_scaler_add = None
_scaler_tgt = None
_scaler_inc = None
_device = torch.device("cpu")

INCIDENCE_MIN = 0
INCIDENCE_MAX = 10000

class BiLSTMModel(nn.Module):
    def __init__(self, input_dim, hidden_dim, num_layers, additional_dim,
                 output_dim, dropout):
        super().__init__()
        self.bilstm = nn.LSTM(input_dim, hidden_dim, num_layers,
                              batch_first=True, dropout=dropout,
                              bidirectional=True)
        self.fc1 = nn.Linear(2 * hidden_dim + additional_dim, 64)
        self.fc2 = nn.Linear(64, output_dim)
        self.sigmoid = nn.Sigmoid()
        self.softplus = nn.Softplus()

    def forward(self, x, additional_inputs):
        _, (h_n, _) = self.bilstm(x)
        hid = torch.cat((h_n[-2], h_n[-1]), dim=1)
        combined = torch.cat((hid, additional_inputs), dim=1)
        x = torch.relu(self.fc1(combined))
        out = self.fc2(x)
        return torch.stack([
            self.sigmoid(out[:, 0]),
            self.softplus(out[:, 1]),
            self.softplus(out[:, 2])
        ], dim=1)

def create_fixed_incidence_scaler(shape):
    scaler = MinMaxScaler(feature_range=(0, 1))
    scaler.data_min_ = np.zeros(shape)
    scaler.data_max_ = np.ones(shape) * INCIDENCE_MAX
    scaler.data_range_ = scaler.data_max_ - scaler.data_min_
    scaler.scale_ = 1.0 / scaler.data_range_
    scaler.min_ = 0 - scaler.data_min_ * scaler.scale_
    return scaler

def load_model_components(model_path, scaler_add_path, scaler_tgt_path,
                          scaler_inc_path=None):
    global _model, _scaler_add, _scaler_tgt, _scaler_inc

    _scaler_add = joblib.load(scaler_add_path)
    _scaler_tgt = joblib.load(scaler_tgt_path)

    if scaler_inc_path:
        try:
            _scaler_inc = joblib.load(scaler_inc_path)
        except:
            _scaler_inc = None
    else:
        _scaler_inc = None

    _model = BiLSTMModel(input_dim=1, hidden_dim=160, num_layers=3,
                         additional_dim=2, output_dim=3, dropout=0.5)
    state = torch.load(model_path, map_location=_device)
    _model.load_state_dict(state)
    _model.to(_device).eval()

def predict_sir_parameters(seq, additional_pair):
    global _scaler_inc
    x = np.asarray(seq, dtype=np.float32).reshape(1, -1, 1)

    if _scaler_inc is None:
        _scaler_inc = create_fixed_incidence_scaler(x.shape[1])

    x_scaled = _scaler_inc.transform(x.reshape(1, -1)).reshape(1, -1, 1)
    add_np = np.array([additional_pair], dtype=np.float32)
    add_scaled = _scaler_add.transform(add_np)

    x_t = torch.tensor(x_scaled, dtype=torch.float32, device=_device)
    add_t = torch.tensor(add_scaled, dtype=torch.float32, device=_device)

    with torch.no_grad():
        out = _model(x_t, add_t).cpu().numpy()

    return _scaler_tgt.inverse_transform(out)[0].tolist()

def cleanup_model():
    global _model, _scaler_add, _scaler_tgt, _scaler_inc
    _model = None
    _scaler_add = None
    _scaler_tgt = None
    _scaler_inc = None
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
    model = file.path(base_dir, "model4_bilstm.pt"),
    scaler_add = file.path(base_dir, "scaler_additional.pkl"),
    scaler_tgt = file.path(base_dir, "scaler_targets.pkl"),
    scaler_inc = file.path(base_dir, "scaler_incidence.pkl")
  )

  # Check required files (scaler_inc is optional)
  required <- c("model", "scaler_add", "scaler_tgt")
  missing <- required[!vapply(file_paths[required], file.exists, logical(1))]
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

#' Return required Python module names
#' @keywords internal
.required_python_modules <- function() {
  c("numpy", "sklearn", "joblib", "torch")
}

#' Resolve the Python target used by package setup and diagnostics
#' @keywords internal
.resolve_python_target <- function() {
  envname <- .bilstm_env$venv_name

  has_conda <- .has_reticulate_conda()
  envs <- if (has_conda) .get_conda_env_names() else character()
  env_exists <- envname %in% envs
  env_activated <- FALSE

  if (env_exists) {
    env_activated <- isTRUE(tryCatch(
      .activate_package_conda_env(required = FALSE),
      error = function(e) FALSE
    ))
  }

  py_ready <- tryCatch(
    reticulate::py_available(initialize = isTRUE(env_activated)),
                       error = function(e) FALSE)
  py_cfg <- if (py_ready) {
    tryCatch(reticulate::py_config(), error = function(e) NULL)
  } else {
    NULL
  }

  python_path <- if (!is.null(py_cfg) && !is.null(py_cfg$python)) {
    py_cfg$python
  } else {
    NULL
  }

  python_version <- if (!is.null(py_cfg) && !is.null(py_cfg$version)) {
    as.character(py_cfg$version)
  } else {
    NULL
  }

  python_source <- NULL
  if (isTRUE(py_ready)) {
    if (!is.null(python_path) && grepl(envname, python_path, fixed = TRUE)) {
      python_source <- "package_conda_env"
    } else {
      python_source <- "active_session"
    }
  }

  list(
    python_available = isTRUE(py_ready),
    python_path = python_path,
    python_version = python_version,
    virtualenv = NULL,
    conda_env = if (env_exists) envname else NULL,
    python_source = python_source
  )
}

#' Check package conda environment health
#' @keywords internal
.check_conda_env_health <- function(envname) {
  if (!.has_reticulate_conda()) {
    return(list(ok = FALSE, message = "reticulate conda is not available."))
  }

  if (!(envname %in% .get_conda_env_names())) {
    return(list(ok = FALSE, message = "Conda environment does not exist."))
  }

  ok <- tryCatch({
    reticulate::use_condaenv(envname, required = TRUE)
    reticulate::py_available(initialize = TRUE)
  }, error = function(e) FALSE)

  if (!isTRUE(ok)) {
    return(list(ok = FALSE,
                message = "Could not initialize Python from conda environment."))
  }

  list(ok = TRUE, message = "Conda environment is healthy.")
}

#' Install Python packages with retry logic
#' @keywords internal
.install_python_packages <- function(packages,
                                     envname,
                                     method = "conda",
                                     pip_options = NULL,
                                     retries = 2) {
  attempt <- 1L
  last_error <- NULL

  while (attempt <= retries) {
    ok <- tryCatch({
      reticulate::py_install(
        packages,
        envname = envname,
        method = method,
        pip = TRUE,
        pip_options = pip_options
      )
      TRUE
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      FALSE
    })

    if (ok) return(invisible(TRUE))

    if (attempt < retries) {
      message("Install attempt ", attempt, " failed; retrying...")
      Sys.sleep(2)
    }
    attempt <- attempt + 1L
  }

  stop(last_error, call. = FALSE)
}

#' Verify module imports using reticulate's active Python
#' @keywords internal
.verify_python_imports <- function(modules = .required_python_modules()) {
  if (!tryCatch(reticulate::py_available(initialize = TRUE),
                error = function(e) FALSE)) {
    return(list(ok = FALSE,
                failed = modules,
                details = setNames(lapply(modules, function(x) {
                  list(status = "error",
                       error = "Python could not be initialized in reticulate")
                }), modules)))
  }

  failed <- character()
  details <- list()

  for (mod in modules) {
    available <- tryCatch(reticulate::py_module_available(mod),
                          error = function(e) FALSE)
    if (available) {
      version <- tryCatch({
        module <- reticulate::import(mod, delay_load = FALSE)
        if (!is.null(module$`__version__`)) as.character(module$`__version__`) else "unknown"
      }, error = function(e) "unknown")
      details[[mod]] <- list(status = "ok", version = version)
    } else {
      failed <- c(failed, mod)
      msg <- tryCatch(
        {
          reticulate::import(mod, delay_load = FALSE)
          "Import failed"
        },
        error = function(e) conditionMessage(e)
      )
      details[[mod]] <- list(status = "error", error = msg)
    }
  }

  list(ok = length(failed) == 0, failed = failed, details = details)
}

# =============================================================================
# Python Setup (user-facing)
# =============================================================================

#' Set up Python dependencies for epiworldRcalibrate
#'
#' This function creates a dedicated conda environment (managed by
#' \'reticulate\') and installs all
#' required Python packages (numpy, scikit-learn, joblib, PyTorch). Run this
#' once after installing the package. This is kept separate from model
#' functions so that package installation never happens automatically during
#' normal use.
#'
#' @param force Logical; if \code{TRUE}, removes and recreates the virtual
#'   environment from scratch. Default is \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} on success.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Ensures reticulate-managed Miniconda is available.
#'   \item Creates a conda environment named \code{"epiworldRcalibrate"}.
#'   \item Installs \code{numpy}, \code{scikit-learn}, \code{joblib}, and
#'     \code{torch} (CPU version) into that conda environment.
#'   \item Verifies all packages can be imported.
#' }
#'
#' @examples
#' \dontrun{
#' # First-time setup (run once after installing the package)
#' setup_python_deps()
#'
#' # Force reinstall if something went wrong
#' setup_python_deps(force = TRUE)
#' }
#'
#' @export
setup_python_deps <- function(force = FALSE) {

  envname <- .bilstm_env$venv_name

  .ensure_reticulate_conda()

  # ---- Optionally remove existing environment ----
  if (force) {
    message("Removing existing conda environment '", envname, "'...")
    envs <- .get_conda_env_names()
    if (envname %in% envs) {
      tryCatch(
        reticulate::conda_remove(envname = envname, conda = "auto"),
        error = function(e) {
          stop(
            "Could not remove conda environment '", envname, "': ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
    }
    # Reset internal state
    .bilstm_env$model_loaded <- FALSE
    .bilstm_env$model_dir <- NULL
  }

  # ---- Create conda environment if needed ----
  envs <- .get_conda_env_names()
  if (!(envname %in% envs)) {
    message("Creating conda environment '", envname, "'...")
    tryCatch({
      reticulate::conda_create(
        envname = envname,
        packages = c("python=3.11", "pip"),
        conda = "auto"
      )
    }, error = function(e) {
      stop(
        "Failed to create conda environment '", envname, "': ",
        conditionMessage(e),
        call. = FALSE
      )
    })
  } else {
    message("Conda environment '", envname, "' already exists.")
  }

  health <- .check_conda_env_health(envname)
  if (!health$ok) {
    stop(
      "Conda environment '", envname, "' is not healthy: ", health$message,
      "\nTry: setup_python_deps(force = TRUE)",
      call. = FALSE
    )
  }

  # Bind reticulate to the package conda env for this session
  tryCatch({
    reticulate::use_condaenv(envname, required = TRUE)
  }, error = function(e) {
    stop(
      "Could not activate conda environment '", envname, "': ",
      conditionMessage(e), "\n",
      "If Python was already initialized with another interpreter, restart R and run setup_python_deps() again.",
      call. = FALSE
    )
  })

  # ---- Define required packages ----
  required_packages <- list(
    list(check_name = "numpy",   install_name = "numpy"),
    list(check_name = "sklearn", install_name = "scikit-learn"),
    list(check_name = "joblib",  install_name = "joblib"),
    list(check_name = "torch",   install_name = "torch")
  )

  # ---- Check what is already installed ----
  module_check <- .verify_python_imports(
    modules = vapply(required_packages, function(x) x$check_name, character(1))
  )

  needs_install <- character()
  for (pkg in required_packages) {
    available <- identical(module_check$details[[pkg$check_name]]$status, "ok")

    if (!available) {
      message("Package '", pkg$check_name, "' not found, will install.")
      needs_install <- c(needs_install, pkg$install_name)
    } else {
      message("Package '", pkg$check_name, "' already installed.")
    }
  }

  # ---- Install missing packages ----
  if (length(needs_install) > 0) {
    # Install non-torch packages first
    non_torch <- setdiff(needs_install, "torch")
    if (length(non_torch) > 0) {
      message("Installing: ", paste(non_torch, collapse = ", "), " ...")
      tryCatch({
        .install_python_packages(non_torch,
                                 envname = envname,
                                 method = "conda",
                                 retries = 2)
      }, error = function(e) {
        stop("Failed to install packages ",
             paste(non_torch, collapse = ", "), ": ",
             conditionMessage(e), call. = FALSE)
      })
    }

    # Install PyTorch with CPU-only version
    if ("torch" %in% needs_install) {
      message("Installing PyTorch (CPU version)...")
      tryCatch({
        .install_python_packages(
          "torch",
          envname = envname,
          method = "conda",
          pip_options = c("--index-url",
                          "https://download.pytorch.org/whl/cpu"),
          retries = 2
        )
      }, error = function(e) {
        stop("Failed to install PyTorch: ", conditionMessage(e),
             call. = FALSE)
      })
    }
  }

  # ---- Verify all packages can be imported ----
  message("Verifying installation...")
  verification <- .verify_python_imports()

  for (pkg_name in names(verification$details)) {
    pkg_result <- verification$details[[pkg_name]]
    if (identical(pkg_result$status, "ok")) {
      message("[OK]   ", pkg_name, " v", pkg_result$version)
    } else {
      message("[FAIL] ", pkg_name, ": ", pkg_result$error)
    }
  }

  if (!verification$ok) {
    stop(
      "Installation succeeded but the following packages failed to import: ",
      paste(verification$failed, collapse = ", "), ".\n",
      "Try: setup_python_deps(force = TRUE)",
      call. = FALSE
    )
  }

  message("\nPython setup complete! You can now use the package normally.")
  message("Example:")
  message("  epiworldRcalibrate::init_bilstm_model()")
  invisible(TRUE)
}

# =============================================================================
# Core API
# =============================================================================

#' Initialize BiLSTM Model for SIR Parameter Estimation
#'
#' Loads the trained BiLSTM model and associated scaler objects into memory.
#' Requires that Python dependencies have been set up via
#' \code{\link{setup_python_deps}}.
#'
#' @param model_dir Optional path to model directory. Defaults to the
#'   package's bundled model files.
#' @param force_reload Logical; reload even if already loaded.
#'
#' @return Invisibly returns \code{TRUE} on success.
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

  # Load with optional scaler_inc
  scaler_inc_path <- if (file.exists(file_paths$scaler_inc)) {
    file_paths$scaler_inc
  } else {
    NULL
  }

  reticulate::py$load_model_components(
    model_path      = file_paths$model,
    scaler_add_path = file_paths$scaler_add,
    scaler_tgt_path = file_paths$scaler_tgt,
    scaler_inc_path = scaler_inc_path
  )

  .bilstm_env$model_loaded <- TRUE
  .bilstm_env$model_dir    <- model_dir
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
#' @return Named numeric vector: \code{ptran}, \code{crate}, \code{R0}.
#' @export
estimate_sir_parameters <- function(daily_cases, population_size,
                                    recovery_rate) {
  if (!.bilstm_env$model_loaded)
    stop("Model not loaded. Call init_bilstm_model() first.")

  .validate_sir_inputs(daily_cases, population_size, recovery_rate)

  out <- reticulate::py$predict_sir_parameters(
    as.numeric(daily_cases),
    list(as.numeric(population_size), as.numeric(recovery_rate))
  )
  names(out) <- c("ptran", "crate", "R0")

  # Recalculate crate based on R0, recovery_rate, and ptran
  out["crate"] <- out["R0"] * recovery_rate / out["ptran"]

  out
}

#' Calibrate SIR Parameters (one-step convenience wrapper)
#'
#' Optionally initializes the BiLSTM model and then calls
#' \code{\link{estimate_sir_parameters}} on the provided data.
#'
#' @param daily_cases Numeric vector of length 61 containing daily incidence
#'   counts (day 0 to day 60).
#' @param population_size Single numeric value giving the total population size.
#' @param recovery_rate Single numeric value giving the recovery rate parameter.
#' @param model_dir Optional path to the directory containing the trained
#'   BiLSTM model and scaler files. If \code{NULL}, the package's bundled
#'   assets are used.
#' @param auto_init Logical; if \code{TRUE} (default), automatically calls
#'   \code{\link{init_bilstm_model}} when the model is not yet loaded.
#'
#' @return Named numeric vector: \code{ptran}, \code{crate}, \code{R0}.
#' @export
calibrate_sir <- function(daily_cases,
                          population_size,
                          recovery_rate,
                          model_dir  = NULL,
                          auto_init  = TRUE) {

  if (!.bilstm_env$model_loaded && auto_init)
    init_bilstm_model(model_dir)

  estimate_sir_parameters(daily_cases, population_size, recovery_rate)
}

# =============================================================================
# Utilities
# =============================================================================

#' Check model status
#'
#' @return A list describing model load status and Python config.
#' @export
check_model_status <- function() {
  list(
    loaded          = .bilstm_env$model_loaded,
    model_directory = .bilstm_env$model_dir,
    python_config   = tryCatch(
      utils::capture.output(reticulate::py_config()),
      error = function(e) NULL
    )
  )
}

#' Check Python and package installation status
#'
#' Reports whether Python is available, which package-managed conda
#' environment is in use,
#' and whether each required Python package is importable.
#'
#' @return A list with Python runtime details and package status.
#' @export
check_python_setup <- function() {
  resolved <- .resolve_python_target()
  result <- c(resolved, list(packages = list()))

  if (!result$python_available) {
    message("Python is not available.")
    message("Run epiworldRcalibrate::setup_python_deps() to set up.")
    return(result)
  }

  # Check packages in selected python
  packages <- .required_python_modules()
  verification <- .verify_python_imports(modules = packages)
  for (pkg in names(verification$details)) {
    pkg_result <- verification$details[[pkg]]
    result$packages[[pkg]] <- list(
      available = identical(pkg_result$status, "ok")
    )
    if (identical(pkg_result$status, "ok")) {
      result$packages[[pkg]]$version <- pkg_result$version
    } else {
      result$packages[[pkg]]$error <- pkg_result$error
    }
  }

  # Summarise for the user
  missing <- packages[!vapply(result$packages,
                              function(p) p$available, logical(1))]
  if (length(missing) > 0) {
    message("Missing packages: ", paste(missing, collapse = ", "))
    message("Run epiworldRcalibrate::setup_python_deps() to install them.")
  } else {
    message("All required Python packages are installed.")
  }

  result
}

#' Unload the model from memory
#'
#' @return Invisibly \code{TRUE} on success.
#' @export
cleanup_model <- function() {
  if (!.bilstm_env$model_loaded) {
    message("No model loaded.")
    return(invisible(TRUE))
  }

  try(reticulate::py$cleanup_model(), silent = TRUE)
  .bilstm_env$model_loaded <- FALSE
  .bilstm_env$model_dir    <- NULL
  message("Model cleaned up.")
  invisible(TRUE)
}

# =============================================================================
# .onAttach — friendly first-use guidance
# =============================================================================

#' @keywords internal
.onAttach <- function(libname, pkgname) {

  # Check if the package conda environment exists
  envname <- .bilstm_env$venv_name
  envs <- if (.has_reticulate_conda()) .get_conda_env_names() else character()

  if (!(envname %in% envs)) {
    packageStartupMessage(
      "epiworldRcalibrate: Python dependencies are not set up yet.\n",
      "Run this once to install them:\n\n",
      "  epiworldRcalibrate::setup_python_deps()\n"
    )
  }
}
