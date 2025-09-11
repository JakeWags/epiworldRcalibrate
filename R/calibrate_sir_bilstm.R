#' @keywords internal
"_PACKAGE"

library(reticulate)

# Global environment tracking
.bilstm_env <- new.env()
.bilstm_env$model_loaded <- FALSE
.bilstm_env$model_dir <- NULL

#' Initialize BiLSTM Model for SIR Parameter Estimation
#'
#' @param model_dir Character. Path to directory containing model files.
#'                  If NULL, uses default model directory
#' @param force_reload Logical. Force reload even if model already loaded (default: FALSE)
#'
#' @details
#' This function loads the pre-trained BiLSTM model and scalers into memory.
#' Must be called before using \code{\link{estimate_sir_parameters}}.
#'
#' Required files in \code{model_dir}:
#' \itemize{
#'   \item model4_bilstm_relchange_no_eps.pt - PyTorch model weights
#'   \item scaler_additional_no_eps.pkl - Scaler for population size and recovery rate
#'   \item scaler_targets_no_eps.pkl - Scaler for transmission rate, contact rate, and R0
#' }
#'
#' @return Logical. TRUE if model loaded successfully
#' @export
#'
#' @examples
#' \dontrun{
#' # Initialize model (do this once per R session)
#' init_bilstm_model()
#'
#' # Or specify custom model directory
#' init_bilstm_model("/path/to/your/model/files")
#' }
init_bilstm_model <- function(model_dir = NULL, force_reload = FALSE) {

  if (is.null(model_dir)) {
    # Try package installation directory first
    model_dir <- system.file("models", package = "epiworldRcalibrate")
    if (model_dir == "") {
      # Fallback to development directory
      model_dir <- "~/Desktop/epiworldRcalibrate_fixed/epiworldRcalibrate/inst/models"
    }
  }

  # Check if already loaded
  if (.bilstm_env$model_loaded && !force_reload && identical(.bilstm_env$model_dir, model_dir)) {
    message("BiLSTM model already loaded. Use force_reload=TRUE to reload.")
    return(TRUE)
  }

  # Validate model directory
  if (!dir.exists(model_dir)) {
    stop(paste("Model directory does not exist:", model_dir))
  }

  # Define required file paths
  base_dir <- normalizePath(model_dir)
  model_path <- file.path(base_dir, "model4_bilstm_relchange_no_eps.pt")
  scaler_add_path <- file.path(base_dir, "scaler_additional_no_eps.pkl")
  scaler_tgt_path <- file.path(base_dir, "scaler_targets_no_eps.pkl")

  # Check all required files exist
  required_files <- c(model_path, scaler_add_path, scaler_tgt_path)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    stop(paste("Required model files not found:", paste(basename(missing_files), collapse = ", ")))
  }

  # Python code for model loading and preprocessing
  python_code <- '
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

    This preprocessing step transforms raw case counts into relative changes:
    - Day 0: Always 0 (reference point)
    - Day i: (count[i] - count[i-1]) / count[i-1] if count[i-1] > 0
    - Day i: 0 if count[i-1] == 0 (handles division by zero)

    Args:
        raw_counts: Array of raw daily case counts (length 61)

    Returns:
        Array of percentage changes ready for model input (length 61)
    """
    raw_counts = np.asarray(raw_counts, dtype=float)

    # Initialize output array
    percentage_changes = np.zeros_like(raw_counts)

    # Calculate percentage changes for each day (starting from day 1)
    percentage_changes[1:] = np.where(
        raw_counts[:-1] != 0,  # If previous day had cases
        (raw_counts[1:] - raw_counts[:-1]) / raw_counts[:-1],  # Calculate % change
        0  # If previous day had 0 cases, set change to 0
    )

    return percentage_changes

def load_model_components(model_path, scaler_add_path, scaler_tgt_path):
    """Load the trained model and scalers"""
    global _model, _scaler_additional, _scaler_targets

    try:
        # Load scalers for data normalization
        _scaler_additional = joblib.load(scaler_add_path)  # For n, recov
        _scaler_targets = joblib.load(scaler_tgt_path)     # For ptran, crate, R0

        # Create and load the trained model
        _model = BiLSTMModel(
            input_dim=1,        # Single feature (percentage change)
            hidden_dim=160,     # Hidden layer size
            num_layers=3,       # Number of LSTM layers
            additional_dim=2,   # Additional inputs (n, recov)
            output_dim=3,       # Outputs (ptran, crate, R0)
            dropout=0.5         # Dropout rate
        )

        # Load trained weights
        state_dict = torch.load(model_path, map_location=_device)
        _model.load_state_dict(state_dict)
        _model.to(_device)
        _model.eval()  # Set to evaluation mode

        return True

    except Exception as e:
        raise RuntimeError(f"Failed to load model components: {str(e)}")

def predict_sir_parameters(raw_incidence_counts, population_size, recovery_rate):
    """
    Predict SIR parameters from raw incidence data

    Args:
        raw_incidence_counts: List of 61 daily case counts (day 0 to day 60)
        population_size: Total population size
        recovery_rate: Recovery rate (1/infectious_period)

    Returns:
        List of [transmission_probability, contact_rate, R0]
    """
    global _model, _scaler_additional, _scaler_targets

    if _model is None:
        raise RuntimeError("Model not loaded. Call load_model_components() first.")

    # Validate input data
    raw_incidence_counts = np.asarray(raw_incidence_counts, dtype=float)
    if len(raw_incidence_counts) != 61:
        raise ValueError(f"Expected 61 daily counts (day 0 to day 60), got {len(raw_incidence_counts)}")

    # Step 1: Preprocess the raw incidence data
    processed_incidence = preprocess_incidence_data(raw_incidence_counts)

    # Step 2: Reshape for model input (batch_size=1, sequence_length=61, features=1)
    X_tensor = torch.tensor(
        processed_incidence.reshape(1, -1, 1),
        dtype=torch.float32,
        device=_device
    )

    # Step 3: Prepare and scale additional inputs (population size, recovery rate)
    additional_inputs = np.array([[population_size, recovery_rate]], dtype=float)
    additional_inputs_scaled = _scaler_additional.transform(additional_inputs)
    additional_tensor = torch.tensor(
        additional_inputs_scaled,
        dtype=torch.float32,
        device=_device
    )

    # Step 4: Make prediction
    with torch.no_grad():
        # Get scaled predictions from model
        predictions_scaled = _model(X_tensor, additional_tensor).cpu().numpy()

        # Convert back to original scale
        predictions_original = _scaler_targets.inverse_transform(predictions_scaled)

    # Return as list: [ptran, crate, R0]
    return predictions_original[0].tolist()

def cleanup_model():
    """Clean up model and scalers from memory"""
    global _model, _scaler_additional, _scaler_targets
    _model = None
    _scaler_additional = None
    _scaler_targets = None
    return True

def show_preprocessing_example(raw_counts):
    """Show how raw counts are preprocessed (for debugging/understanding)"""
    processed = preprocess_incidence_data(raw_counts)
    return processed.tolist()
'

  # Initialize Python environment
  tryCatch({
    py_run_string(python_code)
    message("Python environment initialized")
  }, error = function(e) {
    stop(paste("Failed to initialize Python environment:", e$message))
  })

  # Load model and scalers
  tryCatch({
    py$load_model_components(
      model_path = normalizePath(model_path),
      scaler_add_path = normalizePath(scaler_add_path),
      scaler_tgt_path = normalizePath(scaler_tgt_path)
    )

    .bilstm_env$model_loaded <- TRUE
    .bilstm_env$model_dir <- model_dir
    message("BiLSTM model loaded successfully!")
    message("Ready to estimate SIR parameters")
    return(TRUE)

  }, error = function(e) {
    .bilstm_env$model_loaded <- FALSE
    stop(paste("Failed to load model:", e$message))
  })
}

#' Estimate SIR Parameters from Daily Incidence Data
#'
#' @param daily_cases Numeric vector of exactly 61 consecutive daily case counts (day 0 to day 60)
#' @param population_size Numeric. Total population size for the outbreak
#' @param recovery_rate Numeric. Recovery rate (1/average_infectious_period_in_days)
#'
#' @details
#' This function estimates SIR model parameters from daily incidence data using a
#' pre-trained BiLSTM neural network.
#'
#' **Data Flow:**
#' 1. Takes your raw daily case counts for 61 days (e.g., [5, 8, 12, 15, 23, ...])
#' 2. Automatically converts to percentage changes (e.g., [0, 0.6, 0.5, 0.25, 0.53, ...])
#' 3. Feeds processed data through the trained neural network
#' 4. Returns calibrated SIR parameters
#'
#' **Requirements:**
#' - Exactly 61 consecutive days of case count data (day 0 through day 60)
#' - Case counts should be non-negative integers
#' - Must call \code{\link{init_bilstm_model}} first
#'
#' @return Named numeric vector with three components:
#' \describe{
#'   \item{ptran}{Transmission probability per contact}
#'   \item{crate}{Contact rate (contacts per person per day)}
#'   \item{R0}{Basic reproduction number}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Step 1: Initialize the model (once per R session)
#' init_bilstm_model()
#'
#' # Step 2: Prepare your outbreak data (61 consecutive days: day 0 to day 60)
#' my_outbreak_data <- c(
#'   # Day 0-6: Initial cases
#'   2, 3, 5, 8, 12, 18, 25,
#'   # Day 7-20: Early growth phase
#'   35, 48, 65, 85, 108, 135, 165, 198, 235, 275, 318, 362, 405, 448,
#'   # Day 21-34: Peak period
#'   485, 518, 545, 565, 580, 590, 595, 595, 590, 580, 565, 545, 520, 490,
#'   # Day 35-48: Decline phase
#'   455, 415, 370, 325, 280, 238, 200, 165, 135, 108, 85, 65, 48, 35,
#'   # Day 49-55: Late decline
#'   25, 18, 12, 8, 5, 3, 2,
#'   # Day 56-60: Final phase
#'   1, 1, 0, 1, 0
#' )
#'
#' # Verify we have exactly 61 days
#' length(my_outbreak_data)  # Should be 61
#'
#' # Step 3: Estimate SIR parameters
#' sir_params <- estimate_sir_parameters(
#'   daily_cases = my_outbreak_data,
#'   population_size = 10000,
#'   recovery_rate = 1/7  # 7-day average infectious period
#' )
#'
#' # Step 4: View results
#' print(sir_params)
#' # Example output:
#' #   ptran    crate       R0
#' # 0.0234   12.456    2.134
#'
#' # Access individual parameters
#' transmission_prob <- sir_params["ptran"]
#' contact_rate <- sir_params["crate"]
#' basic_reproduction_number <- sir_params["R0"]
#' }
estimate_sir_parameters <- function(daily_cases, population_size, recovery_rate) {

  # Check if model is loaded
  if (!.bilstm_env$model_loaded) {
    stop("BiLSTM model not loaded. Please call init_bilstm_model() first.")
  }

  # Validate inputs
  if (!is.numeric(daily_cases) || length(daily_cases) != 61) {
    stop("daily_cases must be a numeric vector of exactly 61 values (day 0 to day 60)")
  }

  if (!is.numeric(population_size) || length(population_size) != 1 || population_size <= 0) {
    stop("population_size must be a positive number")
  }

  if (!is.numeric(recovery_rate) || length(recovery_rate) != 1 || recovery_rate <= 0) {
    stop("recovery_rate must be a positive number")
  }

  if (any(daily_cases < 0)) {
    stop("daily_cases cannot contain negative values")
  }

  # Make prediction
  tryCatch({
    # Convert inputs to appropriate types
    daily_cases <- as.numeric(daily_cases)
    population_size <- as.numeric(population_size)
    recovery_rate <- as.numeric(recovery_rate)

    # Call Python prediction function
    result <- py$predict_sir_parameters(daily_cases, population_size, recovery_rate)

    # Name the results
    names(result) <- c("ptran", "crate", "R0")

    return(result)

  }, error = function(e) {
    stop(paste("Parameter estimation failed:", e$message))
  })
}

#' Quick SIR Parameter Estimation (One-Step Function)
#'
#' @param daily_cases Numeric vector of exactly 61 consecutive daily case counts (day 0 to day 60)
#' @param population_size Numeric. Total population size
#' @param recovery_rate Numeric. Recovery rate (1/infectious_period_days)
#' @param model_dir Character. Optional path to model directory
#' @param auto_init Logical. Automatically initialize model if not loaded (default: TRUE)
#'
#' @details
#' Convenience function that combines model initialization and parameter estimation.
#' Perfect for one-off analyses where you don't want to manually call \code{\link{init_bilstm_model}}.
#'
#' @return Named numeric vector with SIR parameters: ptran, crate, R0
#' @export
#'
#' @examples
#' \dontrun{
#' # One-line parameter estimation (automatically loads model)
#' # Create 61 days of outbreak data (day 0 to day 60)
#' outbreak_data <- c(
#'   1, 2, 4, 7, 12, 20, 32, 48, 67, 89, 115, 142, 168, 192, 215,
#'   235, 250, 260, 265, 265, 260, 250, 235, 215, 192, 168, 142, 115,
#'   89, 67, 48, 32, 20, 12, 7, 4, 2, 1, 1, 0, 0, 1, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#' )
#'
#' # Verify length
#' length(outbreak_data)  # Should be 61
#'
#' params <- quick_estimate_sir(outbreak_data, population_size = 5000, recovery_rate = 1/5)
#' print(params)
#' }
quick_estimate_sir <- function(daily_cases, population_size, recovery_rate,
                               model_dir = NULL, auto_init = TRUE) {

  if (!.bilstm_env$model_loaded && auto_init) {
    message("Model not loaded. Initializing automatically...")
    init_bilstm_model(model_dir = model_dir)
  }

  return(estimate_sir_parameters(daily_cases, population_size, recovery_rate))
}

#' Show Data Preprocessing Example
#'
#' @param raw_counts Numeric vector of raw daily case counts (any length for demonstration)
#'
#' @details
#' This function shows exactly how your raw case count data is transformed
#' before being fed to the neural network. Useful for understanding the
#' preprocessing step and debugging unexpected results.
#'
#' The preprocessing converts raw counts to percentage changes:
#' - Day 0: 0 (reference point)
#' - Day i: (count[i] - count[i-1]) / count[i-1] if count[i-1] > 0
#' - Day i: 0 if count[i-1] = 0
#'
#' **Note:** For actual predictions, you need exactly 61 days (day 0 to day 60).
#'
#' @return Data frame showing the transformation
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with simple outbreak curve
#' example_counts <- c(1, 3, 8, 15, 25, 35, 40, 38, 30, 20, 12, 6, 2, 1, 0)
#' show_preprocessing(example_counts)
#'
#' # Test with the exact format your model expects (61 days)
#' full_outbreak <- c(rep(c(1, 2, 5, 8, 12), each = 12), 1)  # Creates 61 values
#' show_preprocessing(full_outbreak)
#' }
show_preprocessing <- function(raw_counts) {

  python_code_temp <- '
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
    py_run_string(python_code_temp)
    processed <- py$show_preprocessing_temp(as.numeric(raw_counts))

    # Create informative data frame
    result <- data.frame(
      day = 0:(length(raw_counts)-1),  # Day 0 to day N-1
      raw_count = raw_counts,
      percentage_change = round(processed, 4),
      stringsAsFactors = FALSE
    )

    # Add helpful message about expected length
    if (length(raw_counts) != 61) {
      message(paste("Note: For model predictions, you need exactly 61 days.",
                    "Currently showing", length(raw_counts), "days."))
    }

    return(result)

  }, error = function(e) {
    stop(paste("Preprocessing demonstration failed:", e$message))
  })
}

#' Check Model Status
#'
#' @return List with model loading status and directory
#' @export
check_model_status <- function() {
  list(
    loaded = .bilstm_env$model_loaded,
    model_directory = .bilstm_env$model_dir
  )
}

#' Clean Up Model from Memory
#'
#' @details
#' Removes the loaded model and scalers from memory. Useful for freeing up
#' resources in long-running R sessions.
#'
#' @return None (invisibly returns TRUE if successful)
#' @export
cleanup_model <- function() {
  if (.bilstm_env$model_loaded) {
    tryCatch({
      py$cleanup_model()
      .bilstm_env$model_loaded <- FALSE
      .bilstm_env$model_dir <- NULL
      message("Model cleaned up successfully")
      invisible(TRUE)
    }, error = function(e) {
      warning(paste("Error during cleanup:", e$message))
      invisible(FALSE)
    })
  } else {
    message("No model currently loaded")
    invisible(TRUE)
  }
}
