# data-raw/abc_calibration_results.R
# Generate ABC calibration results for package data

suppressPackageStartupMessages({
  library(epiworldR)
  library(tictoc)
})

# Load packaged data
data("utah_covid_data", package = "epiworldRcalibrate")

# Configuration
n_days <- 61
N <- 30000
recov <- 1/7
model_ndays <- 60
model_seed <- 122

# Get COVID data
get_covid_data <- function(n_days) {
  covid_data <- utah_covid_data
  last_date <- max(covid_data$Date, na.rm = TRUE)
  covid_data <- subset(covid_data, Date > (last_date - n_days))
  covid_data <- covid_data[order(covid_data$Date), ]
  return(covid_data)
}

covid_data <- get_covid_data(n_days)
incidence_vec <- covid_data$Daily.Cases

cat("=== ABC Calibration for Package Data ===\n")
cat("Date range:", as.character(min(covid_data$Date)), "to",
    as.character(max(covid_data$Date)), "\n")
cat("Total cases:", sum(incidence_vec), "\n")
cat("Mean daily cases:", round(mean(incidence_vec), 1), "\n\n")

# =============================================================================
# ABC Helper Functions
# =============================================================================

simulate_epidemic_calib <- function(params, ndays = model_ndays, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  sim_model <- ModelSIRCONN(
    name = "sim",
    n = as.integer(params[1]),
    prevalence = as.numeric(params[2]),
    contact_rate = as.numeric(params[3]),
    transmission_rate = as.numeric(params[5]),
    recovery_rate = as.numeric(params[4])
  )

  verbose_off(sim_model)
  run(sim_model, ndays = ndays)

  counts <- get_hist_total(sim_model)
  infected_counts <- counts[counts$state == "Infected", "counts"]

  return(infected_counts)
}

simulation_fun <- function(params, lfmcmc_obj, observed_data_info) {
  sim_params <- c(
    observed_data_info$n,
    observed_data_info$preval,
    params[1],  # contact_rate
    params[2],  # recovery_rate
    params[3]   # transmission_prob
  )

  infected_counts <- simulate_epidemic_calib(sim_params, ndays = model_ndays)
  return(as.numeric(infected_counts))
}

summary_fun <- function(data, lfmcmc_obj) {
  return(as.numeric(data))
}

proposal_fun <- function(old_params, lfmcmc_obj) {
  new_crate <- old_params[1] * exp(rnorm(1, sd = 0.1))
  new_recov <- plogis(qlogis(old_params[2]) + rnorm(1, sd = 0.1))
  new_ptran <- plogis(qlogis(old_params[3]) + rnorm(1, sd = 0.1))

  return(c(new_crate, new_recov, new_ptran))
}

kernel_fun <- function(simulated_stat, observed_stat, epsilon, lfmcmc_obj) {
  diff <- sum((simulated_stat - observed_stat)^2)
  return(exp(-diff / (2 * epsilon^2)))
}

# =============================================================================
# Run ABC Calibration
# =============================================================================

cat("=== Running ABC Calibration ===\n")

initial_infected <- incidence_vec[1]
initial_prevalence <- initial_infected / N

observed_data_info <- list(
  n = N,
  preval = initial_prevalence
)

dummy_model <- ModelSIRCONN(
  name = "dummy",
  n = as.integer(N),
  prevalence = 0.1,
  contact_rate = 5.0,
  transmission_rate = 0.1,
  recovery_rate = 0.1
)

local_simulation_fun <- function(params, lfmcmc_obj) {
  return(simulation_fun(params, lfmcmc_obj, observed_data_info))
}

lfmcmc_obj <- LFMCMC(dummy_model)
lfmcmc_obj <- set_simulation_fun(lfmcmc_obj, local_simulation_fun)
lfmcmc_obj <- set_summary_fun(lfmcmc_obj, summary_fun)
lfmcmc_obj <- set_proposal_fun(lfmcmc_obj, proposal_fun)
lfmcmc_obj <- set_kernel_fun(lfmcmc_obj, kernel_fun)
lfmcmc_obj <- set_observed_data(lfmcmc_obj, incidence_vec)

# Parameters: 3000 samples with 1500 burn-in
init_params <- c(5, 1/7, 0.0571)
n_samples_calib <- 3000
burnin <- 1500
epsilon <- sqrt(sum(incidence_vec^2)) * 0.05

cat("LFMCMC Configuration:\n")
cat("  Samples:", n_samples_calib, "(50% burn-in)\n")
cat("  Epsilon:", round(epsilon, 2), "\n")
cat("  Seed:", model_seed, "\n\n")

tic("ABC calibration")
run_lfmcmc(
  lfmcmc = lfmcmc_obj,
  params_init = init_params,
  n_samples = n_samples_calib,
  epsilon = epsilon,
  seed = model_seed
)
abc_time <- toc()

# =============================================================================
# Extract and Summarize Results
# =============================================================================

# Get all accepted parameters
accepted <- get_all_accepted_params(lfmcmc_obj)
post_burnin <- tail(accepted, n = (n_samples_calib - burnin))

# Calculate summary statistics
abc_median <- apply(post_burnin, 2, median)
abc_mean <- apply(post_burnin, 2, mean)
abc_lower <- apply(post_burnin, 2, quantile, probs = 0.025)
abc_upper <- apply(post_burnin, 2, quantile, probs = 0.975)
abc_sd <- apply(post_burnin, 2, sd)

# Extract individual parameters
abc_crate <- abc_median[1]
abc_recov <- abc_median[2]
abc_ptran <- abc_median[3]
abc_R0 <- (abc_crate * abc_ptran) / abc_recov

# Calculate R0 statistics
R0_samples <- (post_burnin[, 1] * post_burnin[, 3]) / post_burnin[, 2]
abc_R0_mean <- mean(R0_samples)
abc_R0_lower <- quantile(R0_samples, 0.025)
abc_R0_upper <- quantile(R0_samples, 0.975)
abc_R0_sd <- sd(R0_samples)

cat("\n=== Calibration Results ===\n")
cat("Elapsed time:", round((abc_time$toc - abc_time$tic) / 60, 2), "minutes\n\n")

cat("Calibrated Parameters (Median [95% CI]):\n")
cat("Contact Rate:      ", sprintf("%.4f", abc_crate),
    " [", sprintf("%.4f", abc_lower[1]), ", ", sprintf("%.4f", abc_upper[1]), "]\n", sep = "")
cat("Recovery Rate:     ", sprintf("%.4f", abc_recov),
    " [", sprintf("%.4f", abc_lower[2]), ", ", sprintf("%.4f", abc_upper[2]), "]\n", sep = "")
cat("Transmission Prob: ", sprintf("%.4f", abc_ptran),
    " [", sprintf("%.4f", abc_lower[3]), ", ", sprintf("%.4f", abc_upper[3]), "]\n", sep = "")
cat("R0:                ", sprintf("%.4f", abc_R0),
    " [", sprintf("%.4f", abc_R0_lower), ", ", sprintf("%.4f", abc_R0_upper), "]\n\n", sep = "")

# =============================================================================
# Create Package Data Object
# =============================================================================

abc_calibration_results <- list(
  # Point estimates (median)
  abc_crate = abc_crate,
  abc_recov = abc_recov,
  abc_ptran = abc_ptran,
  abc_R0 = abc_R0,

  # Summary statistics
  abc_median = abc_median,
  abc_mean = abc_mean,
  abc_lower = abc_lower,
  abc_upper = abc_upper,
  abc_sd = abc_sd,

  # R0 statistics
  abc_R0_mean = abc_R0_mean,
  abc_R0_lower = abc_R0_lower,
  abc_R0_upper = abc_R0_upper,
  abc_R0_sd = abc_R0_sd,

  # Posterior samples (for uncertainty quantification)
  post_burnin = post_burnin,
  R0_samples = R0_samples,

  # Metadata
  metadata = list(
    calibration_date = Sys.Date(),
    data_source = "utah_covid_data",
    date_range = c(
      start = as.character(min(covid_data$Date)),
      end = as.character(max(covid_data$Date))
    ),
    n_days = n_days,
    population_size = N,
    recovery_rate_fixed = recov,
    initial_prevalence = initial_prevalence,
    total_cases = sum(incidence_vec),
    mean_daily_cases = mean(incidence_vec)
  ),

  # MCMC configuration
  config = list(
    n_samples = n_samples_calib,
    burnin = burnin,
    epsilon = epsilon,
    seed = model_seed,
    init_params = init_params,
    model_ndays = model_ndays
  ),

  # Timing
  timing = list(
    elapsed_seconds = abc_time$toc - abc_time$tic,
    elapsed_minutes = (abc_time$toc - abc_time$tic) / 60
  )
)

# =============================================================================
# Save Package Data
# =============================================================================

usethis::use_data(abc_calibration_results, overwrite = TRUE)

cat("=== Data Saved ===\n")
cat("File: data/abc_calibration_results.rda\n")
cat("Objects included:\n")
cat("  - Point estimates (abc_crate, abc_recov, abc_ptran, abc_R0)\n")
cat("  - Summary statistics (median, mean, CI, SD)\n")
cat("  - Posterior samples (", nrow(post_burnin), " samples)\n", sep = "")
cat("  - Metadata (dates, population, configuration)\n")
cat("  - Timing information\n\n")

cat("Total computation time:", round((abc_time$toc - abc_time$tic) / 60, 2), "minutes\n")
cat("\n=== Script Complete ===\n")
