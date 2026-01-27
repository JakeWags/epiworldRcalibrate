# data-raw/process_covid_calibration.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(epiworldR)
  library(epiworldRcalibrate)
  library(tictoc)
})

# Load COVID data ----------------------------------------------------------
get_covid_data <- function(n_days) {
  # Use packaged data
  covid_data <- utah_covid_data

  # Filter to last n_days
  last_date <- max(covid_data$Date, na.rm = TRUE)
  covid_data <- subset(covid_data, Date > (last_date - n_days)) %>%
    dplyr::arrange(Date)

  stopifnot("Daily.Cases" %in% names(covid_data))
  covid_data
}

n_days <- 61
covid_data <- get_covid_data(n_days)
incidence <- covid_data$Daily.Cases
incidence_vec <- incidence
stopifnot(length(incidence) == n_days)

cat("Date range:", as.character(min(covid_data$Date)), "to",
    as.character(max(covid_data$Date)), "\n")
cat("Total cases observed:", sum(incidence), "\n")
cat("Mean daily cases:", round(mean(incidence), 1), "\n")

# Configuration ------------------------------------------------------------
N <- 20000     # Population size (BiLSTM)
recov <- 1 / 7  # Recovery rate (7-day infectious period)
model_ndays <- 60  # Simulation days
model_seed <- 122

initial_infected <- incidence_vec[1]
initial_prevalence <- initial_infected / N

# Define model ONCE outside simulation ------------------------------------
model_sir <- ModelSIRCONN(
  name              = "COVID-19",
  n                 = as.integer(N),
  prevalence        = initial_prevalence,
  contact_rate      = 5.0,
  transmission_rate = 0.0571,
  recovery_rate     = recov
)

verbose_off(model_sir)

# Simulation Function -----------------------------------------------------
simulation_fun <- function(params, lfmcmc_obj) {
  # params: [contact_rate, recovery_rate, transmission_rate]

  set_param(model_sir, "Contact rate", params[1])
  set_param(model_sir, "Recovery rate", params[2])
  set_param(model_sir, "Transmission rate", params[3])

  run(model_sir, ndays = model_ndays)

  # Get daily incidence data instead of cumulative counts
  incidence_data <- plot_incidence(model_sir, plot = FALSE)
  infected_counts <- incidence_data$Infected
  return(as.numeric(infected_counts))
}

# CHANGED: Simpler, more stable summary statistics
summary_fun <- function(data, lfmcmc_obj) {
  c(
    sum(data),                  # Total cases
    max(data),                  # Peak height
    which.max(data),            # Day of peak
    mean(data[1:20]),           # Early phase mean
    mean(data[21:40]),          # Middle phase mean
    mean(data[41:60])           # Late phase mean
  )
}

# Proposal function uses log transform for contact rate (no reflection needed)
proposal_fun <- function(old_params, lfmcmc_obj) {
  # Contact rate: use log transform to ensure positivity
  new_crate <- exp(log(old_params[1]) + rnorm(1, sd = 0.1))

  # Recovery and transmission rates: use logit transform (bounded [0,1])
  new_recov <- plogis(qlogis(old_params[2]) + rnorm(1, sd = 0.025))
  new_ptran <- plogis(qlogis(old_params[3]) + rnorm(1, sd = 0.025))

  return(c(new_crate, new_recov, new_ptran))
}

# CHANGED: Weighted kernel function to prevent any stat from dominating
kernel_fun <- function(simulated_stat, observed_stat, epsilon, lfmcmc_obj) {
  # Normalize each statistic by its observed value
  weights <- 1 / (abs(observed_stat) + 1)
  diff <- sum(weights * (simulated_stat - observed_stat)^2)
  return(exp(-diff / (2 * epsilon^2)))
}

# Run ABC Calibration ------------------------------------------------------
cat("\n=== ABC Calibration ===\n")

lfmcmc_obj <- LFMCMC(model_sir)
lfmcmc_obj <- set_simulation_fun(lfmcmc_obj, simulation_fun)
lfmcmc_obj <- set_summary_fun(lfmcmc_obj, summary_fun)
lfmcmc_obj <- set_proposal_fun(lfmcmc_obj, proposal_fun)
lfmcmc_obj <- set_kernel_fun(lfmcmc_obj, kernel_fun)

# IMPORTANT: Apply summary function to observed data before setting it
observed_summary_stats <- summary_fun(incidence_vec, NULL)
cat("Observed summary statistics:", round(observed_summary_stats, 2), "\n")
lfmcmc_obj <- set_observed_data(lfmcmc_obj, incidence_vec)

init_params <- c(5, 1/7, 0.0571)
n_samples_calib <- 3000
burnin <- 1500

# CHANGED: Lower epsilon since we're using weighted distance
epsilon <- 10  # Reduced from 100

cat("Running LFMCMC with", n_samples_calib, "samples (50% burn-in)...\n")
cat("Using", length(observed_summary_stats), "summary statistics\n")
cat("Epsilon:", epsilon, "\n\n")

tic("ABC calibration")
run_lfmcmc(
  lfmcmc = lfmcmc_obj,
  params_init = init_params,
  n_samples = n_samples_calib,
  epsilon = epsilon,
  seed = model_seed
)
abc_time_output <- toc()

# Extract calibration time
abc_calibration_time <- abc_time_output$toc - abc_time_output$tic

# Process Results ----------------------------------------------------------
accepted <- get_all_accepted_params(lfmcmc_obj)
acceptance_rate <- nrow(accepted) / n_samples_calib * 100

cat("\nAcceptance rate:", round(acceptance_rate, 2), "%\n")

post_burnin <- tail(accepted, n = (n_samples_calib - burnin))

abc_median <- apply(post_burnin, 2, median)
abc_lower <- apply(post_burnin, 2, quantile, probs = 0.025)
abc_upper <- apply(post_burnin, 2, quantile, probs = 0.975)

abc_crate <- abc_median[1]
abc_recov <- abc_median[2]
abc_ptran <- abc_median[3]
abc_R0 <- (abc_crate * abc_ptran) / abc_recov

cat("\nCalibrated Parameters (Median with 95% CI):\n")
cat("Contact Rate:", round(abc_crate, 4),
    "[", round(abc_lower[1], 4), ",", round(abc_upper[1], 4), "]\n")
cat("Recovery Rate:", round(abc_recov, 4),
    "[", round(abc_lower[2], 4), ",", round(abc_upper[2], 4), "]\n")
cat("Transmission Prob:", round(abc_ptran, 4),
    "[", round(abc_lower[3], 4), ",", round(abc_upper[3], 4), "]\n")
cat("R0:", round(abc_R0, 4), "\n")

# Save calibration results -------------------------------------------------
abc_calibration_params <- list(
  contact_rate = abc_crate,
  recovery_rate = abc_recov,
  transmission_prob = abc_ptran,
  R0 = abc_R0,
  contact_rate_ci = c(lower = abc_lower[1], upper = abc_upper[1]),
  recovery_rate_ci = c(lower = abc_lower[2], upper = abc_upper[2]),
  transmission_prob_ci = c(lower = abc_lower[3], upper = abc_upper[3]),
  calibration_time_seconds = abc_calibration_time,
  n_samples = n_samples_calib,
  burnin = burnin,
  epsilon = epsilon,
  seed = model_seed,
  posterior_samples = post_burnin,
  acceptance_rate = acceptance_rate
)

# Save to data folder
usethis::use_data(abc_calibration_params, overwrite = TRUE)
# Trace Plot for 3 Parameters ----------------------------------------------
accepted <- get_all_accepted_params(lfmcmc_obj)

# Get range for all parameters
y_min <- min(accepted)
y_max <- max(accepted)

plot(
  accepted[, 1], type = "l", ylim = c(y_min, y_max),
  main = "Trace of the parameters",
  lwd = 2,
  col = "tomato",
  xlab = "Step",
  ylab = "Parameter value"
)

lines(accepted[, 2], type = "l", lwd = 2, col = "steelblue")
lines(accepted[, 3], type = "l", lwd = 2, col = "darkgreen")

legend(
  "topright",
  bty = "n",
  legend = c("Contact rate", "Recovery rate", "Transmission rate"),
  pch    = 20,
  col    = c("tomato", "steelblue", "darkgreen")
)
cat("\n✓ Saved abc_calibration_params to data/abc_calibration_params.rda\n")
cat("  Calibration time:", round(abc_calibration_time, 2), "seconds\n")
cat("  Acceptance rate:", round(acceptance_rate, 2), "%\n")
