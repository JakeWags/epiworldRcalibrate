# Load required libraries
library(epiworldR)
library(data.table)
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(cowplot)

# --------------------------
# Data Download Functions
# --------------------------

get_data_from_url <- function(data_url, target_file) {
  tryCatch({
    temp_file <- tempfile()
    download.file(data_url, temp_file, mode = "wb")

    datafile_name <- grep(
      target_file,
      unzip(temp_file, list = TRUE)$Name,
      ignore.case = TRUE,
      value = TRUE
    )

    if (length(datafile_name) == 0L)
      stop("Target file not found in ZIP archive.")

    datafile <- unz(temp_file, datafile_name[1])
    collected_data <- read.csv(datafile)
    collected_data
  },
  error = function(cond) {
    message("Error: ", conditionMessage(cond))
    return(-1)
  },
  warning = function(cond) {
    message("Warning: ", conditionMessage(cond))
    return(-2)
  },
  finally = unlink(temp_file))
}

get_covid_data <- function(n_days) {
  data_url <- "https://coronavirus-dashboard.utah.gov/Utah_COVID19_data.zip"
  target_file_regex <- "Trends_Epidemic+"

  covid_data <- get_data_from_url(data_url, target_file_regex)

  if (is.numeric(covid_data) && covid_data < 0)
    stop("Failed to download or read Utah COVID-19 data.")

  covid_data$Date <- as.Date(covid_data$Date)

  last_date <- max(covid_data$Date, na.rm = TRUE)
  covid_data <- subset(covid_data, Date > (last_date - n_days)) %>%
    arrange(Date)

  stopifnot("Daily.Cases" %in% names(covid_data))
  return(covid_data)
}

# --------------------------
# Global Simulation Settings
# --------------------------
model_ndays <- 60   # simulation duration (days)
model_seed  <- 122  # seed for reproducibility
global_n    <- 10000  # population size (used in calibration)

# --------------------------
# Load Real COVID-19 Data
# --------------------------
cat("Loading real Utah COVID-19 data...\n")
n_days <- 61
covid_data <- get_covid_data(n_days)
incidence <- covid_data$Daily.Cases
incidence_vec <- incidence
stopifnot(length(incidence) == n_days)

observed_infected <- incidence_vec

cat("Date range:", as.character(min(covid_data$Date)), "to",
    as.character(max(covid_data$Date)), "\n")
cat("Total cases:", sum(observed_infected), "\n")
cat("Mean daily cases:", round(mean(observed_infected), 1), "\n\n")

# --------------------------
# Simulation Functions
# --------------------------

# Function to simulate epidemic for calibration
simulate_epidemic_calib <- function(params, ndays = model_ndays, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Fixed parameter indexing [n, preval, crate, recov, ptran]
  sim_model <- ModelSIRCONN(
    name="sim",
    n                 = as.integer(params[1]),    # n
    prevalence        = as.numeric(params[2]),    # prevalence
    contact_rate      = as.numeric(params[3]),    # contact_rate
    transmission_rate = as.numeric(params[5]),    # transmission_rate (ptran)
    recovery_rate     = as.numeric(params[4])     # recovery_rate
  )

  verbose_off(sim_model)
  run(sim_model, ndays = ndays)

  # Get only the infected counts
  counts <- get_hist_total(sim_model)
  infected_counts <- counts[counts$state == "Infected", "counts"]

  return(infected_counts)
}

# Fixed simulation function for ABC
simulation_fun <- function(params, lfmcmc_obj, observed_data_info) {
  # params = [crate, recov, ptran]
  # observed_data_info contains n and initial prevalence

  # Fixed parameter assembly - maintain correct order [n, preval, crate, recov, ptran]
  sim_params <- c(
    observed_data_info$n,          # n (fixed)
    observed_data_info$preval,     # prevalence (fixed)
    params[1],                     # contact_rate (being calibrated)
    params[2],                     # recovery_rate (being calibrated)
    params[3]                      # transmission_prob (being calibrated)
  )

  # Run simulation with the parameters
  infected_counts <- simulate_epidemic_calib(sim_params, ndays = model_ndays)
  return(as.numeric(infected_counts))
}

# Summary function for ABC
summary_fun <- function(data, lfmcmc_obj) {
  return(as.numeric(data))
}

# Generate new parameter proposals
proposal_fun <- function(old_params, lfmcmc_obj) {
  # old_params contains: contact_rate, recovery_rate, transmission_prob

  new_crate <- old_params[1] * exp(rnorm(1, sd = 0.1))  # Log-normal proposal
  new_recov <- plogis(qlogis(old_params[2]) + rnorm(1, sd = 0.1))
  new_ptran <- plogis(qlogis(old_params[3]) + rnorm(1, sd = 0.1))

  return(c(new_crate, new_recov, new_ptran))
}

# Kernel function using sum of squared differences across all days
kernel_fun <- function(simulated_stat, observed_stat, epsilon, lfmcmc_obj) {
  # Calculate the sum of squared differences across all days
  diff <- sum((simulated_stat - observed_stat)^2)
  return(exp(-diff / (2 * epsilon^2)))
}

# --------------------------
# ABC Calibration Function
# --------------------------

run_abc_calibration <- function(observed_data, population_size, seed = model_seed) {
  tryCatch({
    cat("Running ABC calibration on real data...\n")

    # Setup observed data info
    initial_infected <- observed_data[1]
    initial_prevalence <- initial_infected / population_size

    observed_data_info <- list(
      n = population_size,
      preval = initial_prevalence
    )

    # Create dummy model for LFMCMC
    dummy_model <- ModelSIRCONN(
      name              = "dummy",
      n                 = as.integer(population_size),
      prevalence        = 0.1,
      contact_rate      = 5.0,
      transmission_rate = 0.1,
      recovery_rate     = 0.1
    )

    # Create a local wrapper that captures observed_data_info
    local_simulation_fun <- function(params, lfmcmc_obj) {
      return(simulation_fun(params, lfmcmc_obj, observed_data_info))
    }

    # Setup and run LFMCMC with the full time series
    lfmcmc_obj <- LFMCMC(dummy_model)
    lfmcmc_obj <- set_simulation_fun(lfmcmc_obj, local_simulation_fun)
    lfmcmc_obj <- set_summary_fun(lfmcmc_obj, summary_fun)
    lfmcmc_obj <- set_proposal_fun(lfmcmc_obj, proposal_fun)
    lfmcmc_obj <- set_kernel_fun(lfmcmc_obj, kernel_fun)
    lfmcmc_obj <- set_observed_data(lfmcmc_obj, observed_data)

    # Initial parameters for ABC [crate, recov, ptran]
    init_params <- c(
      5,              # contact rate
      1/7,            # recovery rate
      0.0571          # transmission probability
    )

    # Run the LFMCMC
    n_samples_calib <- 1000
    burnin <- 500  # 50% burn-in
    epsilon <- sqrt(sum(observed_data^2)) * 0.05

    cat("Running LFMCMC with", n_samples_calib, "samples...\n")
    cat("Burn-in:", burnin, "(50%)\n")
    cat("Epsilon:", round(epsilon, 2), "\n\n")

    start_time <- Sys.time()
    epiworldR::run_lfmcmc(
      lfmcmc = lfmcmc_obj,
      params_init = init_params,
      n_samples = n_samples_calib,
      epsilon = epsilon,
      seed = seed
    )
    end_time <- Sys.time()

    cat("Calibration completed in", round(difftime(end_time, start_time, units = "secs"), 1), "seconds\n\n")

    # Get accepted parameters from the MCMC chain
    accepted <- get_all_accepted_params(lfmcmc_obj)

    # Calculate parameters with burn-in
    if (!is.null(accepted) && nrow(accepted) > 0) {
      # Apply burn-in
      post_burnin <- tail(accepted, n = (n_samples_calib - burnin))

      calibrated_params_raw <- apply(post_burnin, 2, median)
      calibrated_lower <- apply(post_burnin, 2, quantile, probs = 0.025)
      calibrated_upper <- apply(post_burnin, 2, quantile, probs = 0.975)

      abc_crate <- calibrated_params_raw[1]
      abc_recov <- calibrated_params_raw[2]
      abc_ptran <- calibrated_params_raw[3]

      # Assemble calibrated parameters in correct order [n, preval, crate, recov, ptran]
      calibrated_params <- c(
        population_size,      # n (fixed)
        initial_prevalence,   # prevalence (fixed)
        abc_crate,           # calibrated contact rate
        abc_recov,           # calibrated recovery rate
        abc_ptran            # calibrated transmission probability
      )

      # Calculate R0
      abc_R0 <- (abc_crate * abc_ptran) / abc_recov
      R0_samples <- (post_burnin[, 1] * post_burnin[, 3]) / post_burnin[, 2]
      abc_R0_lower <- quantile(R0_samples, 0.025)
      abc_R0_upper <- quantile(R0_samples, 0.975)

      cat("=== Calibrated Parameters (Median with 95% CI) ===\n")
      cat("Contact Rate:", round(abc_crate, 4),
          "[", round(calibrated_lower[1], 4), ",", round(calibrated_upper[1], 4), "]\n")
      cat("Recovery Rate:", round(abc_recov, 4),
          "[", round(calibrated_lower[2], 4), ",", round(calibrated_upper[2], 4), "]\n")
      cat("Transmission Prob:", round(abc_ptran, 4),
          "[", round(calibrated_lower[3], 4), ",", round(calibrated_upper[3], 4), "]\n")
      cat("R0:", round(abc_R0, 4),
          "[", round(abc_R0_lower, 4), ",", round(abc_R0_upper, 4), "]\n\n")

      cat("Acceptance rate:", nrow(accepted) / n_samples_calib * 100, "%\n")
      cat("Post-burnin samples:", nrow(post_burnin), "\n\n")

    } else {
      stop("ABC calibration failed - no accepted parameters")
    }

    # Simulate predictions using the ABC calibrated parameters
    abc_predicted_infected <- simulate_epidemic_calib(calibrated_params, ndays = model_ndays,
                                                      seed = seed + 200)

    # Create results dataframe for all days
    days <- 0:model_ndays
    daily_results <- data.frame(
      day = days,
      date = covid_data$Date,
      calib_crate = abc_crate,
      calib_recov = abc_recov,
      calib_ptran = abc_ptran,
      calib_R0 = abc_R0,
      observed_infected = observed_data,
      abc_predicted_infected = abc_predicted_infected,
      abc_bias = abc_predicted_infected - observed_data,
      abc_rel_bias = ifelse(observed_data > 0,
                            (abc_predicted_infected - observed_data) / observed_data,
                            NA)
    )

    # ABC parameters summary
    abc_parameters <- data.frame(
      parameter = c("contact_rate", "recovery_rate", "transmission_prob", "R0"),
      median = c(abc_crate, abc_recov, abc_ptran, abc_R0),
      lower_95 = c(calibrated_lower[1], calibrated_lower[2], calibrated_lower[3], abc_R0_lower),
      upper_95 = c(calibrated_upper[1], calibrated_upper[2], calibrated_upper[3], abc_R0_upper),
      n_samples = n_samples_calib,
      n_burnin = burnin,
      n_post_burnin = nrow(post_burnin),
      time_seconds = as.numeric(difftime(end_time, start_time, units = "secs"))
    )

    return(list(
      daily_results = daily_results,
      abc_parameters = abc_parameters,
      calibrated_params = calibrated_params
    ))

  }, error = function(e) {
    cat("Error in ABC calibration:", e$message, "\n")
    return(NULL)
  })
}

# --------------------------
# Run Calibration
# --------------------------

cat("Starting ABC calibration...\n")
calibration_start_time <- Sys.time()

results <- run_abc_calibration(observed_infected, global_n, seed = model_seed)

calibration_end_time <- Sys.time()
total_time <- difftime(calibration_end_time, calibration_start_time, units = "secs")

cat("\n===========================================================\n")
cat("Total ABC calibration time:", round(total_time, 2), "seconds\n")
cat("===========================================================\n\n")
cat("Total ABC calibration time:", round(total_time/60, 2), "minutes\n")

if (!is.null(results)) {
  # Save results
  write.csv(results$daily_results, "abc_real_data_daily_results.csv", row.names = FALSE)
  write.csv(results$abc_parameters, "abc_real_data_parameters.csv", row.names = FALSE)

  cat("Results saved to CSV files\n\n")

  # --------------------------
  # Visualization
  # --------------------------

  cat("Generating visualizations...\n")

  # Plot observed vs predicted
  p <- ggplot(results$daily_results, aes(x = date)) +
    geom_line(aes(y = observed_infected, color = "Observed"), linewidth = 1.2) +
    geom_point(aes(y = observed_infected, color = "Observed"), size = 2) +
    geom_line(aes(y = abc_predicted_infected, color = "ABC Calibrated"),
              linewidth = 1.2, linetype = "dashed") +
    scale_color_manual(values = c("Observed" = "black", "ABC Calibrated" = "red")) +
    labs(
      title = "ABC Calibration: Real Utah COVID-19 Data",
      subtitle = paste0(
        "R0: ", round(results$abc_parameters$median[4], 2),
        " | Contact rate: ", round(results$abc_parameters$median[1], 2),
        " | Trans prob: ", round(results$abc_parameters$median[3], 3)
      ),
      x = "Date",
      y = "Daily Infected Count",
      color = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )

  print(p)
  ggsave("abc_real_data_calibration_plot.png", p, width = 10, height = 6, dpi = 300)

  # Calculate fit metrics
  rmse <- sqrt(mean((results$daily_results$abc_predicted_infected -
                       results$daily_results$observed_infected)^2))
  mae <- mean(abs(results$daily_results$abc_predicted_infected -
                    results$daily_results$observed_infected))

  cat("\n=== Fit Metrics ===\n")
  cat("RMSE:", round(rmse, 2), "\n")
  cat("MAE:", round(mae, 2), "\n")
  cat("Total calibration time:", round(total_time, 2), "seconds\n\n")

} else {
  cat("Calibration failed\n")
}

cat("===========================================================\n")
cat("  ABC Calibration Complete\n")
cat("===========================================================\n")
