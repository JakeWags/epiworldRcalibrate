suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(epiworldR)
  library(epiworldRcalibrate)
  library(zoo)  # for rolling averages
})

## --------------------------------------------------------------------
## DATA HELPERS
## --------------------------------------------------------------------

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
  covid_data <- subset(covid_data, Date > (last_date - n_days)) %>% arrange(Date)

  stopifnot("Daily.Cases" %in% names(covid_data))
  covid_data
}

## --------------------------------------------------------------------
## MODEL + CALIBRATION PIPELINE WITH IMPROVEMENTS
## --------------------------------------------------------------------

# 1) Load last 61 days of Utah cases
n_days <- 61
covid_data <- get_covid_data(n_days)
incidence <- covid_data$Daily.Cases

# OPTION 1: Smooth the observed data with 7-day rolling average
incidence_smoothed <- rollmean(incidence, k = 7, fill = "extend", align = "center")

cat("=== Data Summary ===\n")
cat("Original incidence range:", min(incidence), "to", max(incidence), "\n")
cat("Mean daily cases:", mean(incidence), "\n")
cat("Smoothed incidence range:", min(incidence_smoothed), "to", max(incidence_smoothed), "\n")

stopifnot(length(incidence) == n_days)

# 2) IMPROVED CALIBRATION
# Try larger population size for better dynamics
N <- 5000  # Increased from 10000
recov <- 1 / 7

cat("\n=== Running LSTM Calibration ===\n")
cat("Population size:", N, "\n")
cat("Recovery rate:", recov, "(7-day infectious period)\n")

# Calibrate on SMOOTHED data for better parameter estimation
lstm_predictions <- calibrate_sir(
  daily_cases     = incidence_smoothed,
  population_size = N,
  recovery_rate   = recov
)

cat("\n=== Calibrated Parameters ===\n")
print(lstm_predictions)

# 3) Initial prevalence based on first smoothed value
init_infected <- incidence_smoothed[1]
prev <- init_infected / N

cat("\nInitial infected:", init_infected, "\n")
cat("Initial prevalence:", prev, "\n")

## ================================================================
##  RUN 1000 SIMULATIONS WITH TRANSITION MATRICES
## ================================================================

# ---- 1. Build base model ----
model <- ModelSIRCONN(
  name              = "Utah LSTM-calibrated SIR (Improved)",
  n                 = N,
  prevalence        = prev,
  contact_rate      = lstm_predictions[["crate"]],
  transmission_rate = lstm_predictions[["ptran"]],
  recovery_rate     = recov
)

# ---- 2. Run model 1000 times with transition matrix saver ----
cat("\n=== Running 1000 simulations... ===\n")
saver <- make_saver("transition")

run_multiple(
  model,
  ndays    = n_days - 1,
  nsims    = 1000,
  saver    = saver,
  nthreads = 8
)

sim_results <- run_multiple_get_results(model)

# ---- 3. Extract S→I transitions ----
cat("\n=== Extracting S→I transitions... ===\n")
transitions <- sim_results$transition %>%
  filter(from == "Susceptible", to == "Infected") %>%
  arrange(sim_num, date)

# ---- 4. Calculate quantiles for each day ----
cat("\n=== Calculating 95% CI... ===\n")
quantiles_df <- transitions %>%
  group_by(date) %>%
  summarize(
    lower_ci = quantile(counts, 0.025),
    upper_ci = quantile(counts, 0.975),
    median   = quantile(counts, 0.5),
    mean     = mean(counts),
    sd       = sd(counts),
    .groups  = "drop"
  )

cat("Model predictions range:", min(quantiles_df$median), "to", max(quantiles_df$median), "\n")
cat("CI width (mean):", mean(quantiles_df$upper_ci - quantiles_df$lower_ci), "\n")

# ---- 5. Prepare plot data with both original and smoothed ----
plot_df <- quantiles_df %>%
  mutate(
    Date                 = covid_data$Date,
    observed_cases       = incidence,
    observed_smoothed    = incidence_smoothed
  )

# ---- 6. PLOT 1: With smoothed observed data ----
p_smoothed <- ggplot(plot_df, aes(x = Date)) +
  # Confidence interval ribbon
  geom_ribbon(
    aes(ymin = lower_ci, ymax = upper_ci, fill = "95% CI"),
    alpha = 0.3
  ) +
  # Model median
  geom_line(
    aes(y = median, color = "Model median"),
    linewidth = 1.2
  ) +
  # Smoothed observed (thicker, more visible)
  geom_line(
    aes(y = observed_smoothed, color = "Observed (7-day avg)"),
    linewidth = 1.5
  ) +
  # Original observed (thinner, for reference)
  geom_line(
    aes(y = observed_cases, color = "Observed (daily)"),
    linewidth = 0.8,
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Model median"           = "red",
      "Observed (7-day avg)"   = "blue",
      "Observed (daily)"       = "lightblue"
    )
  ) +
  scale_fill_manual(
    values = c("95% CI" = "red")
  ) +
  labs(
    title = "Daily Infected: Observed vs Model with 95% CI",
    subtitle = paste0("Population: ", format(N, big.mark=","),
                      " | Contact rate: ", round(lstm_predictions[["crate"]], 3),
                      " | Trans. prob: ", round(lstm_predictions[["ptran"]], 3)),
    x = "",
    y = "Daily Counts",
    color = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

print(p_smoothed)

# Save plot
ggsave(
  "man/infected_with_ci_improved.png",
  plot = p_smoothed,
  width = 14,
  height = 8,
  dpi = 300
)

# ---- 7. PLOT 2: Log scale for better visualization ----
p_log <- p_smoothed +
  scale_y_log10() +
  labs(
    title = "Daily Infected: Log Scale",
    y = "Daily Counts (log scale)"
  )

ggsave(
  "/mnt/user-data/outputs/infected_with_ci_logscale.png",
  plot = p_log,
  width = 14,
  height = 8,
  dpi = 300
)

# ---- 8. Calculate metrics ----
# Coverage
coverage <- plot_df %>%
  mutate(
    in_ci_original = observed_cases >= lower_ci & observed_cases <= upper_ci,
    in_ci_smoothed = observed_smoothed >= lower_ci & observed_smoothed <= upper_ci
  ) %>%
  summarize(
    coverage_original = mean(in_ci_original),
    coverage_smoothed = mean(in_ci_smoothed),
    total_days = n()
  )

# RMSE and MAE
metrics <- plot_df %>%
  summarize(
    RMSE_original = sqrt(mean((observed_cases - median)^2)),
    RMSE_smoothed = sqrt(mean((observed_smoothed - median)^2)),
    MAE_original  = mean(abs(observed_cases - median)),
    MAE_smoothed  = mean(abs(observed_smoothed - median)),
    corr_original = cor(observed_cases, median),
    corr_smoothed = cor(observed_smoothed, median)
  )

cat("\n=== Model Performance Metrics ===\n")
cat("Coverage (original data in CI):", round(coverage$coverage_original * 100, 1), "%\n")
cat("Coverage (smoothed data in CI):", round(coverage$coverage_smoothed * 100, 1), "%\n")
cat("\nRMSE (original):", round(metrics$RMSE_original, 2), "\n")
cat("RMSE (smoothed):", round(metrics$RMSE_smoothed, 2), "\n")
cat("\nMAE (original):", round(metrics$MAE_original, 2), "\n")
cat("MAE (smoothed):", round(metrics$MAE_smoothed, 2), "\n")
cat("\nCorrelation (original):", round(metrics$corr_original, 3), "\n")
cat("Correlation (smoothed):", round(metrics$corr_smoothed, 3), "\n")

# ---- 9. Save detailed results ----
write_csv(
  plot_df,
  "/mnt/user-data/outputs/sir_model_results_improved.csv"
)

# Summary table
summary_table <- tibble(
  Metric = c("Population Size", "Contact Rate", "Transmission Prob",
             "Recovery Rate", "Initial Infected", "RMSE", "Correlation",
             "Coverage Rate"),
  Value = c(
    N,
    lstm_predictions[["crate"]],
    lstm_predictions[["ptran"]],
    recov,
    init_infected,
    metrics$RMSE_smoothed,
    metrics$corr_smoothed,
    coverage$coverage_smoothed
  )
)

write_csv(summary_table, "/mnt/user-data/outputs/model_summary.csv")

cat("\n=== Files saved ===\n")
cat("- infected_with_ci_improved.png\n")
cat("- infected_with_ci_logscale.png\n")
cat("- sir_model_results_improved.csv\n")
cat("- model_summary.csv\n")

cat("\n=== RECOMMENDATIONS ===\n")
if (metrics$corr_smoothed < 0.5) {
  cat("⚠ Low correlation suggests model may need:\n")
  cat("  1. Different population size (try 100000 or larger)\n")
  cat("  2. Time-varying parameters (contact rate changes over time)\n")
  cat("  3. More complex model (SEIR, age structure, etc.)\n")
}
if (coverage$coverage_smoothed < 0.8) {
  cat("⚠ Low coverage suggests CI may be too narrow\n")
  cat("  Consider parameter uncertainty in calibration\n")
}
if (metrics$RMSE_smoothed > mean(incidence_smoothed) * 0.5) {
  cat("⚠ High RMSE suggests poor fit\n")
  cat("  Model systematically under/over predicts\n")
}
