

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(epiworldR)
  library(epiworldRcalibrate)
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
## MODEL + CALIBRATION PIPELINE
## --------------------------------------------------------------------

# 1) Load last 61 days of Utah cases
n_days <- 61
covid_data <- get_covid_data(n_days)
incidence <- covid_data$Daily.Cases
incidence_vec=incidence
stopifnot(length(incidence) == n_days)
n=5000
recov=1/7
# 2) LSTM calibration
N     <- 5000
recov <- 1 / 7
lstm_predictions <- calibrate_sir(
  daily_cases     = incidence,
  population_size = N,
  recovery_rate   = recov
)

print(lstm_predictions)

# 3) Initial prevalence
init_infected <- incidence[1]
prev <- init_infected / N

## ================================================================
##  RUN 1000 SIMULATIONS WITH TRANSITION MATRICES
## ================================================================

# ---- 1. Build base model ----
model <- ModelSIRCONN(
  name              = "Utah LSTM-calibrated SIR",
  n                 = N,
  prevalence        = incidence[1] / N,
  contact_rate      = lstm_predictions[["crate"]],
  transmission_rate = lstm_predictions[["ptran"]],
  recovery_rate     = recov
)
1.3220*1/7
0.1888571/0.0804
model <- ModelSIRCONN(
  name              = "Utah LSTM-calibrated SIR",
  n                 = 5000,
  prevalence        = incidence[1] / 10000,
  contact_rate      = 2.4,
  transmission_rate = 0.083,
  recovery_rate     = recov
)
# ---- 2. Run model 1000 times with transition matrix saver ----
cat("\n=== Running 1000 simulations... ===\n")
saver <- make_saver("transition")

run_multiple(
  model,
  ndays    = n_days - 1,   # 60 → gives 61 time points (0-60)
  nsims    = 2000,
  saver    = saver,
  nthreads = 8
)
1.3944*1/7
0.1992/0.083
sim_results <- run_multiple_get_results(model,nthreads=8,freader=data.table::fread)

# ---- 3. Extract S→I transitions from transition matrices ----
cat("\n=== Extracting S→I transitions... ===\n")
# The transition matrix has from_state, to_state, date, sim_num, counts
transitions <- sim_results$transition %>%
  filter(from == "Susceptible", to == "Infected") %>%
  arrange(sim_num, date)

cat("Dimensions of transitions data:\n")
print(dim(transitions))
cat("\nFirst few rows:\n")
print(head(transitions))

# ---- 4. Calculate quantiles for each day ----
cat("\n=== Calculating 95% CI (0.025 and 0.975 quantiles)... ===\n")
quantiles_df <- transitions %>%
  group_by(date) %>%
  summarize(
    lower_ci = quantile(counts, 0.025),
    upper_ci = quantile(counts, 0.975),
    median   = quantile(counts, 0.5),
    mean     = mean(counts),
    .groups  = "drop"
  )
incidence
# ---- 5. Merge with observed data ----
plot_df <- quantiles_df %>%
  mutate(
    Date           = covid_data$Date,
    observed_cases = incidence
  )

# ---- 6. Plot with confidence intervals ----
p_with_ci <- ggplot(plot_df, aes(x = Date)) +
  # Confidence interval ribbon
  geom_ribbon(
    aes(ymin = lower_ci, ymax = upper_ci),
    fill = "red",
    alpha = 0.3
  ) +
  # Model median
  geom_line(
    aes(y = median, color = "Model median (S→I)"),
    linewidth = 1.2
  ) +
  # Observed cases
  geom_line(
    aes(y = observed_cases, color = "Observed cases"),
    linewidth = 1.4
  ) +
  geom_point(
    aes(y = observed_cases, color = "Observed cases"),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "Model median (S→I)" = "red",
      "Observed cases"     = "blue"
    )
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
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

print(p_with_ci)
1.5663*1/7
0.2237571/0.091
# Save the plot
ggsave(
  "man/infected_with_ci_2000run_10000pop.png",
  plot = p_with_ci,
  width = 12,
  height = 7,
  dpi = 300
)

# ---- 7. Summary statistics ----
cat("\n=== Summary of S→I transitions ===\n")
print(summary(quantiles_df))

# Check coverage: how many observed points fall within CI
coverage <- plot_df %>%
  mutate(in_ci = observed_cases >= lower_ci & observed_cases <= upper_ci) %>%
  summarize(
    coverage_rate = mean(in_ci),
    total_days    = n(),
    days_in_ci    = sum(in_ci)
  )

cat("\n=== CI Coverage ===\n")
print(coverage)
cat(sprintf("\n%.1f%% of observed cases fall within the 95%% CI\n",
            coverage$coverage_rate * 100))

# ---- 8. Save results to CSV ----
write_csv(
  plot_df,
  "man/sir_model_results_with_ci_2000run_10000pop.csv"
)

cat("\n=== Results saved to outputs directory ===\n")
