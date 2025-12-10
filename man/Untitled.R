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

stopifnot(length(incidence) == n_days)

# 2) LSTM calibration
N     <- 10000
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

# 4) Build + run SIR model (ndays = 60 yields a 61-point incidence series)
model <- ModelSIRCONN(
  name              = "Utah LSTM-calibrated SIR",
  n                 = N,
  prevalence        = prev,
  contact_rate      = lstm_predictions[["crate"]],
  transmission_rate = lstm_predictions[["ptran"]],
  recovery_rate     = recov
)

run(model, ndays = n_days - 1)
?run_multiple
# 5) Extract model infected incidence
inc_df <- plot_incidence(model, plot = FALSE)

infected_col <- intersect(
  names(inc_df),
  c("Infected", "infected", "Infected.Incidence", "infected_incidence")
)

if (length(infected_col) == 0L)
  stop("Could not find model infected column.")

model_infected <- inc_df[[infected_col[1]]][seq_len(n_days)]

results <- tibble(
  Date           = covid_data$Date,
  observed_cases = incidence,
  model_infected = model_infected
)

## --------------------------------------------------------------------
## PLOTS
## --------------------------------------------------------------------

# (A) Overlay plot
p_overlay <- ggplot(results, aes(x = Date)) +
  geom_line(
    aes(y = model_infected, color = "Model infected"),
    linewidth = 1.4
  ) +
  geom_point(
    aes(y = model_infected, color = "Model infected"),
    size = 1.8
  ) +
  geom_line(
    aes(y = observed_cases, color = "Observed cases"),
    linewidth = 1.4
  ) +
  geom_point(
    aes(y = observed_cases, color = "Observed cases"),
    size = 1.8
  ) +
  scale_color_manual(
    values = c(
      "Observed cases" = "blue",
      "Model infected" = "red"
    )
  ) +
  labs(
    title = "Utah COVID-19: Observed vs LSTM-Calibrated SIR",
    x = "",
    y = "Daily Counts",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

p_overlay



## ================================================================
##  ASSUMES: covid_data, incidence, lstm_predictions, N, recov, n_days
## ================================================================

# ---- 1. Build base model (not run yet) ----
model <- ModelSIRCONN(
  name              = "Utah LSTM-calibrated SIR",
  n                 = N,
  prevalence        = incidence[1] / N,
  contact_rate      = lstm_predictions[["crate"]],
  transmission_rate = lstm_predictions[["ptran"]],
  recovery_rate     = recov
)

# ---- 2. Run model 100 times ----
saver <- make_saver("total_hist")

run_multiple(
  model,
  ndays   = n_days - 1,   # 60 → gives 61 incidence points
  nsims   = 100,
  saver   = saver,
  nthreads = 8
)

sim_results <- run_multiple_get_results(model)


# ---- 3. Extract daily infected incidence from all 100 runs ----
inc_hist <- sim_results$total_hist %>%
  filter(state == "Infected") %>%
  arrange(sim_num, date)

# Convert to cumulative for each simulation
cum_sim <- inc_hist %>%
  group_by(sim_num) %>%
  mutate(cum_infected = cumsum(counts)) %>%
  ungroup()

# Mean cumulative infected across the 100 simulations
mean_cum <- cum_sim %>%
  group_by(date) %>%
  summarize(
    mean_cum_infected = mean(cum_infected),
    .groups = "drop"
  )


# ---- 4. Cumulative observed cases ----
covid_cum <- tibble(
  date            = 0:(n_days - 1),
  cum_observed    = cumsum(incidence)
)


# ---- 5. Merge for plotting ----
plot_df <- left_join(mean_cum, covid_cum, by = "date") %>%
  mutate(Date = covid_data$Date)


# ---- 6. Plot cumulative model vs cumulative observed ----
p_cumulative <- ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = mean_cum_infected, color = "Model cumulative infected"),
            linewidth = 1.4) +
  geom_line(aes(y = cum_observed, color = "Cumulative observed cases"),
            linewidth = 1.4) +
  scale_color_manual(
    values = c(
      "Model cumulative infected" = "red",
      "Cumulative observed cases" = "blue"
    )
  ) +
  labs(
    title = "Cumulative Infections: Observed vs LSTM-Calibrated SIR (100 runs)",
    x = "",
    y = "Cumulative counts",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

p_cumulative


