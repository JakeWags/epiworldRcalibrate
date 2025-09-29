suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(epiworldR)
  library(epiworldRcalibrate)
})

# 1) Pull RAW daily cases (no smoothing) and keep the last 61 days
raw <- get_covid_data(120) %>% arrange(Date)
seg <- raw %>% filter(!is.na(Daily.Cases)) %>% tail(61)

# Incidence vector for LSTM: RAW daily cases
incidence <- as.numeric(seg$Daily.Cases)
stopifnot(length(incidence) == 61L)

# 2) Predict parameters with your BiLSTM
#    (point to YOUR models dir per your README)
init_bilstm_model("~/Desktop/epiworldRcalibrate_fixed/epiworldRcalibrate/inst/models")

N     <- 100000   # population size used as a covariate and for simulation
recov <- 1/7      # ~7-day infectious period

params <- predict_sir_bilstm(incidence, n = N, recov = recov)
print(params)
# $ptran, $crate, $R0

# 3) Set initial prevalence to match the dataset
#    We convert the FIRST day's cases into initial infected using an ascertainment factor.
ascertainment <- 1                 # tweak if you have a better estimate
init_infected <- max(1, round(incidence[1] / ascertainment))
prev          <- init_infected / N       # prevalence at day 1 (same scale as SIR)

# 4) Build the SIR model with LSTM params and run EXACTLY 61 days
model <- ModelSIRCONN(
  name              = "Utah replay 61d (LSTM-calibrated)",
  n                 = N,
  prevalence        = prev,
  contact_rate      = params[["crate"]],
  transmission_rate = params[["ptran"]],
  recovery_rate     = recov
)

run(model, ndays = 61)

# 5) Get daily infected incidence from the model and align to the 61 dates
inc_df <- plot_incidence(model, plot = FALSE)

# Be robust to column naming across epiworldR versions
infected_col <- intersect(names(inc_df), c("Infected", "infected", "Infected.Incidence", "infected_incidence"))
if (length(infected_col) == 0L) stop("Couldn't find 'infected' incidence column in plot_incidence().")

sim_61 <- tibble::tibble(
  Date     = seg$Date,
  infected = as.numeric(inc_df[[infected_col[1]]])[seq_len(61)]
)

# 6) Overlay plot: Observed raw cases (blue) vs LSTM-driven SIR incidence (red)
ggplot() +
  # observed (blue) as a line
  geom_line(data = seg, aes(Date, Daily.Cases), color = "blue", linewidth = 1) +
  geom_point(data = seg, aes(Date, Daily.Cases), color = "blue", size = 1) +
  # model incidence (red) as a line
  geom_line(data = sim_61, aes(Date, infected), color = "red", linewidth = 1.1) +
  labs(
    title = "Utah COVID-19: 61-day replay — observed (blue) vs SIR infected incidence (red)",
    x = NULL, y = "Counts per day"
  ) +
  theme_minimal()
