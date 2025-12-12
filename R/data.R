#' Utah COVID-19 Data
#'
#' A dataset containing COVID-19 trends and epidemic data for Utah,
#' downloaded from the Utah Coronavirus Dashboard.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{Date}{Date of observation (Date class)}
#'   \item{Daily.Cases}{Number of daily COVID-19 cases (numeric)}
#'   \item{...}{Additional columns from the Utah COVID-19 trends data}
#' }
#'
#' @source Utah Coronavirus Dashboard
#'   \url{https://coronavirus-dashboard.utah.gov/}
#'
#' @details
#' This dataset is extracted from the "Trends_Epidemic" file within the
#' Utah COVID-19 data ZIP archive. The data includes the most recent
#' period of observations as specified during data preparation.
#'
#' The data is processed to:
#' \itemize{
#'   \item Convert dates to Date class
#'   \item Filter to recent observations
#'   \item Arrange chronologically by date
#' }
#'
#' @examples
#' \dontrun{
#' # Load the data
#' data(utah_covid_data)
#'
#' # View structure
#' str(utah_covid_data)
#'
#' # Plot daily cases
#' plot(utah_covid_data$Date, utah_covid_data$Daily.Cases, type = "l")
#' }
"utah_covid_data"

#' ABC Calibration Results for Utah COVID-19 Data
#'
#' Pre-computed ABC calibration results using 3000 MCMC samples with 1500 burn-in
#' on Utah COVID-19 incidence data. These results are used in the vignettes to
#' avoid computationally intensive calibration during package building.
#'
#' @format A list with 17 elements:
#' \describe{
#'   \item{abc_crate}{Calibrated contact rate (median)}
#'   \item{abc_recov}{Calibrated recovery rate (median)}
#'   \item{abc_ptran}{Calibrated transmission probability (median)}
#'   \item{abc_R0}{Calculated basic reproduction number}
#'   \item{abc_lower}{2.5\% quantiles for all parameters}
#'   \item{abc_upper}{97.5\% quantiles for all parameters}
#'   \item{abc_median}{Median values for all parameters}
#'   \item{post_burnin}{Matrix of posterior samples after burn-in (1500 x 3)}
#'   \item{abc_time_elapsed}{Computation time in seconds}
#'   \item{n_samples}{Number of MCMC samples (3000)}
#'   \item{burnin}{Number of burn-in samples (1500)}
#'   \item{epsilon}{ABC tolerance parameter}
#'   \item{seed}{Random seed used (122)}
#'   \item{N}{Population size (30000)}
#'   \item{recov}{Recovery rate (1/7)}
#'   \item{initial_prevalence}{Initial disease prevalence}
#'   \item{model_ndays}{Simulation days (60)}
#' }
#'
#' @details
#' This data object contains pre-computed results from an Approximate Bayesian
#' Computation (ABC) calibration procedure applied to Utah COVID-19 incidence data.
#' The calibration uses a Likelihood-Free Markov Chain Monte Carlo (LFMCMC)
#' approach to estimate SIR model parameters.
#'
#' @source Generated using ABC-MCMC calibration on Utah COVID-19 data via
#'   \code{data-raw/abc_calibration_results.R}
#'
#' @examples
#' \dontrun{
#' # Load the ABC results
#' data(abc_calibration_results)
#'
#' # View calibrated parameters
#' abc_calibration_results$abc_crate
#' abc_calibration_results$abc_ptran
#' abc_calibration_results$abc_R0
#'
#' # View posterior distribution
#' hist(abc_calibration_results$post_burnin[, 1],
#'      main = "Posterior Distribution of Contact Rate",
#'      xlab = "Contact Rate")
#' }
"abc_calibration_results"
