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
