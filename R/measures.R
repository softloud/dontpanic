#' Measured data
#'
#' Raw datasets scraped from googlesheets and imported from atracker iphone app.
#'
#' @format A list of datasets.
#' \describe{
#' 	 \item{time_tracker_raw}{daily time records raw data from atracker}
#' 	 \item{time_tracker}{tidied column names and date objects}
#' 	 \item{measures}{google sheets records}
#' 	 \item{workload_key}{current pom goals}
#' 	 \item{operations}{phases of work}
#' 	 \item{daily}{manual daily records tracked in a googlesheet}
#' 	 \item{signifiers}{symbol signfiers for bullet journalling and visualisations}
#' 	 \item{daily_tasks}{tasks to be completed each day}
#' }

"measures"
