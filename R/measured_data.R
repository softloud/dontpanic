#' Get data
#'
#' @param atracker_file An irritatingly manual process at the moment.
#'
#' This function gathers up my productivity data for `measured.` blog and `life's work`.
#'
#' If it doesn't run, try [these instructions](https://github.com/jennybc/googlesheets/issues/343}).


measured_data <- function(atracker_file) {
  # import time tracker data (exported manually from iphone app atracker)
  time_tracker_raw <- readr::read_csv(atracker_file)

  # import measures (googlesheet where I track things manually)
  measures <- googlesheets::gs_title("measures")

  time_tracker <- time_tracker_raw %>%
    # tidy column names
    dplyr::rename(
      task = `Task name`,
      description = `Task description`,
      start = `Start time`,
      finish = `End time`,
      duration = Duration,
      duration_hrs = `Duration in hours`,
      note = Note,
      category = Category
    ) %>%
    dplyr::mutate(
      # get date object, as I'll want to do things by day
      start = lubridate::dmy_hm(start),
      finish = lubridate::dmy_hm(finish),
      date = lubridate::ymd(start),
    )
  return(tibble::tibble(
    data = list(
      time_tracker_raw = time_tracker_raw,
      measures = measures
    ),
    description = c(
      "daily time records raw data from atracker",
      "google sheets records"
    )

  ))
}
