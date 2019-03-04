#' wrangle measured. data
#'
#' @param df \code{\link{measured_data}} output

measured_wrangle <- function(mdat) {
  daily_work <- mdat %>%
    purrr::pluck("data", "time_tracker") %>%
    # no analysis yet on these
    dplyr::filter(
      category == "phi" | category == "theta" | category == "psi") %>%
    select(-note, -description) %>%
    group_by(date, category) %>%
    summarise(hrs = sum(duration_hrs))

  decade <- daily_work %>%
    filter(date > lubridate::today() - 10) %>%


  century <- daily_work %>%
    filter(date > lubridate::today() - 100)


  return(decade = decade, century = century, all = daily_work)
}
