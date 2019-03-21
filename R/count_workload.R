count_workload <- function(daily_df =
                             measures %>%
                             purrr::pluck("data", "time_tracker"),
                           # name = "test",
                           days = 10){


  counts <- daily_df %>%
    dplyr::ungroup() %>%
    dplyr::filter(date > lubridate::today() - days) %>%
    dplyr::group_by(category) %>%
    dplyr::count(category_workload)

  return(
    counts
  )
}
