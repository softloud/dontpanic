intensity_rank <- function(rank) {
  dplyr::case_when(
    rank == 0 ~ "sweet fuck all",
    rank == 1 ~ "eh, got something done",
    rank == 2 ~ "pretty good effort",
    rank == 3 ~ "adequate",
    rank == 4 ~ "eaglescream"
  )
}

#' @export

pom_today <- function() {
  rawpoms <-
    suppressWarnings(
      togglr::get_time_entries(
        api_token = Sys.getenv("TOGGL"),
        since = lubridate::today() - lubridate::hours(8)
      )
    )

  rawpoms %>%
    dplyr::rename(category = project_name) %>%
    dplyr::select(category, duration) %>%
    dplyr::filter(category %in% c("phi", "psi", "theta")) %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(poms = sum(duration) / 60 / 20,
                     poms = round(poms, 1))

}



#' Retrieve poms for a given period
#'
#' @family lifeswork
#'
#' @export


pom_report <- function(time_period = 1, terse = TRUE, with_date = FALSE) {

  since_date <- lubridate::today() - time_period

  rawpoms <-
    suppressWarnings(
      togglr::get_time_entries(
        api_token = Sys.getenv("TOGGL"),
        since = since_date - lubridate::hours(8)
      )
    )

  cleanpoms <-
    rawpoms %>%
    dplyr::rename(category = project_name, date = start) %>%
    dplyr::filter(category %in% c("phi", "theta", "psi")) %>%
    dplyr::mutate(date = date - lubridate::hours(8),
                  date = lubridate::date(date)) %>%
    dplyr::group_by(category, date) %>%
    dplyr::summarise(seconds = sum(duration))

  # missing days
  missing <-
    tibble::tibble(
      date = seq(lubridate::date(since_date),
                 lubridate::today(), by = "day")
    ) %>%
    dplyr::mutate(phi = "phi", theta = "theta", psi = "psi") %>%
    tidyr::gather(key = "category", value = "value", phi, theta, psi) %>%
    dplyr::select(-value) %>%
    dplyr::mutate(seconds = 0) %>%
    dplyr::anti_join(cleanpoms, by = c("date", "category"))

  allpoms <-
  dplyr::bind_rows(cleanpoms, missing) %>%
    dplyr::mutate(
      poms = (seconds / 60) / 20,
      poms = round(poms, 1)
    ) %>%
    dplyr::select(-seconds) %>%
    dplyr::mutate(
      intensity_cat = dplyr::case_when(
        poms == 0 ~  0,
        poms < 1 ~ 1,
        poms  < 2 ~ 2,
        poms  < 3 ~ 3,
        poms  > 3 ~ 4
      )
    )

    dayrank <-
      allpoms %>%
      dplyr::select(date, category, intensity_cat) %>%
      tidyr::spread(category, intensity_cat) %>%
      dplyr::mutate(
        intensity_day = purrr::pmap_dbl(list(phi, theta, psi), min)
      ) %>%
      dplyr::select(date, intensity_day) %>%
      dplyr::mutate(intensity_day = intensity_rank(intensity_day))

    output <-
      allpoms %>%
      dplyr::full_join(dayrank) %>%
      dplyr::mutate(
        intensity_cat = intensity_rank(intensity_cat),
        day = lubridate::wday(date, label = TRUE, abbr = FALSE),
        month = lubridate::month(date, label = TRUE, abbr = FALSE),
        year = lubridate::year(date)
      ) %>%
      dplyr::relocate(date, .after = dplyr::last_col()) %>%
      dplyr::arrange(date, category)

  if (with_date) output else output %>% dplyr::select(-date)

}
