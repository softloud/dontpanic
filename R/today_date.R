#' Return date in language I'm trying to learn right now
#'
#' Current is French, but I might switch back to German. Depends on how the write-up is going.
#'
#' Haven't coded this for other locations yet, but can be extended later.
#'
#' @export

today_date <- function(language = "french", verbose = FALSE) {

  greeting <- "la date d'aujourd'hui est "

  current_timezone_datetime <- today(Sys.timezone())

  date_dictionary <-
    gs_key("1aswBpfjSPbjhASyO7RiGrESEU5Pqjv2AaO4Lmkj_rKs",
           verbose = verbose)

  # output date as I'd like to display it in my bujo
  suppressMessages(paste0(
    greeting,
    # day
    date_dictionary %>%
      gs_read(ws = "weekday", verbose = verbose) %>%
      dplyr::filter(numeric ==
                      current_timezone_datetime %>% wday(week_start = 1)) %>%
      pluck(language),
    ", ",
    # date
    date_dictionary %>%
      gs_read(ws = "date", verbose = verbose) %>%
      dplyr::filter(numeric ==
                      current_timezone_datetime %>% day()) %>%
      pluck(language),
    ", ",
    # month
    date_dictionary %>%
      gs_read(ws = "month", verbose = verbose) %>%
      dplyr::filter(numeric ==
                      current_timezone_datetime %>% month()) %>%
      pluck(language)

  ))

}
