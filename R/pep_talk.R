#' How long have I been sober?
#'
#' Provides number of days, weeks, and years without an alcoholic beverage.
#'
#' @export

pep_talk <- function( ) {
  sober_since <- lubridate::ymd_hms("2021-07-23 07:22:39 UTC", tz = "UTC")

  sobriety <- round(lubridate::now(tz = "UTC") - sober_since, 1)

  print(round(sobriety, 1))

}
