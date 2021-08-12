#' How long have I been sober?
#'
#' Provides number of days, weeks, and years without an alcoholic beverage.
#'
#' @export

pep_talk <- function( ) {
  sober_since <- lubridate::ymd_hms("2021-07-23 07:22:39 UTC", tz = "UTC")

  sobriety <- lubridate::now(tz = "UTC") - sober_since

  print(round(sobriety, 2))
  cat('Specifically: ')
  cat(as.character(round(lubridate::as.period(sobriety), 1)))
  cat("\n***\nCurrent strategy:\n1) Max two drinks per night.\n2) No drinking on consecutive nights.")


}
