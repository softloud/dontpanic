#' Format date for humans
#'
#' Return a nicely formatted title date for RMarkdown presentations.
#'
#' @param this_date Optional argument to set date with a `date` object.
#' Otherwise defaults to current date.
#'
#' @export
#'
#' @examples
#' # title_date()
#' # title_date("2017-11-04")

title_date <- function(this_date = lubridate::today()) {
    paste(
    this_date %>% lubridate::wday(label = T, abbr = F),
    this_date %>% lubridate::day(),
    this_date %>% lubridate::month(label = T, abbr = F),
    this_date %>% lubridate::year()
  )
}
