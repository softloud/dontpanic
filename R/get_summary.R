#' Get Sample Summary Statistics
#'
#' Generates a random sample and returns a summary statistics tibble from that
#' sample.
#'
#' @param n Sample size.
#' @param dist A baseR distribution name, e.g. "lnorm".
#' @param par A list of parameters.
#'
#' @export

get_summary <- function(n, dist, par) {
  this_sample <- get_sample(n, dist, par)
  return(tibble(
    mean = this_sample %>% mean(),
    median = this_sample %>% median(),
    min = this_sample %>% min(),
    max = this_sample %>% max(),
    iqr = this_sample %>% IQR(),
    range = max(this_sample) - min(this_sample),
    sd = this_sample %>% sd(),
    var = this_sample %>% var()
  ))
}
