#' Get Sample Summary Statistics
#'
#' Returns a random sample with that sample's summary statistics presented.
#'
#' @param n Sample size.
#' @param dist A baseR distribution name, e.g. "lnorm".
#' @param par A list of parameters.
#'
#' @export

sample_summary <- function(n, dist, par) {
  this_sample <- get_sample(n, dist, par = par)
  return(
    list(
      sample = this_sample, # This way the results can always be confirmed.
      summary = tibble::tibble(
        mean = this_sample %>% mean(),
        median = this_sample %>% median(),
        min = this_sample %>% min(),
        max = this_sample %>% max(),
        iqr = this_sample %>% IQR(),
        range = max(this_sample) - min(this_sample),
        sd = this_sample %>% sd(),
        var = this_sample %>% var()
      ))
    )
}
