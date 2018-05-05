#' Get summary statistics of a given sample
#'
#' Summary statistics of a given sample vector returned as a tibble.
#'
#' @param sample A numeric vector.
#'
#' @export

get_summary <- function(sample) {
  tibble::tibble(
    mean = sample %>% mean(),
    median = sample %>% median(),
    min = sample %>% min(),
    max = sample %>% max(),
    iqr = sample %>% IQR(),
    range = max(sample) - min(sample),
    sd = sample %>% sd(),
    var = sample %>% var())
}
