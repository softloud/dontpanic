#' Sample function
#'
#' Generate a sample from an arbitrary distribution
#'
#' @param n Sample size.
#' @param dist A baseR distribution name, e.g. "lnorm".
#' @param par A list of parameters.
#'
#' @export

get_sample <- function(n, dist, par) {
  # Returns a sample for a given distribution set.
  #
  # Arguments:
  #   n : sample size.
  #   dist: a distribution in baseR speak, e.g. "lnorm".
  #   par : a list comprising parameters for distribution.
  #
  # Returns:
  #   A vector sample.

  return(eval(parse(
    text =  paste0(
      "r",
      dist,
      "(",
      n,
      ",",
      paste(par, collapse = ","),
      ")"
    )
  )))
}
