#' Variance of Cohen's d function
#' 
#' Calculates the variance of Cohen's d (standardised mean difference).
#' 
#' @param d Cohen's d.
#' @param n1 First arm's n.
#' @param n2 Second arm's n.
#' 
#' @export

d_var <- function(d, n1, n2) {
  return(
    (n1 + n2 ) / (n1 * n2) +
      d^2 / (2 * (n1 + n2))
      )
}