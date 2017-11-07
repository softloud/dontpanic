#' Generic Sd Function
#'
#' Calculates the standard deviation for an arbitrary distribution.
#'
#' @param dist A baseR distribution string (e.g., "lnorm").
#' @param par A list of parameters.
#'
#' @export

get_sd <- function(dist, par, this_mean) {
  this_f <- function(x) {
    eval(parse(
      text = paste0(
        x ^ 2,
        "*",
        "d",
        dist,
        "(",
        x,
        ",",
        paste(par, collapse = ","),
        ")"
      )
    ))
  }
  this_f <- Vectorize(this_f)

  EXsq <-
    integrate(
      this_f,
      lower = -Inf,
      upper = Inf,
      subdivisions = 10000L
    )$value

  return(sqrt(EXsq - this_mean ^ 2))
}
