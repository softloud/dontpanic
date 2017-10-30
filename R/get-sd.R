#' Generic sd function
#' 
#' Calculates the standard deviation for an arbitrary distribution.
#' 
#' @param dist A baseR distribution string (e.g., "lnorm").
#' @param par A list of parameters.
#'   
#' @export

get_sd <- function(dist, par, this.mean) {
  this.f <- function(x) {
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
  this.f <- Vectorize(this.f)
  
  e.of.x.sq <-
    integrate(
      this.f,
      lower = -Inf,
      upper = Inf,
      subdivisions = 10000L
    )$value
  
  return(sqrt(e.of.x.sq - this.mean ^ 2))
}