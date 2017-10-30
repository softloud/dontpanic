#' Generic mean function
#' 
#' Calculates the mean for an arbitrary distribution.
#' 
#' @param dist A baseR distribution string (e.g., "lnorm").
#' @param par A list of parameters.
#'   
#' @export

get_mean <- function(dist, par){
  
  mu.f <- function(x) {
    return(
      eval(parse(
        text = paste0(
          x,
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
    )
  }
  mu.f <- Vectorize(mu.f)
  
  # mu.f <- function(x, ...){
  #   x*d(x, ...)
  # }
  # 
  mu <- integrate(mu.f, lower = -Inf, upper = Inf, subdivisions = 1000L)$value
  
  return(mu)
}