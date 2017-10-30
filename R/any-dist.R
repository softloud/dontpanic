#' Quantile, denisty, or probatility for an arbitrary distribution.
#' 
#' Returns a quantile, probability, or density for given distribution.
#'
#' @param x A numeric quantile or probability.
#' @param dist A distribution in baseR speak, e.g. "lnorm".
#' @param par A `list` comprising parameters for the distribution.
#' @param type "q" for quantile, "d" for density, "p" for probability. 
#' Defaults to "q".
#' 
#' @export


any_dist <-
  function(x, 
           dist, 
           par = list(mean = 0, sd = 1),
           type = "q") {
    
    return(eval(parse(text = paste0(
      paste0(
        type,
        dist,
        "(",
        "c(",
        paste(x, collapse = ","),
        ")",
        ",",
        paste(par, collapse = ","),
        ")"
      )
    ))))
  }