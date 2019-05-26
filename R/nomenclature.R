#' lifeswork nomenclature
#' @name nomenclature
NULL

#' musical notation shortcuts for output in .Rmd; use as in-line R.
#'
#' See [signifiers](https://lifes-work.netlify.com/rituals.html#signifiers).
#'
#' @describeIn nomenclature
#' @export
quaver <- function(){knitr::asis_output("&#9834;")}

#' @describeIn nomenclature
#' @export
quavers <- function(){knitr::asis_output("&#9835;")}

#' @describeIn nomenclature
#' @export
fermata <- function(){knitr::asis_output("&#119056;")}
