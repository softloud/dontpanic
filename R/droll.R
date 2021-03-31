#' Roll dnd dice
#'
#' @param d Number of sides of dice.
#' @param n Number of dice to roll. Defaults to 1.
#'
#' @export

droll <- function(d, n = 1) {
  sample(1:d, n, replace = TRUE)
}

