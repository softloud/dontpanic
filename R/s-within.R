#' S_within function
#' 
#' Calculates the s_within for Cohen's d.
#' 
#' @param s1 First arm's sd.
#' @param s2 Other arm's sd.
#' @param n1 First arm's n.
#' @param n2 Second arm's n.
#' 
#' @export

s_within <- function(s1, s2, n1, n2) {
  return(
    sqrt(
      (
        (n1 - 1) * s1^2 + (n2 - 1) * s2^2
      ) / 
        (
          n1 + n2 - 2
        )
    )
  )
}