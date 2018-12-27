#' Randomly assign an exercise from statistical rethinking
#'
#' @param e Vector of exercises, i.e., c(1, 2, 4, 5)
#'
#' @export

today_rethinking_ex <- function(e, m, h) {

  # choose difficulty
  ex <- sample(c("e", "m", "h"), 1)

  # message
  message <- "Try exercise"
  end_message <- "today: either complete or try until end of pom."

  if (ex == "e") cat(paste(message, toupper(ex), sample(e, 1), end_message))
  else if (ex == "m") cat(paste(message, toupper(ex), sample(m, 1), end_message))
  else if (ex == "h") cat(paste(message, toupper(ex), sample(h, 1), end_message))
}
