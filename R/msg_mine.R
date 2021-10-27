#' Charles' custom messages
#'
#' Don't forget to preface with dontpanic:: so can keep track of where it comes from.
#'
#' @param msg Message.
#'
#' @return
#' @export
#'
#' @examples

msg <- function(msg) {
  message(crayon::blue(glue::glue("--] {msg}")))
}
