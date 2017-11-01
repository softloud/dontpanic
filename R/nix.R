#' Nix.
#'
#' rm(list = ls(all = TRUE))
#'
#' @export

nix <- function() {
  rm(list = ls(all.names = TRUE))
}
