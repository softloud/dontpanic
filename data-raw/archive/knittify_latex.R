#' knittify latex col
#'
#' Bind value in latex $$ for output with kable() in bookdown.
#'
#' @export

knittify_latex <- function(x, ...) {
  paste0("$\\", ..., x, "$", collapse = "") %>%
    plotly::TeX()
}
