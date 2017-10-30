#' LookALook function
#' 
#' Quick view of the head of a tibble.
#' 
#' @param df A dataframe or tibble.
#' @param n How many rows to display.
#' 
#' @export

look_a_look <- function(df, n = 3) {
  df %>% head(n) %>% View()
}