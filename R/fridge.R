#' Refrigerator plot
#'
#' @family lifeswork
#'
#' @export

# fridge <- function() {}

# decade <- pom_report(10, with_date = TRUE) %>%
#   dplyr::mutate(period = "decade")
#
# century <- pom_report(100, with_date = TRUE) %>%
#   dplyr::mutate(period = "century")
#
#
# millenium <- pom_report(1000, with_date = TRUE) %>%
#   dplyr::mutate(period = "millenium")
#
# allpoms <-
#     dplyr::bind_rows(decade, century, millenium)
#
# allpoms %>%
#   ggmosaic::geom_mosaic(
#     ggplot2::aes(weight = product(intensity_cat),
#                  x = product(intensity_day),
#                  fill = intensity_cat)
#   )
