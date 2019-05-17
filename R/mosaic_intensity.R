#' Mosaic plot of workload
#'
#' A mosaicplot of intensity achieved by category and day.
#'
#' @param days Number of days' data to display.
#' @param df Dataframe of measures.' data.
#'
#' @export

mosaic_intensity <- function(days = 100000,
                             df = dontpanic::time_tracker,
                             angle = 35,
                             # vjust = 1,
                             hjust = 1, ...) {
  df %>%
  ggplot()+
    ggmosaic::geom_mosaic(
      aes(x = product(daily_workload),
          fill = category_workload,
          weight = n)) +
    scale_fill_grey(start = 0.8, end = 0.2) +
    theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text.x = element_text(angle = 35, hjust = 1))+
    facet_grid(period ~ category, labeller = label_parsed)

   #
   # df %>%
   #  # purrr::pluck("all") %>%
   #  dplyr::filter(date > lubridate::today() - days) %>%
   #  dplyr::group_by(category_workload, daily_workload) %>%
   #  dplyr::count() %>%
   #  ggplot2::ggplot()+
   #  ggmosaic::geom_mosaic(
   #    aes(x = product(daily_workload),
   #        fill = category_workload,
   #        weight = n)) +
   #  scale_fill_grey() +
   #  ggplot2::theme(axis.text.y = element_blank(),
   #        axis.ticks.y = element_blank(),
   #        axis.text.x = element_text(angle = 35, hjust = hjust, ...))

}

#' Wrangle daily category data into something mosaicish.
#'
#'
