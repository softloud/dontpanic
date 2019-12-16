#' Make a panda
#'
#' @export

panda_plot <- function() {
  panda_body_df <- tibble::tibble(
    x = c( 0.5, 0.6),
    y = c(0.55, 0.23),
    radius = c(0.25, 0.15),
    part = c("head", "body")
  ) %>%
    dplyr::arrange(y)

  panda_body_coord <- function(part, coord) {
    panda_body_df %>% dplyr::filter(part == !!part) %>% purrr::pluck(coord)
  }

  panda_body <- panda_body_df %>%
    ggplot2::ggplot(aes(x0 = x, y0 = y, r = radius)) +
    ggforce::geom_circle(fill = "white") +
    ggplot2::lims(x = c(0, 1),
                  y = c(0, 1)) +
    ggplot2::theme_void()

  # eyes
  dots_data <- tibble::tibble(
    x = panda_body_coord("head", "x") +
      c(-1, 1) * panda_body_coord("head", "radius") / 2.2,
    y = panda_body_coord("head", "y") +
      panda_body_coord("head", "radius") * 0.1,
    radius = 0.02,
    part = "eyes"
  ) %>%
    # ears
    dplyr::add_row(
      x = panda_body_coord("head", "x") +
        c(-1, 1) * (panda_body_coord("head", "radius") * 0.75),
      y = panda_body_coord("head", "y") +
        panda_body_coord("head", "radius") * 0.9 +
        rnorm(2, mean = 0, sd = panda_body_coord("head", "radius") * 0.1),
      radius = panda_body_coord("head", "radius") * 0.4,
      part = "ears"
    ) %>%
    # paws
    dplyr::add_row(
      x = panda_body_coord("body", "x") +
        c(-1, 1) * panda_body_coord("body", "radius") * 0.9 +
        rnorm(2,sd = panda_body_coord("body", "radius") * 0.2),
      y = panda_body_coord("body", "y") +
        panda_body_coord("body", "radius") * 0.1 +
        rnorm(2,sd = panda_body_coord("body", "radius") * 0.2),
      radius = panda_body_coord("head", "radius") * 0.2,
      part = "front_paws"
    ) %>%
    dplyr::add_row(
      x = panda_body_coord("body", "x") +
        c(-1, 1) * panda_body_coord("body", "radius") * 0.9 +
        rnorm(2,sd = panda_body_coord("body", "radius") * 0.1),
      y = panda_body_coord("body", "y") -
        panda_body_coord("body", "radius") * 0.9 +
        rnorm(2,sd = panda_body_coord("body", "radius") * 0.1),
      radius = panda_body_coord("head", "radius") * 0.2,
      part = "back_paws"
    )
  panda_body +
    ggforce::geom_circle(data = dots_data,
                         aes(x0 = x, y0 = y, r = radius),
                         fill = "black")

}
