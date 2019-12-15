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
    arrange(y)

  panda_body_coord <- function(part, coord) {
    panda_body_df %>% dplyr::filter(part == !!part) %>% pluck(coord)
  }

  panda_body <- panda_body_df %>%
    ggplot(aes(x0 = x, y0 = y, r = radius)) +
    ggforce::geom_circle(fill = "white") +
    lims(x = c(0, 1),
         y = c(0, 1)) +
    theme_void()

  # eyes
  dots_data <- tibble::tibble(
    x = panda_body_coord("head", "x") +
      c(-1, 1) * panda_body_coord("head", "radius") / 2.2,
    y = panda_body_coord("head", "y") +
      panda_body_coord("head", "radius") / 6,
    radius = 0.02,
    part = "eyes"
  ) %>%
    # ears
    add_row(
      x = panda_body_coord("head", "x") +
        c(-1, 1) * (panda_body_coord("head", "radius") / 1.3),
      y = panda_body_coord("head", "y") +
        panda_body_coord("head", "radius") * 0.9,
      radius = panda_body_coord("head", "radius") / 3,
      part = "ears"
    ) # %>%
  # paws
  # add_row(
  #   x =
  # )

  panda_body +
    ggforce::geom_circle(data = dots_data,
                aes(x0 = x, y0 = y, r = radius),
                fill = "black")

}
