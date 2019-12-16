#' Make a panda
#'
#' @param seed Set a random seed to reproduce a panda.
#' @param msg The panda can speak your words.
#'
#' @export

panda <- function(seed = "random", msg = NULL) {

  # seed
  if (seed == "random") {
    panda_seed <- seq(1, 100) %>% sample(1)
  } else {
    panda_seed <- seed
  }

  set.seed(panda_seed)

  # panda body coords
  panda_body_df <- tibble::tibble(
    # x = c( 0.5, 0.6),
    x = rnorm(2, 0.5, 0.05),
    # y = c(0.55, 0.23),
    y = rnorm(2, c(0.5, 0.2), 0.02),
    radius = c(0.25, 0.15),
    part = c("head", "body")
  ) %>%
    dplyr::arrange(y)

  panda_body_coord <- function(part, coord) {
    panda_body_df %>% dplyr::filter(part == !!part) %>% purrr::pluck(coord)
  }

  ears <-     # ears
    tibble(
      x = panda_body_coord("head", "x") +
        c(-1, 1) * (panda_body_coord("head", "radius") * 0.9),
      y = panda_body_coord("head", "y") +
        panda_body_coord("head", "radius") * 0.6 +
        rnorm(2, mean = 0, sd = panda_body_coord("head", "radius") * 0.1),
      radius = panda_body_coord("head", "radius") * 0.5,
      part = "ears"
    )

  panda_ears <- ears %>%
    ggplot2::ggplot(aes(x0 = x, y0 = y, r = radius)) +
    ggforce::geom_circle(fill = "grey", colour = "grey")

  panda_body <- panda_ears +
    ggforce::geom_circle(fill = "white",
                         colour = "grey",
                         data = panda_body_df)



  # eyes
  dots_data <- tibble::tibble(
    x = panda_body_coord("head", "x") +
      c(-1, 1) * panda_body_coord("head", "radius") / 2.2,
    y = panda_body_coord("head", "y") +
      panda_body_coord("head", "radius") * 0.1,
    radius = 0.02,
    part = "eyes"
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

  panda_plot <- panda_body +
    ggforce::geom_circle(data = dots_data,
                         aes(x0 = x, y0 = y, r = radius),
                         fill = "grey", colour = "grey")

  # add text
  add_text <- if (is.null(msg)) {
    panda_plot
  } else {
    panda_plot +
      annotate("text", x =  panda_body_coord("head", "x") +
                 (panda_body_coord("head", "radius") * 2.3),
               y = panda_body_coord("head", "y"),
               colour = "darkgrey",
               label = stringr::str_wrap(msg, width = 25)) +
      xlim(c(0, 1.3))
  }

  output_plot <- add_text +
    ggplot2::theme_void()

  # output seed
  cat(paste("seed that generated this panda:", panda_seed))
  output_plot

}
