#' Visualise between-study variability from a multinma model
#'
#' @param nma_mod A multinma model object.
#'
#' @export


tau <- function(nma_mod) {
  tau_vals <-
    summary(nma_mod) %>%
    as.data.frame() %>%
    dplyr::filter(parameter == "tau")

  tau_quantiles <- tau_vals %>%
    dplyr::select(4:8)

  tau_quant_labs <-
    tau_quantiles %>%
    names() %>%
    paste(collapse = ", ")

  tibble(
    x = rstan::extract(nma_mod$stanfit, pars = "tau")$tau %>% as.numeric()
  ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_density(ggplot2::aes(x = x)) +
    ggplot2::geom_vline(
      xintercept = tau_quantiles %>% t(),
      linetype = "dotted",
      alpha = 0.6
    ) +
    ggplot2::xlim(tau_vals$mean - 3*tau_vals$sd, tau_vals$mean + 3*tau_vals$sd) +
    # ggthemes::theme_tufte(
    #   base_size = 25
    # ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Between-study variation",
      subtitle = latex2exp::TeX("$\\tau^2$"),
      y = "",
      x = "",
      caption = glue::glue(
        "Dotted lines indicate quantiles:
      {tau_quant_labs}")
    )
}
