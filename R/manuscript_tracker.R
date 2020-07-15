#' #'
#' #'
#' #'
#' #'
#'
#' gs_url <- "https://docs.google.com/spreadsheets/d/1sWRaWCw-grpf3AFktsRahFb03z37J0JE7mZxDp34FxM/edit#gid=1892813229"
#'
#' gs_metadat <-
#'   gs4_get()
#'
#' gs_metadat %>%
#'   pluck("sheets")
#'
#' dat %>% filter(total == min(total)) %>% pluck("ms") %>% sample(3)

#' Get manuscript tracker data
#'
#' @param ms manuscript and name of sheet
#' @param ms_url googlesheet of manuscript trackers
#'
#' @export
#'
#' @family manuscript trackers

ms_dat <- function(ms, ms_url = Sys.getenv("MS_TRACKER")) {
  googlesheets4::read_sheet(ms_url, ms) %>% janitor::clean_names()
}

#' Progress converter
#'
#' @param progress A number from 1 to 8.
#'
#' @family manuscript trackers
#'
#' @export

ms_progress <- function(progress) {
  dplyr::case_when(
    progress == 1 ~ "heading",
    progress == 2 ~ "sentence",
    progress == 3 ~ "paragraph",
    progress == 4 ~ "section",
    progress > 4 &
      progress < 8 ~ paste('read', progress - 4, collapse = " "),
    TRUE ~ "progress is a number in 1:8"
  )
}


#' Get next three dots
#'
#' @inheritParams ms_dat
#'
#' @family manuscript trackers
#'
#' @export

ms_three <-
  function(ms,
           ms_url = Sys.getenv("MS_TRACKER"),
           draft = TRUE) {
    ms_dat <-
      ms_dat(ms, ms_url)

    key <- ms_dat("progress_key", ms_url)

    stage_levels <- if (isTRUE(draft)) {
      "draft"
    } else {
      c("draft", "edit")
    }

    ms_dat %>%
      dplyr::mutate(total = dplyr::if_else(is.na(total), 1, total + 1)) %>%
      dplyr::left_join(key, by = c("total" = "progress")) %>%
      dplyr::filter(stage %in% stage_levels) %>%
      dplyr::arrange(total) %>%
      dplyr::select(ms, next_action = label) %>%
      head(3)

  }

#' Manuscript summary
#'
#' @inheritParams ms_dat
#'
#' @family manuscript trackers
#'
#' @export

ms_report <- function(ms, ms_url = Sys.getenv("MS_TRACKER")) {
  dat <-  ms_dat(ms, ms_url)

  tibble::tibble(completed = sum(dat$total, na.rm = TRUE),
                 sections = nrow(dat)
) %>%
    dplyr::mutate(
      draft_remaining = sections * 4 - completed,
      draft_remaining = dplyr::if_else(draft_remaining < 0, 0, draft_remaining),
      to_complete  = sections * 8
    )


}

#' Visualise manuscript progess over time
#'
#' @inheritParams ms_dat
#'
#' @export

ms_vis <- function(ms_url = Sys.getenv("MS_TRACKER")){
  plot_dat <- ms_dat("completedness", ms_url)

  plot_dat %>%
    dplyr::group_by(manuscript) %>%
    dplyr::mutate(shape = round(completed / (sections * 4)),
                  shape = dplyr::if_else(shape <= 4, "draft", paste("read", shape - 3)),
                  p = completed / (sections * 4),
                  p = dplyr::if_else(p > 1, 1, p)) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = p, colour = manuscript)) +
    ggplot2::geom_line(alpha = 0.3) +
    ggplot2::geom_point(alpha = 0.6, size = 5, ggplot2::aes(shape = shape)) +
    hrbrthemes::scale_color_ipsum() +
    ggthemes::theme_wsj() +
    ggplot2::labs(title = "manuscript progress over time",
                  y = "proportion of draft steps") #+
    #ggplot2::theme(axis.title = ggplot2::element_text(size = 12))
}
