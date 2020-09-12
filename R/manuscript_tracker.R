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
#' dat %>% filter(progress_achieved == min(progress_achieved)) %>% pluck("ms") %>% sample(3)

#' Get manuscript tracker data
#'
#' @param ms manuscript and name of sheet
#' @param ms_url googlesheet of manuscript trackers
#'
#' @export
#'
#' @family manuscript trackers

ms_dat <- function(ms, ms_url = Sys.getenv("MS_TRACKER")) {
  dat <-
    googlesheets4::read_sheet(ms_url, ms) %>%
    janitor::clean_names()

  progress_levels <-
    names(dat)[2:ncol(dat)]

  dat %>%
    dplyr::mutate(progress_achieved = dat %>%
                    dplyr::select(-1) %>%
                    apply(1, sum, na.rm = TRUE)) %>%
    dplyr::select(ms, progress_achieved) %>%
    dplyr::mutate(
      next_action = dplyr::if_else(
        progress_achieved == length(progress_levels),
        "completed",
        progress_levels[progress_achieved + 1]
      ),
      status = dplyr::if_else(stringr::str_detect(next_action, "read"),
                              "edit",
                              "draft"),
      total_levels = length(progress_levels)
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
    # get the data
    ms_dat <-
      ms_dat(ms, ms_url)

    ms_dat %>%
      dplyr::arrange(progress_achieved) %>%
      head(3)
  }

#' Next action
#'
#' @inheritParams ms_three
#'
#' @export

ms_next <- function(ms, ...) {
  ms_three(ms, ...) %>%
    head(1)
}

#' Manuscript summary
#'
#' @inheritParams ms_dat
#'
#' @family manuscript trackers
#'
#' @export

ms_report <- function(ms, ms_url = Sys.getenv("MS_TRACKER")) {
  dat <- ms_dat(ms, ms_url)

  tibble::tibble(
    completed = sum(dat$progress_achieved, na.rm = TRUE),
    sections = nrow(dat),
    progress_levels = dat$total_levels %>% unique()
  )   %>%
    dplyr::mutate(
      draft_remaining =
        dat %>%
        dplyr::filter(status == "draft") %>%
        dplyr::mutate(remaining = total_levels - progress_achieved) %>%
        dplyr::pull(remaining) %>%
        sum()
      ,
      to_complete  = sum(dat$total_levels - dat$progress_achieved)
    )


}

#' Visualise manuscript progess over time
#'
#' @all_ms Show active manuscripts, default, or all manuscripts.
#' @inheritParams ms_dat
#'
#' @export

ms_vis <- function(all_ms = FALSE,
                   ms_url = Sys.getenv("MS_TRACKER")) {
  completedness <-
    googlesheets4::read_sheet(ms_url, "completedness") %>%
    janitor::clean_names()

  categories <-
    googlesheets4::read_sheet(ms_url, "dataset_key") %>%
    janitor::clean_names()  %>%
    dplyr::mutate(category_label = paste0("$\\", category, "$"))

  all_ms <-
    completedness %>%
    dplyr::left_join(categories, by = "manuscript")

  if (isTRUE(all_ms)) {
    print("haven't coded this yet")
  } else {

      all_ms %>%
      dplyr::filter(is.na(status)) %>%
      dplyr::group_by(manuscript) %>%
      ggplot2::ggplot(ggplot2::aes(
        x = date,
        y = completed / (sections * progress_levels),
        colour = manuscript
      )) +
      ggplot2::geom_line(alpha = 0.3) +
      ggplot2::geom_point(alpha = 0.6,
                          size = 5) +
      ggplot2::facet_grid(category ~ .,
        #labeller = ggplot2::label_parsed(category_label)
                          ) +
      hrbrthemes::scale_color_ipsum() +
      ggplot2::ylim(0,1) +
      ggthemes::theme_wsj() +
      ggplot2::labs(title = "manuscript progress over time",
                    y = "planned work completed")

      }
}
