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
#' dat %>% filter(draft_progress == min(draft_progress)) %>% pluck("ms") %>% sample(3)

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
    suppressMessages(googlesheets4::read_sheet(ms_url, ms)) %>%
    janitor::clean_names()

  progress_levels <-
    dat %>%
    dplyr::select(!dplyr::contains("read"), -ms) %>%
    names()

  dat %>%
    dplyr::select(ms) %>%
    dplyr::mutate(
      draft_progress =
        dat %>%
        dplyr::select(!dplyr::contains("read"), -ms) %>%
        apply(1, sum, na.rm = TRUE),
      read_progress =
        dat %>%
        dplyr::select(dplyr::contains("read")) %>%
        apply(1, sum, na.rm = TRUE)
    ) %>%
    dplyr::select(ms, draft_progress, read_progress) %>%
    dplyr::mutate(
      draft_next_action = dplyr::case_when(
        draft_progress == length(progress_levels) &
          read_progress >= 4 ~ "completed",
        draft_progress == length(progress_levels) &
          read_progress < 4 ~ "read",
        TRUE ~ progress_levels[draft_progress + 1]
      ),
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

ms_next <-
  function(ms,
           ms_url = Sys.getenv("MS_TRACKER")) {
    # get the data
    ms_dat <-
      ms_dat(ms, ms_url)

    cat("\n***\nthese sections next\n\n")

    next_actions <-
      ms_dat %>%
      dplyr::arrange(draft_progress) %>%
      dplyr::select(ms, draft_next_action) %>%
      head(1)


    print(next_actions)

    min_reading <-
      ms_dat %>%
      dplyr::filter(read_progress == min(read_progress)) %>%
      head(1)

    reading_message <-
      dplyr::if_else(
        min_reading$read_progress >= 4,
        "\n***\nAll sections read at least 4 times.\n***\n",
        sprintf(
          "\n***\nthen read from Section %s until end of pom",
          min_reading %>% dplyr::pull(ms)
        )
      )

    cat(reading_message)

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

  dat %>%
    dplyr::mutate(
      draft_remaining = total_levels - draft_progress,
      read_remaining = dplyr::if_else(read_progress > 4, 0, 4 - read_progress)
    ) %>%
    dplyr::summarise(
      completed = sum(draft_progress),
      sections = dplyr::n(),
      progress_levels = max(total_levels),
      draft_remaining = sum(draft_remaining),
      read_remaining = sum(read_remaining)
    )

}

#' Visualise manuscript progess over time
#'
#' @param all_ms Show active manuscripts, default, or all manuscripts.
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
      ggplot2::facet_grid(
        category ~ .,
        #labeller = ggplot2::label_parsed(category_label)) +
        hrbrthemes::scale_color_ipsum() +
          ggplot2::ylim(0, 1) +
          ggthemes::theme_wsj() +
          ggplot2::labs(title = "manuscript progress over time",
                        y = "planned work completed")
      )

  }
}
