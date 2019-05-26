#' Visualise thesis chapter progress
#'
#' Scrapes measures-chapters worksheet for latest update.
#'
#' @export

dumpsterfire_progress <- function() {
  # https://docs.google.com/spreadsheets/d/1hv7pkBGu8XQQOIBbBt1_1LvKGBR7zTdQYCzogrv3hz0/edit#gid=777059488
  # got key using extract_key_from_url function
  # get data
  #
  # start by registering sheet by key
  measures <- gs_key("1hv7pkBGu8XQQOIBbBt1_1LvKGBR7zTdQYCzogrv3hz0")

  projects <- gs_read(measures, ws = "chapters")

  # set levels for plot
  block_levels <- c(
    "content_prep",
    "content_review",
    "content_consensus",
    "prose_prep",
    "prose_review",
    "prose_consensus",
    "final_prep",
    "final_review",
    "final_complete"
  )

  phase_levels <- c("content", "prose", "final")
  subphase_levels <- c("prep", "review", "consensus")
  project_levels  <-
    projects %>% arrange(desc(order)) %>% pluck("projects")

  plot_data <- projects %>%
    gather(
      key = stage,
      value = stage_status,
      content_prep,
      content_review,
      content_consensus,
      prose_prep,
      prose_review,
      prose_consensus,
      final_prep,
      final_review,
      final_complete
    ) %>%
    separate(
      stage,
      into = c("plot_phase", "plot_subphase"),
      sep = "_",
      remove = FALSE
    ) %>%
    mutate(
      stage = fct_relevel(stage, block_levels),
      start = (as.numeric(stage) - 1) / length(block_levels),
      end = as.numeric(stage) / length(block_levels),
      line_id = row_number()
    ) %>%
    gather(key = "terminal", value = "x", start, end) %>%
    mutate(
      plot_phase = fct_relevel(plot_phase, phase_levels),
      plot_subphase = fct_relevel(plot_subphase, subphase_levels),
      project = fct_relevel(project, project_levels),
      stage = fct_relevel(stage, block_levels)
    ) %>%
    arrange(line_id)

  plot_data %>%
    ggplot(aes(x = x, y = project)) +
    geom_line(
      aes(
        group = line_id,
        colour = status,
        alpha = stage_status
      ),
      lineend = "butt",
      show.legend = FALSE,
      size = 5
    ) +
    geom_line(
      aes(
        group = line_id,
        colour = status,
        alpha = stage_status,
        linetype = type
      ),
      size = 1
    ) +
    scale_x_continuous(
      "progress within phase",
      breaks = seq(0, 1, by = 1 / length(block_levels)),
      labels = c(rep(subphase_levels, times = 3), "")
    ) +
    theme(axis.text.x = element_text(hjust = 0, angle = -30),
          legend.position = NULL) +
    # scale_colour_grey() +
    scale_alpha_discrete(range = c(0.2, 0.8)) +
    hrbrthemes::scale_colour_ipsum() +
    facet_grid(order ~ plot_phase, scales = "free") +
    labs(
      title = "progress of pieces of writing, towards content and prose",
      caption = str_wrap(
        "Content phase aims for 30% completion or consensus on content,
         and prose phase aims for 80% completion in prose. The horizontal plot facets represent the order Charles currently intends to complete these projects. "
      )
    ) +
    ylab("pieces of writing that will contribute to the thesis")


}
