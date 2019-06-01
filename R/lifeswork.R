#' lifeswork
#'
#' These functions use [measures_gs_key] to access googlesheets, and produce my productivity tools.
#'
#'
#' The idea is to access gs as little as possible. Multiple things are generated from each data set. So I think I'll need to do this as methods.
#'
#' @param sim A simulation object produced by [metasims].
#' @name lifeswork
NULL

#' Ask google sheet for measures.
#'
#' @rdname lifeswork
#' @export

ask_gs <- function(for_ws) {
  # register gs by key
  measures <- gs_key(measures_gs_key) %>%
    gs_read(ws = for_ws)
}


# extract -----------------------------------------------------------------



#' @rdname lifeswork
#' @export

get_daily_tasks <- function() {
  tasks <- ask_gs(for_ws = "daily_tasks")
  class(tasks) <- c("daily_tasks", "data.frame")

  return(tasks)
}

#' @rdname lifeswork
#' @export

get_workload_key <- function() {
  ask_gs(for_ws = "workload_key")
}

#' @rdname lifeswork
#' @export

get_signifiers <- function() {
  ask_gs(for_ws = "signifiers")
}

# wrangling ---------------------------------------------------------------

#' Wrangling functions
#'
#' Keeping this separate as I tweak the wrangling so often.
#' @rdname lifeswork
#' @export

wrangle <- function(tasks) {
  UseMethod("wrangle")
}

#' @rdname lifeswork
#' @export

wrangle.default <- function(tasks) {
  "this function wants a daily_tasks object as input"
}

#' Wrangle daily tasks
#' @rdname lifeswork
#' @export

wrangle.daily_tasks <- function(tasks) {
  tasks %>%
    dplyr::mutate(
      priority = purrr::map_chr(priority, knittify_latex, "\\"),
      context = purrr::map_chr(context, knittify_latex, "\\"),
      category = purrr::map_chr(category, knittify_latex, "\\")
    )

}


# tables ------------------------------------------------------------------

#' Table layout for lifeswork functions.
#' @rdname lifesworks
#' @export

lifeswork_table <- function(df) {
  df %>%
    kableExtra::kable() %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}


