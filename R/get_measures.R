#' Get data
#'
#' This function gathers up my productivity data for `measured.` blog and `life's work`.
#'
#' If it doesn't run, try [these instructions](https://github.com/jennybc/googlesheets/issues/343).
#' @param atracker_file An irritatingly manual process at the moment.
#'
#' @export

get_measures <-
  function(gs_title = "measures", update = FALSE, delay = 7) {
    # import time tracker data (exported manually from iphone app atracker)

    # import measures (googlesheet where I track things manually)
    measures_gs <- googlesheets::gs_title("measures")

    measures <- measures_gs %>% gs_read_all(delay = delay)

    if (isTRUE(update) & !is.null(measures)) {
      cowsay::say("Updating dontpanic:: measures dataset.", colour = "green")
      usethis::use_data(measures, overwrite = TRUE)
    }

    return(measures)

  }
