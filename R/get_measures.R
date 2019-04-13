#' Get data
#'
#' This function gathers up my productivity data for `measured.` blog and `life's work`.
#'
#' If it doesn't run, try [these instructions](https://github.com/jennybc/googlesheets/issues/343).
#' @param atracker_file An irritatingly manual process at the moment.
#'
#' @export

get_measures <-
  function() {
    # import time tracker data (exported manually from iphone app atracker)

    # import measures (googlesheet where I track things manually)
    measures <- googlesheets::gs_title("measures")

    Sys.sleep(2)

    dataset_key <- measures %>%
      googlesheets::gs_read(ws = "dataset_key")
    Sys.sleep(2)

    # extract daily record
    daily <- measures %>%
      googlesheets::gs_read(ws = "daily_record") %>%
      dplyr::mutate(date = lubridate::ymd(date))
    Sys.sleep(2)

    # extract daily tasks
    daily_tasks <- measures %>%
      googlesheets::gs_read(ws = "daily_tasks")
    Sys.sleep(2)
    # extract daily record
    signifiers <- measures %>%
      googlesheets::gs_read(ws = "signifiers")
    Sys.sleep(2)
    # extract operations
    operations <- measures %>%
      googlesheets::gs_read(ws = "operations") %>%
      dplyr::mutate(initiated = lubridate::ymd(initiated))
    Sys.sleep(2)
    # workload definitions
    workload_key <- measures %>%
      googlesheets::gs_read(ws = "workload")
    Sys.sleep(2)
    day_view <- measures %>%
      googlesheets::gs_read(ws = "day_view")

    # return data
    return(
      list(
        dataset_key = dataset_key,
        daily = daily,
        daily_tasks = daily_tasks,
        signifiers = signifiers,
        operations = operations,
        workload_key = workload_key,
        day_view = day_view
      )
    )

  }
