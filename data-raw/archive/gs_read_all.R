#' Read all worksheets
#'
#' I was having issues with timeouts, but using this script seems to solve that.
#'
#' from this issue: https://github.com/jennybc/googlesheets/issues/320#issuecomment-372076823
#'
#' @export

gs_read_all <- function(ss, delay = 4){
  ws_names <- gs_ws_ls(ss)

  gs_read_delayed <- function(ss, ws){
    result <- gs_read(ss, ws)
    Sys.sleep(delay)
    return(result)
  }

  worksheets <- map(ws_names, ~ gs_read_delayed(ss, ws = .x)) %>%
    set_names(ws_names)

  return(worksheets)
}
