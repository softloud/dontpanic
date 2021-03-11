#' Update measures
#'
#' Need a function to source the new data, rebuild the blog.
#'
#' Assumes lifeswork is cloned into repo directory.
#'
#' 1. Saves current working directory.
#' 2. Sets current working directory to lifework.
#' 3. Updates data.
#' 4. Renders site.
#' 5. Pushes to git.
#' 6. Sets working directory back to current.
#'
#' @export

measures_update <- function() {
  current_wd <- getwd()

  setwd("../lifeswork/")

  source("data-raw/update_data.R")

  rmarkdown::render_site()

  update_text <- "
  git add .
  git commit -m \"dontpanic auto update\"
  git push
  "

  system(update_text)

  setwd(current_wd)

}
