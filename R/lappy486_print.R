print_message <- function(say = TRUE) {
  if (isTRUE(say)) {
      beepr::beep("ready")
      cowsay::say("lappy486 has sent a polite request to darth printer to print for us. Printing often counts as doing work; good on you for doing some work. Especially the crappy boring bit. \n",
                  what_color = "green", by = "cat")
  }
}


#' Print file
#'
#' Print a file to lappy486.
#'
#' @export

print_file <- function(file_to_print, path = NULL, say = TRUE) {
  paste0(path, file_to_print) %>%
  paste("lp -o sides=two-sided-long-edge", .) %>%
    system()

  print_message(say)
}

#' Print all files in a folder on lappy486
#'
#' I haven't figured out how to print multiple files. When I try, it times out.
#'
#' @export

lappy486_print <- function(path = "print/") {

  files_to_print <- dir(path)

  for(i in 1:length(files_to_print)) {
    files_to_print %>%
      pluck(i) %>%
      print_file(say = FALSE, path = path)
    Sys.sleep(3)
  }

  print_message()
}
