#' Get the path to my example fake data file
#'
#' @return The full file path to fake_data.xlsx
#' @export
get_example_data_path <- function() {
  system.file("extdata", "fake_data.xlsx", package = "ss2emu")
}

