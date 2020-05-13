
#' Readme of an FSK object
#'
#' @param readme_text A character vector of length 1 with the content of the README file.
#' @param fsk_object An instance of FSK2R.
#'
#' @export
#'
#' @return An instance of FSK2R.
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  set_readme(my_fsk, "This is the README.")
#' }
#'
set_readme <- function(fsk_object, readme_text) {

    fsk_object$readme <- readme_text
    fsk_object

}











