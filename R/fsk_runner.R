
#' Startup FSK runner
#'
#' Starts FSK runner within RStudio.
#'
#' @importFrom shiny runApp
#'
#' @export
#'
#' @return None
#'
FSK_runner <- function() {
    appDir <- system.file("shiny-examples", "myapp", package = "FSK2R")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `FSK2R`.", call. = FALSE)
    }

    runApp(appDir, display.mode = "normal")
}
