
#' Checks that the files defined in the manifest exist
#'
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @param my_manifest A list with the contents of the manifest file.
#' @param file_dir Path to the directory where all the files have been extracted.
#'
check_manifest_files <- function(my_manifest, file_dir) {

    found <- my_manifest$location %>%
        map(~ paste0(file_dir, .data)) %>%
        map(file.exists) %>%
        unlist()

    if (!all(found)) {

        stop(paste("Files", my_manifest$location[!found], "defined in manifest but not found."))

    }

}





