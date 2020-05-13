
#'
#' Extract session information
#'
#' @importFrom utils sessionInfo
#'
#' @return A list with 3 elements: r_version, platform and pckgs.
#' The latter is a data.frame with two columns: package and version.
#'
get_session_info <- function() {

    my_session <- sessionInfo()

    r_version <- my_session$R.version$version.string
    platform <- my_session$platform

    all_pckgs <- my_session$loadedOnly

    pckg_infor <- all_pckgs %>%
        map_dfr(~ tibble(package = .$Package,
                         version = .$Version
                         )
                )

    list(r_version = r_version,
         platform = platform,
         pckgs = pckg_infor)
}


#' Finds where packages are stored
#'
#' @param pckgs Character vector with packages names
#'
#' @return A list of packages locations. If one is not present, a character(0).
#'
find_packages <- function(pckgs) {

    pckgs %>% map(find.package)

}

# aa <- get_session_info()
# find_packages(aa$pcks$package)


