
#' Returns the general info of an FSK object
#'
#' @param fsk_obj An object of class FSK2R
#'
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @return A nested list with the following entries:
#'        \itemize{
#'          \item name
#'          \item source
#'          \item identifier
#'          \item creationDate
#'          \item rights
#'          \item language
#'          \item software
#'          \item creators
#'          \item reference
#'          }
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  get_general_info(my_fsk)
#' }
#'
get_general_info <- function(fsk_obj) {

  creat <- fsk_obj$metadata$generalInformation$creators %>%
    map(unlist)

  if (length(creat) > 0) {

    creat <- set_names(creat, paste0("P", 1:length(creat)))

    creat <- do.call(bind_rows, creat)

  }


  refs <- fsk_obj$metadata$generalInformation$reference %>%
    map(unlist)

  if (length(refs) > 0) {

    refs <- set_names(refs, paste0("P", 1:length(refs)))
    refs <- do.call(bind_rows, refs)

  }

  return(list(name = fsk_obj$metadata$generalInformation$name,
             source = fsk_obj$metadata$generalInformation$source,
             identifier = fsk_obj$metadata$generalInformation$identifier,
             creationDate = fsk_obj$metadata$generalInformation$creationDate,
             rights = fsk_obj$metadata$generalInformation$rights,
             language = fsk_obj$metadata$generalInformation$language,
             software= fsk_obj$metadata$generalInformation$software,
             creators = creat,
             reference = refs

                 ))
}

#' Returns the scope of an FSK object
#'
#' @inheritParams get_general_info
#'
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @return A nested list with the following entries:
#'        \itemize{
#'          \item product
#'          \item hazard
#'          }
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  get_scope(my_fsk)
#' }
#'
get_scope <- function(fsk_obj) {

  prod <- fsk_obj$metadata$scope$product %>%
    map(unlist)

  if (length(prod) > 0) {

    prod <- set_names(prod, paste0("P", 1:length(prod)))
    prod <-  do.call(bind_rows, prod)

  }

  haz <- fsk_obj$metadata$scope$hazard %>%
    map(unlist)

  if (length(haz) > 0) {

    haz <- set_names(haz, paste0("P", 1:length(haz)))
    haz <- do.call(bind_rows, haz)

  }

    return(list(product = prod,
                hazard = haz
    ))
}

#' Returns the background of an FSK object
#'
#' @inheritParams get_general_info
#'
#' @export
#'
#' @return A nested list with the following entries:
#'        \itemize{
#'          \item studyTitle
#'          \item studyDescription
#'          }
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  get_background(my_fsk)
#' }
#'
get_background <- function(fsk_obj) {
    return(list(studyTitle =fsk_obj$metadata$dataBackground$study$studyTitle,
                studyDescription = fsk_obj$metadata$dataBackground$study$studyDescription
    ))
}

#' Returns the model math of an FSK object
#'
#' @inheritParams get_general_info
#'
#' @export
#'
#' @return A nested list with the following entries:
#'        \itemize{
#'          \item parameter
#'          }
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  get_modelmath(my_fsk)
#' }
#'
get_modelmath <- function (fsk_obj) {

  pars <-  fsk_obj$metadata$modelMath$parameter %>%
    map(unlist)

  if (length(pars) < 1) {
    return(list())
  }

  pars <- set_names(pars, paste0("P", 1:length(pars)))

  pars <- do.call(bind_rows, pars)

  return(list(parameter = pars))
}


#' Returns a summary of the simulations of an FSK object (NULL)
#'
#' The function is not in-use. It is kept here for compatibility with older versions.
#'
#' @inheritParams get_general_info
#'
#' @export
#'
#' @return NULL
#'
#'
get_simulations <- function(fsk_obj) {

  NULL

  ## Old version (jsonlite)

  # df <- data.frame(name = character(), newValue = character(), target= character())
  # for ( i in c(1: length(fsk_obj$simulation$listOfModels))) {
  #   for(j in c(1: length(fsk_obj$simulation$listOfModels$model$listOfChanges))) {
  #     de <- data.frame(name = fsk_obj$simulation$listOfModels[i]$model$.attrs["id"],
  #                      newValue = fsk_obj$simulation$listOfModels[i]$model$listOfChanges[j]$changeAttribute["newValue"],
  #                      target = fsk_obj$simulation$listOfModels[i]$model$listOfChanges[j]$changeAttribute["target"])
  #     df <- rbind(df, de)
  #   }
  # }
  # return(df)
}

#' Readme of an FSK object
#'
#' @inheritParams get_general_info
#'
#' @export
#'
#' @return A character vector with the text in the README file.
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  get_readme(my_fsk)
#' }
#'
get_readme <- function(fsk_obj) {
    return(fsk_obj$readme)
}

#' Does the object have an R model?
#'
#' @inheritParams get_general_info
#'
#' @export
#'
#' @return A logical vector.
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  is_fsk_with_r(my_fsk)
#' }
#'
is_fsk_with_r <- function(fsk_obj) {

if(is.null(fsk_obj$R_model)) return(FALSE)
    else return(TRUE)
}

#' Is it an instance of FSK2R?
#'
#' @param object Object to check
#'
#' @export
#'
#' @return A logical vector
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  is.FSK2R(my_fsk)
#' }
#'
is.FSK2R <- function(object) {

  if ("FSK2R" %in% class(object)) {
    TRUE
  } else {
    FALSE
  }

}

#' Number of simulations in the FSK2R object
#'
#' @param fsk_obj An instance of FSK2R
#'
#' @export
#'
#' @return An integer vector of length one.
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  n_simuls_fsk(my_fsk)
#' }
#'
n_simuls_fsk <- function(fsk_obj) {
  length(fsk_obj$simulation$sedML$listOfModels)
}






