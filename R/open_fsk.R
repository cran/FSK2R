

#' Import an FSK model into R
#'
#' Importst the file in file_path and transforms it into a list of class FSK2R.
#'
#' @param file_path Path where the file is located.
#' @param check Whether checks are made. FALSE by default.
#'
#' @importFrom utils unzip
#' @importFrom rlang .data
#'
#' @export
#'
#' @return An instance of FSK2R.
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  get_general_info(my_fsk)
#' }
#'
import_fsk <- function(file_path, check = FALSE) {

  # my_tempdir <- tempdir(check = TRUE)
  my_tempdir <- tempdir()

    if (! file.exists(file_path)) {
        stop(paste("File", file_path, "not found."))
    }

    unzip(file_path, exdir = my_tempdir)

    ## manifest

    my_manifest <- read_fsk_manifest(my_tempdir, check = check)

    if (check) {
        check_manifest_files(my_manifest, my_tempdir)
    }

    ## metaData.json

    my_json_metadata <- read_fsk_json_metadata(my_tempdir, check = check)

    ## metadata.rdf

    my_rdf_metadata <- read_fsk_rdf_metadata(my_tempdir, check = check)

    ## model.sbml

    model_file <- filter(my_manifest, .data$filetype == "sbml")$filename

    if (length(model_file) != 1) {
      stop(paste("There should be only one .sbml file. There are", length(model_file)))
    }

    my_model <- read_fsk_model(my_tempdir, check = check, filename = model_file)

    ## packages.json

    my_paquete <- read_fsk_packages(my_tempdir, check = check)

    ## README.txt

    my_readme <- read_fsk_readme(my_tempdir, check = check)

    ## sim.sedml

    sim_file <- filter(my_manifest, .data$filetype == "sedml")$filename

    if (length(sim_file) != 1) {
      stop(paste("There should be only one .sedml file. There are", length(sim_file)))
    }

    my_simulation <- read_fsk_sim(my_tempdir, check = check, filename = sim_file)

    ## model.R

    Rmodel_file <- filter(my_manifest, .data$filename %in% c("model.r", "model.R"))$filename

    if (length(Rmodel_file) != 1) {
      stop(paste("There should only be one model.R. There are", length(Rmodel_file)))
    }

    my_R_model <- read_R_model(my_tempdir, check = check, filename = Rmodel_file)

    ## visualization.R

    visualization_file <- filter(my_manifest, .data$filename %in% c("visualization.r", "visualization.R"))$filename

    if (length(visualization_file) != 1) {
      stop(paste("There should only be one model.R. There are", length(visualization_file)))
    }

    my_visualization <- read_visualization(my_tempdir, check = check, filename = visualization_file)

    ## Other files

    other_files <- read_other_files(my_tempdir, my_manifest, check = check)

    ## Return

    out <- list(manifest = my_manifest,
                metadata = my_json_metadata,
                model_metadata = my_rdf_metadata,
                model = my_model,
                packages = my_paquete,
                readme = my_readme,
                simulation = my_simulation,
                R_model = my_R_model,
                visualization = my_visualization,
                other_files = other_files
                )

    class(out) <- c("FSK2R", class(out))

    out
}

#' Read "other files"
#'
#' The R models may require further files that we can not predict. This functions just
#' reads all the "unrecognized" files included in the manifest and copies them to
#' the working directory.
#'
#' @param my_tempdir Temporary directory to extract contents of the zyp file.
#' @param my_manifest A list with the information in the manifest file
#' @param check Whether checks are made.
#'
read_other_files <- function(my_tempdir, my_manifest, check = FALSE)  {

  # known_files <- paste0("./",
  #                       c("model.R", "visualization.R", "manifest.xml", "metaData.json", "metadata.rdf",
  #                         "model.sbml", "packages.json", "README.txt", "sim.sedml"))

  known_files <- c(".", "model.R", "model.r", "visualization.r", "visualization.R", "manifest.xml", "metaData.json", "metadata.rdf",
                   "model.sbml", "packages.json", "README.txt", "sim.sedml", "defaultSimulation.R", "defaultSimulation.r")

  other_files <- my_manifest$location[!my_manifest$filename %in% known_files]
  # other_files <- my_manifest$filename[!my_manifest$filename %in% known_files]

  other_paths <- file.path(my_tempdir, other_files)

  set_names(other_paths, other_files)

  # other_paths %>%
  #   map(file) %>%
  #   set_names(other_files)

  # dir.create("./fsk_otherFiles")
  # file.copy(other_files, "./fsk_otherFiles")  # This makes simulations later quite complicated
  # file.copy(other_files, ".")

}


#' Reads the R model in an FSK model
#'
#' @param file_dir path to the file.
#' @param check Whether to make checks. FALSE by default.
#' @param filename Name of the file (model.R by default).
#'
#'
#' @return A character string with the contents of the R file.
#'
read_R_model <- function(file_dir, check = FALSE, filename = "model.R") {

    R_model_path <- file.path(file_dir, filename)

    if (! file.exists(R_model_path)) {
        stop(paste("model.R not found."))
    }

    readtext(R_model_path, verbosity = 0)$text

}

#' Reads the visualization script in an FSK model
#'
#' @inheritParams read_R_model
#'
#' @param filename Name of the file whith the information (visualization.R by default).
#'
#' @return A character string with the contents of the R file.
#'
read_visualization <- function(file_dir, check = FALSE, filename = "visualization.R") {

    visualization_path <- file.path(file_dir, filename)

    if (! file.exists(visualization_path)) {
        stop(paste("visualization.R not found."))
    }

    readtext(visualization_path, verbosity = 0)$text
}

#' Read the manifest of an FSK file and convert it to a data.frame
#'
#' @importFrom XML xmlParse xmlToList
#' @importFrom purrr set_names
#' @importFrom dplyr %>% select
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather spread
#' @importFrom rlang .data
#' @importFrom tools file_ext
#'
#' @inheritParams read_R_model
#' @param filename Name of the file whith the information (manifest.xml by default).
#'
#' @importFrom rlang .data
#'
#' @return A data.frame with the contents of the xlm file.
#'
#'
read_fsk_manifest <- function(file_dir, check = FALSE, filename = "manifest.xml") {

    manifest_path <- file.path(file_dir, filename)

    if (! file.exists(manifest_path)) {
        stop(paste("Manifest file not found."))
    }

    foo <- xmlParse(manifest_path) %>%
        xmlToList() %>%
        as.data.frame(stringsAsFactors = FALSE)

    set_names(foo, paste0("content-", 1:ncol(foo))) %>%
        as_tibble(rownames = "var") %>%
        gather("content", "value", -"var") %>%
        spread("var", "value") %>%
        select(-"content") %>%
        mutate(filename = basename(.data$location),
               filetype = file_ext(.data$location))

}

#' Read the metadata.json file
#'
#' @importFrom rjson fromJSON
#'
#' @inheritParams read_R_model
#'
#' @param filename Name of the file whith the information (meataData.json by default).
#'
#' @return A list with the contents of the metadata file.
#'
read_fsk_json_metadata <- function(file_dir, check = FALSE, filename = "metaData.json") {

    metadata_path <- file.path(file_dir, filename)

    if (! file.exists(metadata_path)) {
        stop(paste("File metaData.json not found."))
    }

    # fromJSON(metadata_path)  # jsonlite
    fromJSON(file = metadata_path)  # rjson

}

#' Read the metadata.rdf
#'
#' @importFrom XML xmlParse xmlToList
#' @importFrom dplyr %>%
#'
#' @inheritParams read_R_model
#'
#' @param filename Name of the file whith the information (metadata.rdf by default).
#'
#' @return A list with the contents of the .xml file.
#'
read_fsk_rdf_metadata <- function(file_dir, check = FALSE, filename = "metadata.rdf") {

    metadata_path <- file.path(file_dir, filename)

    if (! file.exists(metadata_path)) {
        stop(paste("File metadata.rdf not found."))
    }

    xmlParse(metadata_path) %>%
        xmlToList()

}

#' Read the model.sbml
#'
#' @importFrom xml2 read_xml as_list
#' @importFrom dplyr %>%
#'
#' @inheritParams read_R_model
#'
#' @param filename Name of the file whith the information (model.sbml by default).
#'
#' @return A list with the contents of the .xml file.
#'
read_fsk_model <- function(file_dir, check = FALSE, filename = "model.sbml") {

    model_path <- file.path(file_dir, filename)

    if (! file.exists(model_path)) {
        stop(paste(filename, ".sbml not found"))
    }

    # xmlParse(model_path) %>%
    #   xmlToList()

    read_xml(model_path) %>% as_list()

}

#' Read the packages.json
#'
#' @importFrom rjson fromJSON
#'
#' @inheritParams read_R_model
#'
#' @param filename Name of the file whith the information (packages.json by default).
#'
#' @return A list with the contents of the JSON file.
#'
read_fsk_packages <- function(file_dir, check = FALSE, filename = "packages.json") {

    packages_path <- file.path(file_dir, filename)

    if (! file.exists(packages_path)) {

      warning(paste("File packages.json not found."))

      list(Language = "R 3",
           PackageList = list()
           )

    } else {

      fromJSON(file = packages_path)

    }

}

#' Read the README file
#'
#'
#' @inheritParams read_R_model
#'
#' @param filename Name of the file whith the information (README.txt by default).
#'
#' @return A character string with the content of the README file.
#'
read_fsk_readme <- function(file_dir, check = FALSE, filename = "README.txt") {

    readme_path <- file.path(file_dir, filename)

    if (! file.exists(readme_path)) {

        warning(paste("README.txt not found. It will be empty."))
        return("")

    } else {

      readtext(readme_path, verbosity = 0)$text

    }

}

#' Read the sim.sedml file
#'
#' @importFrom xml2 read_xml as_list
#' @importFrom dplyr %>%
#'
#' @inheritParams read_R_model
#'
#' @param filename Name of the file whith the information (sim.sedml by default).
#'
#' @return A list with the content of the xml file.
#'
read_fsk_sim <- function(file_dir, check = FALSE, filename = "sim.sedml") {

    sim_path <- file.path(file_dir, filename)

    if (! file.exists(sim_path)) {
        stop(paste("sim.sedml not found."))
    }

    read_xml(sim_path) %>% as_list()
}









