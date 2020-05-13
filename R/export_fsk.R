
#' Exports an object of FSK class as an .fskx file
#'
#' @param fsk_object The instance of FSK2R to be exported.
#' @param out_path Path where the file is to be saved.
#' @param check Whether checks are made. TRUE by default.
#'
#' @importFrom zip zipr
#' @importFrom R.utils isAbsolutePath
#'
#' @export
#'
#' @return None
#'
#' @examples
#' \donttest{
#'  path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#'  my_fsk <- import_fsk(path_example)
#'  class(my_fsk)
#'  export_fsk(my_fsk, out_path=file.path(tempdir(), "out.fskx"))
#' }
#'
export_fsk <- function(fsk_object, out_path, check = TRUE) {

    tmp_folder <- tempdir()  # tempdir used for zipping
    # my_tempdir <- "temp_fsk"  # For debugging, it is just easier to know where the tempdir is

    my_tempdir <- file.path(tmp_folder, paste0("/FSK_", sample(1:9999, 1)))

    if (dir.exists(my_tempdir)) {
        file.remove(my_tempdir)
    }

    dir.create(my_tempdir)

    ## Write every file

    export_R_model(fsk_object, my_tempdir, check)
    export_visualization(fsk_object, my_tempdir, check)
    export_readme(fsk_object, my_tempdir, check)
    export_metadata(fsk_object, my_tempdir, check)
    export_packages(fsk_object, my_tempdir, check)
    export_manifest(fsk_object, my_tempdir, check)
    export_modelmetadata(fsk_object, my_tempdir, check)
    export_sbmlModel(fsk_object, my_tempdir, check)
    export_simulation(fsk_object, my_tempdir, check)
    export_otherfiles(fsk_object, my_tempdir, check)

    ## Zip and move to out_path

    old_wd <- getwd()
    on.exit(setwd(old_wd))

    setwd(my_tempdir)

    if (isAbsolutePath(out_path)) {

        out_path_whole <- out_path

    } else {

        out_path_whole <- file.path(old_wd, out_path)

    }

    zipr(zipfile = out_path_whole, files = list.files())

}

#' Export other files
#'
#' @inheritParams export_fsk
#'
export_otherfiles <- function(fsk_object, out_path, check = FALSE) {

    file.copy(fsk_object$other_files, out_path)

}

#' Export the model.sbml
#'
#' @importFrom xml2 as_xml_document write_xml
#' @importFrom dplyr %>%
#'
#' @inheritParams export_fsk
#'
export_sbmlModel <- function(fsk_object, out_path, check = FALSE) {

    my_file <- file(file.path(out_path, "model.sbml"))

    fsk_object$model %>% as_xml_document() %>% write_xml(my_file)

}

#' Export the sim.sedml
#'
#' @importFrom xml2 as_xml_document write_xml
#' @importFrom dplyr %>%
#'
#' @inheritParams export_fsk
#'
export_simulation <- function(fsk_object, out_path, check = FALSE) {

    my_file <- file(file.path(out_path, "sim.sedml"))

    fsk_object$simulation %>% as_xml_document() %>% write_xml(my_file)

}

#' Functions for exporting the R model of an FSK2R object
#'
#' @inheritParams export_fsk
#'
export_R_model <- function(fsk_object, out_path, check = FALSE) {

    my_file <- file(file.path(out_path, "model.R"))
    writeLines(fsk_object$R_model, my_file)
    close(my_file)

}
##' Functions for exporting the README of an FSK2R object
#'
#' @inheritParams export_fsk
#'
#'
export_readme <- function(fsk_object, out_path, check = FALSE) {

    my_file <- file(file.path(out_path, "README.txt"))
    writeLines(fsk_object$readme, my_file)
    close(my_file)

}

#' Functions for exporting the visualization script of an FSK2R object
#'
#' @inheritParams export_fsk
#'
#'
export_visualization <- function(fsk_object, out_path, check = FALSE) {

    my_file <- file(file.path(out_path, "visualization.R"))
    writeLines(fsk_object$visualization, my_file)
    close(my_file)

}

#' Function for exporting the metadata of an FSK2R object
#'
#' @inheritParams export_fsk
#'
#' @importFrom rjson toJSON
#'
export_metadata <- function(fsk_object, out_path, check = FALSE) {

    my_file <- file(file.path(out_path, "metaData.json"))

    fsk_object$metadata %>%
        toJSON(indent = 2) %>%
        writeLines(my_file)

    close(my_file)

}

#' Functions for exporting the packages of an FSK2R object
#'
#' @inheritParams export_fsk
#'
#' @importFrom tidyr %>%
#' @importFrom rjson toJSON
#'
export_packages <- function(fsk_object, out_path, check = FALSE) {

    my_file <- file(file.path(out_path, "packages.json"))
    fsk_object$packages %>%
        toJSON(indent = 2) %>%
        writeLines(my_file)

    close(my_file)
}



#' Functions for exporting the manifest of an FSK2R object
#'
#' @inheritParams export_fsk
#'
#' @importFrom rlang .data
#'
export_manifest <- function(fsk_object, out_path, check = FALSE) {

    if (is.null(fsk_object$manifest)) {

        fsk_object <- update_manifest(fsk_object)

    }

    data <- fsk_object$manifest %>%
        mutate(out = paste0('<content location ="', .data$location,
                            '" format ="', .data$format, '" />'))

    my_file <- file(file.path(out_path, "manifest.xml"))

    writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
                 '<omexManifest xmlns="http://identifiers.org/combine.specifications/omex-manifest">',
                 data$out,
                 "</omexManifest>"
                 ),
               my_file)

    close(my_file)
}

#' Updates the manifest file
#'
#' @importFrom tibble tribble
#'
#' @param fsk_object An instance of FSK2R.
#'
#'
update_manifest <- function(fsk_object) {

    data_types <- tribble(
        ~format, ~type,
        "https://www.iana.org/assignments/media-types/text/csv", ".csv",
        "http://purl.org/NET/mediatypes/application/zip", ".zip",
        "https://www.iana.org/assignments/media-types/application/json", ".json",
        "http://purl.org/NET/mediatypes/text-xplain", ".txt",
        "http://purl.org/NET/mediatypes/application/r", ".R",
        "http://purl.org/NET/mediatypes/application/r", ".r",

    )

    manifest <- tribble(
        ~format, ~location,
        "http://identifiers.org/combine.specifications/omex", ".",
        "https://www.iana.org/assignments/media-types/application/json", "./packages.json",
        "http://purl.org/NET/mediatypes/text-xplain", "./README.txt",
        "http://identifiers.org/combine.specifications/omex-metadata", "./metadata.rdf",
        "http://identifiers.org/combine.specifications/omex-manifest", "./manifest.xml",
        "http://purl.org/NET/mediatypes/application/sbml+xml", "./model.sbml",
        "http://purl.org/NET/mediatypes/application/r",  "./visualization.r",
        "http://identifiers.org/combine.specifications/sed-ml", "./sim.sedml",
        "http://purl.org/NET/mediatypes/application/r", "./model.r",
        "https://www.iana.org/assignments/media-types/application/json", "./metaData.json"
    )

    fsk_object$manifest <- manifest

    return(fsk_object)

}


#' Functions for exporting the model metadata of an FSK2R object
#'
#' @inheritParams export_fsk
#'
export_modelmetadata <- function(fsk_object, out_path, check = FALSE) {

    my_lines <- c('<?xml version="1.0" encoding="UTF-8"?>',
                  '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:vCard="http://www.w3.org/2006/vcard/ns#">',
                  '<rdf:Description rdf:about=".">',
                  '<dcterms:conformsTo>2.0</dcterms:conformsTo>',
                  '</rdf:Description>',
                  '<rdf:Description rdf:about="/visualization.r">',
                  '<dc:type xmlns:dc="http://purl.org/dc/elements/1.1/">visualizationScript</dc:type>',
                  '</rdf:Description>',
                  '<rdf:Description rdf:about="/workspace.r">',
                  '<dc:type xmlns:dc="http://purl.org/dc/elements/1.1/">workspace</dc:type>',
                  '</rdf:Description>',
                  '<rdf:Description rdf:about="/model.r">',
                  '<dc:type xmlns:dc="http://purl.org/dc/elements/1.1/">modelScript</dc:type>',
                  '</rdf:Description>',
                  '<rdf:Description rdf:about="/README.txt">',
                  '<dc:type xmlns:dc="http://purl.org/dc/elements/1.1/">readme</dc:type>',
                  '</rdf:Description>',
                  '</rdf:RDF>'
                  )

    my_file <- file(file.path(out_path, "metadata.rdf"))

    writeLines(my_lines, my_file)

    close(my_file)

}

