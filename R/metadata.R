
#' #' Download the latest version of the MetaData Master Table as Excel
#'
#' @importFrom stringr str_ends
#' @importFrom googlesheets gs_url gs_download
#'
#' @param out_path Character saying where to save the file.
#' @param sheet Character specifying what sheet to download. All of them by default (NULL).
#'
#' @export
#'
#' @return None
#'
#'
download_metadata_schema <- function(out_path, sheet = NULL) {

    ## Do not forget to update the URL with future releases of the template

    url <- "https://docs.google.com/spreadsheets/d/12ujuQn49Gp2F-zMbCAgsu9wYfLjooeR2c2Ec1zkLkl0/edit#gid=766274181"

    my_gs <- gs_url(url)

    if (!str_ends(out_path, ".xlsx")) {
        out_path <- paste0(out_path, ".xlsx")
    }

    gs_download(my_gs, to = out_path, ws = sheet)
}

#' Converts a dataframe to a list
#'
#' Stupid rjson reads data differently than idiot jsonlite, so I had
#' to code this shit.
#'
#' @param this_frame data.frame to convert to a list.
#'
#'
dataframe_to_list <- function(this_frame) {

    apply(this_frame, 1, as.list)

}

#' Fix the metadat so that it is lists
#'
#' @param my_metadata A list with the information in the GoogleSheet as generated
#' by metadata_list_to_fsk.
#'
convert_metadata_to_lists <- function(my_metadata) {

    out <- my_metadata

    out$generalInformation <- lapply(my_metadata$generalInformation, function(x) {

        if("data.frame" %in% class(x)) {
            dataframe_to_list(x)

        } else {
            x
        }

    })

    out$scope <- lapply(my_metadata$scope, function(x) {

        if("data.frame" %in% class(x)) {
            dataframe_to_list(x)

        } else {
            x
        }

    })

    out$dataBackground <- lapply(my_metadata$dataBackground, function(x) {

        if("data.frame" %in% class(x)) {
            dataframe_to_list(x)

        } else {
            x
        }

    })

    out$modelMath <- lapply(my_metadata$modelMath, function(x) {

        if("data.frame" %in% class(x)) {
            dataframe_to_list(x)

        } else {
            x
        }

    })

    return(out)

}

#' Reads the metadata contained in a Google Sheet
#'
#'
#' @param fsk_object FSK2R object where to save the metadata
#' @param title Character identifying the Google Sheet
#' @param type_of_model Character identifying the type of model.
#'
#' @importFrom googlesheets gs_title gs_read
#' @importFrom utils tail
#' @importFrom rlang .data
#'
#' @return A list with the information in the GoogleSheet as generated
#' by metadata_list_to_fsk.
#'
#' @export
#'
read_fsk_metadata <- function(fsk_object, title, type_of_model = "generic") {

    ## Read the sheet

    my_gs <- gs_title(title)

    ## Get the maps

    my_map <- map_FSK_metadata(type_of_model)

    ## Extract the data

    my_range <- my_map$ranges
    ws_name <- my_map$ws_name

    main_part <- gs_read(my_gs, ws = ws_name, range = my_range$main) %>%
        mutate(my_name = paste(.data$Topic, .data$`Detailed metadata concept`, sep = "-"))

    template_data <- list(
        # main_data = gs_read(my_gs, ws = ws_name, range = cell_cols(my_range$main)) %>%
        #     tail(., -1) %>%
        #     filter(`Filter for template` == "Yes"),

        general_info = main_part[my_range$start_general_info:(my_range$start_scope - 1), ],
        scope =  main_part[my_range$start_scope:(my_range$start_data_backgrnd - 1), ],
        data_bckgrnd =  main_part[my_range$start_data_backgrnd:(my_range$start_model_math - 1), ],
        model_math =  main_part[my_range$start_model_math:nrow(main_part), ],

        creators = gs_read(my_gs, ws = ws_name, range = my_range$creators) %>%
            tail(-1),

        authors = gs_read(my_gs, ws = ws_name, range = my_range$authors) %>%
            tail(-1),

        references = gs_read(my_gs, ws = ws_name, range = my_range$references) %>%
            tail(-1),

        product = gs_read(my_gs, ws = ws_name, range = my_range$product) %>%
            tail(-1),

        hazard = gs_read(my_gs, ws = ws_name, range = my_range$hazard) %>%
            tail(-1),

        population = gs_read(my_gs, ws = ws_name, range = my_range$population) %>%
            tail(-1),

        study_sample = gs_read(my_gs, ws = ws_name, range = my_range$study_sample) %>%
            tail(-1),

        dietary = gs_read(my_gs, ws = ws_name, range = my_range$dietary) %>%
            tail(-1),

        lab = gs_read(my_gs, ws = ws_name, range = my_range$lab) %>%
            tail(-1),

        # events <- gs_read(my_gs, ws = ws_name, range = my_range$events)
        assay = gs_read(my_gs, ws = ws_name, range = my_range$assay) %>%
            tail(-1),

        quality = gs_read(my_gs, ws = ws_name, range = my_range$quality) %>%
            tail(-1),

        equation = gs_read(my_gs, ws = ws_name, range = my_range$equation) %>%
            tail(-1),

        parameters = gs_read(my_gs, ws = ws_name, range = my_range$parameters) %>%
            tail(-1)

    )

    fsk_object$metadata <- metadata_list_to_fsk(template_data)

    return(fsk_object)


}

#' FSK metadata from local Excel file
#'
#' @param fsk_object FSK2R object where to save the data
#' @param path character describing the path to the file
#' @param type_of_model character identifying the type of model
#' @param fsk_version Character describing the version of FSK-ML ("1.04" by default).
#'
#' @export
#'
#' @importFrom readxl read_excel
#'
#' @return A list with the information in the Excel file as generated
#' by metadata_list_to_fsk.
#'
read_fsk_metadata_excel <- function(fsk_object, path, type_of_model = "generic", fsk_version = "1.0.5") {

    ## Get the maps

    my_map <- map_FSK_metadata(type_of_model)

    ## Get the map

    my_range <- my_map$ranges
    ws_name <- my_map$ws_name

    ## Extract the data

    main_part <- read_excel(path, sheet = ws_name, range = my_range$main)

    template_data <- list(
        # main_data = read_excel(path, sheet = ws_name, range = cell_cols(my_range$main)) %>%
        #     tail(., -1) %>%
        #     filter(`Filter for template` == "Yes"),
        general_info = main_part[my_range$start_general_info:(my_range$start_scope - 1), ],
        scope =  main_part[my_range$start_scope:(my_range$start_data_backgrnd - 1), ],
        data_bckgrnd =  main_part[my_range$start_data_backgrnd:(my_range$start_model_math - 1), ],
        model_math =  main_part[my_range$start_model_math:nrow(main_part), ],

        creators = read_excel(path, sheet = ws_name, range = my_range$creators) %>%
            tail(-1),

        authors = read_excel(path, sheet = ws_name, range = my_range$authors) %>%
            tail(-1),

        references = read_excel(path, sheet = ws_name, range = my_range$references) %>%
            tail(-1),

        product = read_excel(path, sheet = ws_name, range = my_range$product) %>%
            tail(-1),

        hazard = read_excel(path, sheet = ws_name, range = my_range$hazard) %>%
            tail(-1),

        population = read_excel(path, sheet = ws_name, range = my_range$population) %>%
            tail(-1),

        study_sample = read_excel(path, sheet = ws_name, range = my_range$study_sample) %>%
            tail(-1),

        dietary = read_excel(path, sheet = ws_name, range = my_range$dietary) %>%
            tail(-1),

        lab = read_excel(path, sheet = ws_name, range = my_range$lab) %>%
            tail(-1),

        # events <- read_excel(path, sheet = ws_name, range = my_range$events)
        assay = read_excel(path, sheet = ws_name, range = my_range$assay) %>%
            tail(-1),

        quality = read_excel(path, sheet = ws_name, range = my_range$quality) %>%
            tail(-1),

        equation = read_excel(path, sheet = ws_name, range = my_range$equation) %>%
            tail(-1),

        parameters = read_excel(path, sheet = ws_name, range = my_range$parameters) %>%
            tail(-1)
    )


    my_metadata <- metadata_list_to_fsk(template_data, fsk_version = fsk_version)

    my_metadata <- convert_metadata_to_lists(my_metadata)


    fsk_object$metadata <- my_metadata

    return(fsk_object)
}































