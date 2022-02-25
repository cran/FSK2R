
#' Define a new simulation in an FSK2R object
#'
#' Sets a new simulation using the parameters defined in simulation_pars.
#' The method updates all the relevant methods.
#'
#' @param fsk_object Instance of FSK2R
#' @param simulation_id A character with an id for the new simulation.
#' @param parameters A list whose names are the parameters to modify and their values
#' their values for the simulation.
#'
#' @export
#'
#' @importFrom purrr set_names
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @return An instance of FSK2R with the additional simulation data.
#'
set_new_simulation <- function(fsk_object, simulation_id, parameters) {

    ## TODO: Update the model file?

    changes <- parameters %>% map(~ list())

    for (i in 1:length(parameters)) {

        attr(changes[[i]], "newValue") <- parameters[[i]]
        attr(changes[[i]], "target") <- names(parameters)[[i]]
    }

    names(changes) <- rep("changeAttribute", length(changes))

    new_model <- list(listOfChanges = changes)

    fsk_object$simulation$sedML$listOfModels <- c(fsk_object$simulation$sedML$listOfModels, list(model = new_model))

    fsk_object

}

#' Run one simulation in an FSK object
#'
#' Runs the simulation corresponding to index. If defined, it also
#' calls any visualization script.
#'
#' @importFrom dplyr rename left_join mutate filter tibble
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#'
#' @param fsk_object Instance of FSK2R
#' @param index Index of the simulation
#' @param run_visualization Whether to call the visualization script. FALSE
#' by default.
#'
#' @export
#'
#' @return None
#'
run_simulation <- function(fsk_object, index, run_visualization = FALSE) {

    ## Check for missing packages

    # if (length(fsk_object$packages$PackageList) > 0) {
    #
    #     missing_pkgs <- tibble(aa = unlist(fsk_object$packages$PackageList),
    #            bb = names(unlist(fsk_object$packages$PackageList))) %>%
    #         filter(.data$bb == "Package") %>%
    #         filter(!.data$aa %in% rownames(installed.packages()))
    #     missing_pkgs <- missing_pkgs$aa
    #
    #     if (length(missing_pkgs > 0)) install.packages(missing_pkgs, repos = repos)
    #
    # }

    if (length(fsk_object$packages$PackageList) > 0) {

        required_packages <- tibble(aa = unlist(fsk_object$packages$PackageList),
                                    bb = names(unlist(fsk_object$packages$PackageList))) %>%
            filter(.data$bb == "Package")

        unique(required_packages$aa)

        check_pckgs <- lapply(required_packages$aa, function(x) {
            require(x, character.only = TRUE)
        })

        check_pckgs <- unlist(check_pckgs)

        failed_pckgs <- required_packages[!check_pckgs]

        if (length(failed_pckgs) > 0) {
            stop(paste("Some packages required by the FSK model could not be loaded:", failed_pckgs,
                       "Install them using install.packages() before running the model."))
        }

    }

    ## Extract the other files

    if (length(fsk_object$other_files) > 0) {

        for (i in 1:length(fsk_object$other_files)) {

            this_path <- fsk_object$other_files[i]
            file_name <- basename(this_path)

            con <- file(this_path)
            open(con, "rb")  # binary, so we can open basically everything
            content <- readBin(con, raw(), n = 100000L)  # 100000L and hope for the best

            out <- file(file_name, "wb")
            writeBin(content, out)

            close(con)
            close(out)

        }

    }

    ## Define the environment

    sim_env <- new.env(parent = parent.frame())

    if (index > n_simuls_fsk(fsk_object)) stop(paste("Index higher than the number of models"))

    my_model <- fsk_object$simulation$sedML$listOfModels[[index]]

    if (is.null(my_model)) stop(paste("Model", index, "not found."))

    ## Assign the parameters for the simulation

    # meta_info <- fsk_object$metadata$modelMath$parameter

    if (length(my_model$listOfChanges) > 0) {  # So that models without parameters can run.

        meta_info <- fsk_object$metadata$modelMath$parameter %>%
            map(unlist) %>%
            map(~ tibble(par = names(.), value = .)) %>%
            map_dfr(~ spread(., par, value))

        for (row_n in (1:length(my_model$listOfChanges))) {  # Yep, a for loop...

            simul_data <- my_model$listOfChanges[[row_n]]
            par_metadata <- filter(meta_info, .data$parameterID == attr(simul_data, "target"))

            if ("parameterType" %in% names(par_metadata)) {

                par_type <- par_metadata$parameterType

            } else {

                par_type <- par_metadata$parameterDataType
            }

            # par_type <- par_metadata$parameterType

            # if (is.null(par_type)) {
            #     par_type <- par_metadata$parameterDataType
            # }

            # var_value <- switch(par_metadata$parameterType,  Did it change between versions?
            var_value <- switch(par_type,
                                # "Number" = as.numeric(attr(simul_data, "newValue")),  ######
                                eval(parse(text = attr(simul_data, "newValue")), envir = sim_env)
                                # eval(parse(text = par_metadata$parameterValue), envir = sim_env)
            )

            # var_name <- pars_frame[["parameterID"]][i]
            # var_value <- ifelse(is.na(pars_frame[["this_val"]][i]), pars_frame[["newValue"]][i], pars_frame[["this_val"]][i])
            assign(attr(simul_data, "target"), var_value, pos = sim_env)
            # delayedAssign(simul_data[["target"]], var_value, eval.env = sim_env, assign.env = sim_env)

        }

    }

    ## Run the simulation

    eval(parse(text = fsk_object$R_model), envir = sim_env)

    if (run_visualization) eval(parse(text = fsk_object$visualization), envir = sim_env)

}

#' Run every simulation in an FSK object
#'
#' Runs every simulation defined in the FSK object. This includes
#' the ones originally included in the FSK container, as well as
#' the ones added using set_new_simulation().
#'
#' @inheritParams run_simulation
#'
#' @export
#'
#' @return None
#'
run_all_simulations <- function(fsk_object, run_visualization = FALSE) {

    for (i in 1:n_simuls_fsk(fsk_object)) {

        run_simulation(fsk_object, i, run_visualization)

    }

}














