
#' Creates an FSK model from an existing R script
#'
#' The model includes the R model. If provided as arguments, it also includes
#' the visualization script and the README. Besides, it generates a typical
#' model_metadata, as well as a simulation (without parameters).
#' The manifest is left empty.
#'
#' @param r_model character with the path to the R script with the model.
#' @param r_visualization (optional) character with the path to the R
#' script with the visualization.
#' @param readme (optional) path to README file.
#' @param other_files (optional) character vector with the path to additional
#' @param pckg_frame (optional) data.frame with 2 columns `Package`
#' files required by the model.
#'
#' @return An instance of FSK2R.
#'
#' @importFrom readtext readtext
#' @importFrom utils sessionInfo
#'
#' @export
#'
#' @examples
#' \donttest{
#'  model_path <- system.file("extdata", "model.r", package = "FSK2R")
#'  visualization_path <- system.file("extdata", "visualization.r", package = "FSK2R")
#'  FSK_from_R <- create_fsk(model_path, visualization_path)
#' }
#'
create_fsk <- function(r_model, r_visualization = NULL, readme = NULL,
                       other_files = NULL, pckg_frame = NULL) {

    ## R model

    if (! file.exists(r_model)) {
        stop(paste("R model not found"))
    }

    my_model <- readtext(r_model, verbosity = 0)$text

    ## Visualization

    if (is.null(r_visualization)) {
        my_visualization <- ""

    } else {
        my_visualization <- readtext(r_visualization, verbosity = 0)$text

    }

    ## Readme

    if (is.null(readme)) {
        my_readme <- ""

    } else {
        my_readme <- readtext(readme, verbosity = 0)$text

    }

    ## Packages

    if (is.null(pckg_frame)) {
        pckgs <- NULL  # TBD. Try to get them
    } else {
        pckgs <- pckg_frame
    }

    packages <- list(
        Language = paste("R", sessionInfo()$R.version$major),
        PackageList = pckgs
    )

    ## Simulations

    sims <- list()

    ### listOfSimulations

    sims$sedML$listOfSimulations$steadyState$annotation$sourceScript <- list()
    attr(sims$sedML$listOfSimulations$steadyState$annotation$sourceScript, "language") <- "https://iana.org/assignments/mediatypes/text/x-r"
    attr(sims$sedML$listOfSimulations$steadyState$annotation$sourceScript, "src") <- "./param.r"

    sims$sedML$listOfSimulations$steadyState$algorithm <- list()
    attr(sims$sedML$listOfSimulations$steadyState$algorithm, "kisaoID") <- " "

    attr(sims$sedML$listOfSimulations$steadyState, "id") <- "steadyState"
    attr(sims$sedML$listOfSimulations$steadyState, "name") <- ""

    ### listOfModels

    sims$sedML$listOfModels$model$listOfChanges <- list()
    attr(sims$sedML$listOfModels$model, "id") <- "defaultSimulation"
    attr(sims$sedML$listOfModels$model, "name") <- ""
    attr(sims$sedML$listOfModels$model, "language") <- "https://iana.org/assignments/mediatypes/text/x-r"
    attr(sims$sedML$listOfModels$model, "source") <- "./model.r"

    ### listOfTasks

    sims$sedML$listOfTasks$task <- list()
    attr(sims$sedML$listOfTasks$task, "id") <- "task0"
    attr(sims$sedML$listOfTasks$task, "name") <- ""
    attr(sims$sedML$listOfTasks$task, "modelReference") <- "defaultSimulation"
    attr(sims$sedML$listOfTasks$task, "simulationReference") <- "steadyState"

    ### listOfDataGenerators

    sims$sedML$listOfDataGenerators <- list()

    ### listOfOutputs

    sims$sedML$listOfOutputs$plot2D$annotation$sourceScript <- list()
    attr(sims$sedML$listOfOutputs$plot2D$annotation$sourceScript, "language") <- "https://iana.org/assignments/mediatypes/text/x-r"
    attr(sims$sedML$listOfOutputs$plot2D$annotation$sourceScript, "src") <- "./visualization.r"
    attr(sims$sedML$listOfOutputs$plot2D, "id") <- "plot1"
    attr(sims$sedML$listOfOutputs$plot2D, "name") <- ""

    ## Model metadata

    m_metadata <- list()

    m_metadata$a$conformsTo <- "2.0"
    m_metadata$a$.attrs <- c(about = ".")
    attr(m_metadata$a$`.attrs`, "namespaces") <- c(`http://www.w3.org/1999/02/22-rdf-syntax-ns#` = "rdf")

    m_metadata$b$type <- "visualizationScript"
    m_metadata$b$.attrs <- c(about = "/visualization.r")
    attr(m_metadata$b$`.attrs`, "namespaces") <- c(`http://www.w3.org/1999/02/22-rdf-syntax-ns#` = "rdf")

    m_metadata$c$type <- "workspace"
    m_metadata$c$.attrs <- c(about = "/workspace.r")
    attr(m_metadata$c$`.attrs`, "namespaces") <- c(`http://www.w3.org/1999/02/22-rdf-syntax-ns#` = "rdf")

    m_metadata$d$type <- "modelScript"
    m_metadata$d$.attrs <- c(about = "/model.r")
    attr(m_metadata$d$`.attrs`, "namespaces") <- c(`http://www.w3.org/1999/02/22-rdf-syntax-ns#` = "rdf")

    m_metadata$e$type <- "readme"
    m_metadata$e$.attrs <- c(about = "/README.txt")
    attr(m_metadata$e$`.attrs`, "namespaces") <- c(`http://www.w3.org/1999/02/22-rdf-syntax-ns#` = "rdf")

    names(m_metadata) <- rep("Description", 5)

    ## Model

    model_sbml <- list()
    model_sbml$model$sbml$model$listOfParameters <- list()

    attr(model_sbml$model$sbml, "xmlns") <- "http://www.sbml.org/sbml/level3/version1/core"
    attr(model_sbml$model$sbml, "xmlns:fsk") <- "https://foodrisklabs.bfr.bund.de/wp-content/uploads/2017/01/FSK-ML_guidance_document_021216.pdf"
    attr(model_sbml$model$sbml, "version") <- "1"
    attr(model_sbml$model$sbml, "level") <- "3"

    attr(model_sbml$model$sbml$model, "id") <- "model"

    ## Build the object and return

    out <- list(
        manifest = NULL,  # Fills in export
        metadata = NULL,  # Fill from template with functions
        model_metadata = m_metadata,
        model = model_sbml,
        packages = packages,
        readme = my_readme,
        simulation = sims,
        R_model = my_model,
        visualization = my_visualization

    )

    class(out) <- c("FSK2R", class(out))

    out

}













