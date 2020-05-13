
library(FSK2R)
library(tidyverse)
library(DT)

function(input, output) {

    ## File input

    fsk_file <- reactive({
        req(input$fsk_path)

        tryCatch(
            {
                import_fsk(input$fsk_path$datapath)

            },
            error = function(e) {
                stop(safeError(e))
            }
        )
    })

    ## Query

    output$query_general_1d <- renderTable({

        gen_info <- get_general_info(fsk_file())

        frame_types <- gen_info  %>%
            map(class) %>%
            map_lgl(., ~ "data.frame" %in% .)

        list_types <- gen_info %>%
            map(class) %>%
            map_lgl(., ~ "list" %in% .)

        ## Make a data frame with the 1D-vectors

        gen_info[!as.logical(list_types + frame_types)] %>%
            as.data.frame() %>%
            gather(Variable, Value) %>%
            as.data.frame()
    })

    output$query_scope_1d <- renderTable({

        scope_info <- get_scope(fsk_file())

        frame_types <- scope_info  %>%
            map(class) %>%
            map_lgl(., ~ "data.frame" %in% .)

        ## Make a data frame with the 1D-vectors

        scope_info[!frame_types] %>%
            as.data.frame() %>%
            gather(Variable, Value) %>%
            as.data.frame()

    })

    output$query_background_1d <- renderTable({

        back_info <- get_background(fsk_file())

        frame_types <- back_info  %>%
            map(class) %>%
            map_lgl(., ~ "data.frame" %in% .)

        ## Make a data frame with the 1D-vectors

        back_info[!frame_types] %>%
            as.data.frame() %>%
            gather(Variable, Value) %>%
            as.data.frame()

    })

    output$background <- renderTable({
        back <- fsk_file() %>% get_background()

        tibble(attribute = c("Study title", "Study description"),
               value = c(back$studyTitle, back$studyDescription))
    })

    output$model_math <- renderTable({
        fsk_file() %>% get_modelmath() %>% .$parameter
    })

    output$readme <- renderText({
        fsk_file() %>% get_readme()
    })

    ## Simulations

    simul <- eventReactive(input$run_simul, {

        withProgress(message = "Running simulation",
                     run_simulation(fsk_file(), input$simul_id, run_visualization = TRUE)
                     )


    })

    output$simul_result <- renderPlot({

        simul()

    })

    output$simul_param_metadata <- renderTable({

        if (input$simul_id > n_simuls_fsk(fsk_file())) {
            stop("The simulation ID is higher than the number of simulations in the model")
        }

        simul_data <- fsk_file()$simulation$sedML$listOfModels[[input$simul_id]]

        lapply(simul_data[[1]], function(x) {
            tibble(var = attr(x, "target"),
                   value = attr(x, "newValue"))
        }) %>%
            bind_rows()

    })

    ## Output

    output$download_model <- downloadHandler(
        filename = "model.R",
        content = function(file) {
            writeLines(fsk_file()$R_model, file)
        }
    )

    output$download_vis <- downloadHandler(
        filename = "visualization.R",
        content = function(file) {
            writeLines(fsk_file()$visualization, file)
        }
    )

    output$download <- downloadHandler(
        filename = function() {
            "myFSK.fskx"
        },
        content = function(file) {
            export_fsk(fsk_file(), file)
        },
        contentType = "application/zip"
    )
}



























