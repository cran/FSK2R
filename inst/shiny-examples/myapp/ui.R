
library(shiny)

fluidPage(

    ## The title

    titlePanel("FSK runner"),


    sidebarLayout(

        ## The side

        sidebarPanel(

            fileInput("fsk_path", "Choose FSK-ML File",
                      multiple = FALSE,
                      accept = NULL
                      ),
            hr(),
            h3("Need a model?"),
            actionButton(inputId='foo1', label="RAKIP model repository",
                         icon = icon("th"),
                         onclick ="window.open('https://foodrisklabs.bfr.bund.de/rakip-model-repository-web-services/', '_blank')")

        ),

        ## The main

        mainPanel(

            tabsetPanel(type = "tabs",
                        tabPanel("About",
                                 wellPanel(includeMarkdown("about_FSK_runner.Rmd"))
                                 ),
                        tabPanel("Query",  # These should be made dynamically
                                 navlistPanel(
                                     tabPanel("General info",
                                              tableOutput("query_general_1d"),
                                              uiOutput("query_general_frames")
                                              # uiOutput("query_general")
                                              # h3("Model information"),
                                              # tableOutput("model_info"),
                                              # h3("Creators"),
                                              # # tableOutput("model_creators"),
                                              # DT::dataTableOutput("model_creators"),
                                              # h3("References"),
                                              # DT::dataTableOutput("model_references")
                                              # # tableOutput("model_references")
                                              ),
                                     tabPanel("Scope",
                                              tableOutput("query_scope_1d")
                                              # h3("Product"),
                                              # tableOutput("product"),
                                              # h3("Hazard"),
                                              # tableOutput("hazard")
                                              ),
                                     tabPanel("Background",
                                              tableOutput("query_background_1d")
                                              ),
                                     tabPanel("Model math",
                                              tableOutput("model_math")
                                              ),
                                     tabPanel("README",
                                              textOutput("readme")
                                              )
                                 )
                                 ),
                        tabPanel("Run simulations",
                                 fluidRow(numericInput("simul_id", "Simulation number", 1)),
                                 h3("Simulation parameters"),
                                 tableOutput("simul_param_metadata"),
                                 fluidRow(actionButton("run_simul", "Run simulation")),
                                 fluidRow(plotOutput("simul_result"))
                                 ),
                        tabPanel("Export model",
                                 fluidRow(downloadButton("download", "Download as FSK-ML")),
                                 fluidRow(downloadButton("download_model", "Download model.R")),
                                 fluidRow(downloadButton("download_vis", "Download visualization.R"))
                                 )

            )

        )
    )
)





















