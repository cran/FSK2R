# #
# library(FSK2R)
#
# ## Importing a file
#
# path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")
#
# my_fsk <- import_fsk(path_example)
# my_fsk <- import_fsk("initializeParentsAnimals_MOD2.fskx")

# run_simulation(my_fsk, 1, run_visualization = TRUE)
#
# new_model <- set_new_simulation(my_fsk, "new",
#                                 list(r = 0.09,
#                                      Dose_matrix = "as.matrix(read.table(file =\"Dose_matrix.csv\",sep=\",\", header = TRUE, row.names=1))",
#                                      alpha = 0.05,
#                                      beta = 0.045,
#                                      eta = 0.003
#                                 )
# )
#
# n_simuls_fsk(new_model)
#
# run_simulation(new_model, 2, TRUE)
#
# ## Creating a model
#
# model_path <- system.file("extdata", "model.r", package = "FSK2R")
# visualization_path <- system.file("extdata", "visualization.r", package = "FSK2R")
#
# FSK_from_R <- create_fsk(model_path, visualization_path)
#
# run_simulation(FSK_from_R, 1, TRUE)
#
# export_fsk(FSK_from_R, "out.fskx")
#
# ## Metadata
#
# download_metadata_schema("my_template.xlsx")
#
# template_path <- system.file("extdata", "example_template.xlsx", package = "FSK2R")
#
# FSK_from_R <- read_fsk_metadata_excel(FSK_from_R, template_path)
#
# run_simulation(FSK_from_R, 1, TRUE)
#
# ## Output
#
# export_fsk(my_fsk, "out.fskx")
# imported_model <- import_fsk("out.fskx")
#
# run_simulation(imported_model, 1, TRUE)
#
#
#
#
#
#
#
#
#
