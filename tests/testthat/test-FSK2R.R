
context("Basic tests")

## Check that example files are there

test_that("Files in FSK2R package", {

    expect_true(file.exists(system.file("extdata", "model.r", package = "FSK2R")))
    expect_true(file.exists(system.file("extdata", "visualization.r", package = "FSK2R")))
    expect_true(file.exists(system.file("extdata", "example_template.xlsx", package = "FSK2R")))
    expect_true(file.exists(system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")))


})

## Importing/creating models

test_that("Importing model", {

    path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")

    my_fsk <- import_fsk(path_example)

    expect_true(is.FSK2R(my_fsk))

})

test_that("Create a model from R", {

    model_path <- system.file("extdata", "model.r", package = "FSK2R")
    visualization_path <- system.file("extdata", "visualization.r", package = "FSK2R")

    FSK_from_R <- create_fsk(model_path, visualization_path)

    expect_true(is.FSK2R(FSK_from_R))

})

## Simulations

test_that("Running simulation", {


    model_path <- system.file("extdata", "model.r", package = "FSK2R")
    visualization_path <- system.file("extdata", "visualization.r", package = "FSK2R")

    FSK_from_R <- create_fsk(model_path, visualization_path)

    expect_true(is.FSK2R(FSK_from_R))

    run_simulation(FSK_from_R, 1, TRUE)

    expect_true(TRUE)

})

test_that("Add a simulation", {

    path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")

    my_fsk <- import_fsk(path_example)

    new_model <- set_new_simulation(my_fsk, "new",
                                    list(r = 0.09,
                                         Dose_matrix = "as.matrix(read.table(file =\"Dose_matrix.csv\",sep=\",\", header = TRUE, row.names=1))",
                                         alpha = 0.05,
                                         beta = 0.045,
                                         eta = 0.003
                                    )
    )

    expect_true(is.FSK2R(new_model))

})

## Functions for query

test_that("Query general info", {


    path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")

    my_fsk <- import_fsk(path_example)

    gen_info <- get_general_info(my_fsk)

    expect_true(is.list(gen_info))
    expect_true(length(gen_info) > 0)


})

test_that("Query background", {

    path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")

    my_fsk <- import_fsk(path_example)

    back <- get_background(my_fsk)

    expect_true(is.list(back))
    expect_true(length(back) > 0)


})

test_that("Query model math", {

    path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")

    my_fsk <- import_fsk(path_example)

    math <- get_modelmath(my_fsk)

    expect_true(is.list(math))
    expect_true(length(math) > 0)


})

test_that("Query scope", {

    path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")

    my_fsk <- import_fsk(path_example)

    scope <- get_scope(my_fsk)

    expect_true(is.list(scope))
    expect_true(length(scope) > 0)


})

test_that("Query background", {

    path_example <- system.file("extdata", "ToyModelv4.fskx", package = "FSK2R")

    my_fsk <- import_fsk(path_example)

    readme <- get_readme(my_fsk)

    expect_true(is.character(readme))

})


















