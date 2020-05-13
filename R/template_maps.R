
#' Map for the contents of the metadata
#'
#' Maps the location (range) of different pieces of data within
#' the Excel/Google Sheets template.
#' It also includes the names of the sheets.
#'
#' @param type_of_model Type of model, as defined in the FSK-ML documentation.
#' By default, 'generic'.
#' @param fsk_version Character stating the version of FSK-ML.
#'
#' @importFrom rlang .data
#'
#' @return A list with two components: the 'range' where each piece of information is stored and
#' 'ws_name' with the name of the relevant sheet in the GoogleSheet template.
#'
map_FSK_metadata <- function(type_of_model = "generic", fsk_version = "1.04") {

    ranges_1.04 <- list(
        generic = list(
            main = "A1:I156",
            start_general_info = 1,
            start_scope = 35,
            start_data_backgrnd = 76,
            start_model_math = 121,
            creators = "K2:Y9",
            authors = "AA2:AO9",
            references = "K13:V19",
            product = "K37:U50",
            hazard = "V37:AI50",
            population = "AJ37:AV50",
            study_sample = "L95:U99",
            dietary = "L102:Q106",
            lab = "L109:N113",
            events = "P110:Q113",
            assay = "L116:T120",
            quality = "L124:N129",
            equation = "P122:T128",
            parameters = "L131:AA231"
        ),
        dose_response = list(
            main = "A1:I139",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 67,
            start_model_math = 105,
            creators = "K2:Y9",
            authors = "AA2:AO9",
            references = "K13:V19",
            # product = "K37:U50",
            hazard = "K37:X50",
            population = "Y37:AK50",
            study_sample = "L84:T88",
            # dietary = "L102:Q106",
            lab = "K91:M95",
            events = "O90:O95",
            assay = "K98:S103",
            quality = "K106:M113",
            equation = "O106:S112",
            parameters = "K115:Z136"
        ),
        exposure = list(
            main = "A1:I156",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 78,
            start_model_math = 122,
            creators = "K2:Y9",
            authors = "AA2:AO9",
            references = "K13:V19",
            product = "K37:U50",
            hazard = "V37:AI50",
            population = "AJ37:AV50",
            study_sample = "K95:T99",
            dietary = "K101:P105",
            lab = "K108:M112",
            events = "O107:O112",
            assay = "K114:S119",
            quality = "K123:M130",
            equation = "O123:S130",
            parameters = "K132:Z153"
        ),
        process = list(
            main = "A1:I132",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 65,
            start_model_math = 103,
            creators = "K2:Y9",
            authors = "AA2:AO9",
            references = "K13:V19",
            product = "K37:U50",
            hazard = "V37:AI50",
            # population = "AJ37:AV50",
            study_sample = "K82:T86",
            # dietary = "K101:P105",
            lab = "K89:M93",
            events = "O88:O93",
            assay = "K95:S100",
            quality = "K103:M110",
            equation = "O103:S109",
            parameters = "K113:Z133"
        ),
        predictive = list(
            main = "A1:J131",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 65,
            start_model_math = 103,
            creators = "L2:Z9",
            authors = "AB2:AP9",
            references = "L13:W19",
            product = "L37:V48",
            hazard = "W37:AJ48",
            # population = "AJ37:AV50",
            study_sample = "L82:U86",
            # dietary = "K101:P105",
            lab = "L89:N93",
            # events = "O88:O93",
            assay = "L95:T100",
            quality = "L103:N110",
            equation = "P103:T109",
            parameters = "L113:AA132"
        ),
        toxicological = list(
            main = "A1:J139",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 67,
            start_model_math = 105,
            creators = "L2:Z9",
            authors = "AB2:AP9",
            references = "L13:W19",
            # product = "L37:V48",
            hazard = "L37:Y50",
            population = "Z37:AL50",
            study_sample = "L84:U88",
            # dietary = "K101:P105",
            lab = "L91:N95",
            events = "P90:P95",
            assay = "L97:T102",
            quality = "L105:N112",
            equation = "P105:T111",
            parameters = "L115:AA136"
        ),
        QRA = list(
            main = "A1:J156",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 78,
            start_model_math = 122,
            creators = "L2:Z9",
            authors = "AB2:AP9",
            references = "L13:W19",
            product = "L37:V52",
            hazard = "W37:AJ52",
            population = "AK37:AW52",
            study_sample = "L95:U99",
            dietary = "L101:Q105",
            lab = "L108:N112",
            events = "P107:P112",
            assay = "L114:T119",
            quality = "L122:N129",
            equation = "P122:T128",
            parameters = "L132:AA153"
        ),
        risk_charact = list(
            main = "A1:J156",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 78,
            start_model_math = 122,
            creators = "L2:Z9",
            authors = "AB2:AP9",
            references = "L13:W19",
            product = "L37:V52",
            hazard = "W37:AJ52",
            population = "AK37:AW52",
            study_sample = "L95:U99",
            dietary = "L101:Q105",
            lab = "L108:N112",
            events = "P107:P112",
            assay = "L114:T119",
            quality = "L122:N129",
            equation = "P122:T128",
            parameters = "L132:AA153"
        ),
        other_empirical = list(
            main = "A1:J144",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 78,
            start_model_math = 116,
            creators = "L2:Z9",
            authors = "AB2:AP9",
            references = "L13:W19",
            product = "L37:V50",
            hazard = "W37:AJ50",
            population = "AK37:AW50",
            # study_sample = "L95:U99",
            # dietary = "L101:Q105",
            lab = "L102:N106",
            # events = "P107:P112",
            assay = "L108:T113",
            quality = "L116:N123",
            equation = "P116:T122",
            parameters = "L126:AA145"
        ),
        consumption = list(
            main = "A1:J137",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 64,
            start_model_math = 108,
            creators = "L2:Z9",
            authors = "AB2:AP9",
            references = "L13:W19",
            product = "L37:V50",
            # hazard = "W37:AJ50",
            population = "W37:AI50",
            study_sample = "L81:U85",
            dietary = "L87:Q91",
            lab = "L94:N98",
            events = "P93:P98",
            assay = "L100:T105",
            quality = "L108:N115",
            equation = "P108:T114",
            parameters = "L118:AA138"
        ),
        health = list(
            main = "A1:J139",
            start_general_info = 2,
            start_scope = 36,
            start_data_backgrnd = 67,
            start_model_math = 105,
            creators = "L2:Z9",
            authors = "AB2:AP9",
            references = "L13:W19",
            # product = "L37:V50",
            hazard = "L37:Y50",
            population = "Z37:AL50",
            study_sample = "L84:U88",
            # dietary = "L87:Q91",
            lab = "L91:N95",
            events = "P90:P95",
            assay = "L97:T102",
            quality = "L105:N112",
            equation = "P105:T111",
            parameters = "L115:AA136"
        )
    )

    ranges <- switch(fsk_version,
           `1.04` = ranges_1.04,
           stop("Unsupported FSK version"))

    ws_name <- tibble(
        type = names(ranges),
        kwd = c("Generic Metadata Schema", "Dose-response Model", "Exposure Model", "Process Model",
                "Predictive Model", "Toxicological reference value Models", "QRA Models",
                "Risk Charact Model", "Other Empirical Model", "Consumption Model", "Health metrics Models")
    ) %>%
        filter(.data$type == type_of_model)
    ws_name <- ws_name$kwd

    list(ranges = ranges[[type_of_model]], ws_name = ws_name)
}

#' Map between the names used in the template and the xml
#'
#' Returns a map of the names used within the sheets of the
#' Excel/GoogleSheets template and the ones in metadata.json.
#'
#' @importFrom tibble tribble
#'
map_metadata_xml_template <- function() {

    tribble(
        ~template, ~xml,
        "Study / Data / Model name-NA", "name",
        "Source-NA", "source",
        "Identifier-NA", "identifier",
        "Author-See vCard 4.0 standard", "NA",
        "Creator-See vCard 4.0 standard", "NA",
        "Date-Creation date", "creationDate",
        "NA-Last modified date", "modifiedDate",
        "Rights-Rights", "rights",
        "Availability-NA", "availability",
        "URL-NA", "url",
        "Format-NA", "format",
        "References-Is_reference_description?", "NA",
        "NA-Publication type", "NA",
        "NA-Publication year", "NA",
        "NA-PubMed ID", "NA",
        "NA-Publication DOI", "NA",
        "NA-Publication Author List", "NA",
        "NA-Publication Title", "NA",
        "NA-Publication Abstract", "NA",
        "NA-Publication Journal / Vol / Issue etc", "NA",
        "NA-Publication Status", "NA",
        "NA-Publication website", "NA",
        "NA-Comment", "NA",
        "Language-NA", "language",
        "Software-NA", "software",
        "Language written in-NA", "languageWrittenIn",
        "Model category-Model Class", "modelCategory",
        "NA-Model Sub-Class", "modelSubClass",
        "NA-Model Class comment", "modelClassComment",
        "NA-Basic process", "basicProcess",
        "NA-NA", "NA",
        "Status-NA", "status",
        "Objective-NA", "objective",
        "Description-NA", "description",
        "Product / matrix-Product/matrix name", "NA",
        "NA-Product/matrix description", "NA",
        "NA-Product/matrix unit", "NA",
        "NA-Method of production", "NA",
        "NA-Packaging", "NA",
        "NA-Product treatment", "NA",
        "NA-Country of origin", "NA",
        "NA-Area of origin", "NA",
        "NA-Fisheries area", "NA",
        "NA-Date of production", "NA",
        "NA-Date of expiry", "NA",
        "Hazard-Hazard type", "NA",
        "NA-Hazard name", "NA",
        "NA-Hazard description", "NA",
        "NA-Hazard unit", "NA",
        "NA-Adverse effect", "NA",
        "NA-Source of contamination", "NA",
        "NA-Benchmark Dose (BMD)", "NA",
        "NA-Maximum Residue Limit (MRL)", "NA",
        "NA-No Observed Adverse Affect Level (NOAEL)", "NA",
        "NA-Lowest Observed Adverse Effect Level (LOAEL)", "NA",
        "NA-Acceptable Operator Exposure Level (AOEL)", "NA",
        "NA-Acute Reference Dose (ARfD)", "NA",
        "NA-Acceptable Daily Intake (ADI)", "NA",
        "NA-Hazard ind/sum", "NA",
        "Population Group-Population name", "NA",
        "NA-Target population", "NA",
        "NA-Population Span (years)", "NA",
        "NA-Population description", "NA",
        "NA-Population age", "NA",
        "NA-Population gender", "NA",
        "NA-BMI", "NA",
        "NA-Special diet groups", "NA",
        "NA-Pattern consumption", "NA",
        "NA-Region", "NA",
        "NA-Country", "NA",
        "NA-Risk and population factors", "NA",
        "NA-Season", "NA",
        "General comment-NA", "generalComment",
        "Temporal information-Time", "time",
        "Spatial information-Region", "region",
        "NA-Country", "country",
        "Study-Study Identifier", "studyIdentifier",
        "NA-Study Title", "studyTitle",
        "NA-Study Description", "studyDescription",
        "NA-Study Design Type", "studyDesignType",
        "NA-Study Assay Measurement Type", "studyAssayMeasurementType",
        "NA-Study Assay Technology Type", "studyAssayTechnologyType",
        "NA-Study Assay Technology Platform", "studyAssayTechnologyPlatform",
        "NA-Accreditation procedure for the assay technology", "accreditationProcedure",
        "NA-Study Protocol Name", "protocolName",
        "NA-Study Protocol Type", "protocolType",
        "NA-Study Protocol Description", "protocolDescription",
        "NA-Study Protocol URI", "protocolURI",
        "NA-Study Protocol Version", "protocolVersion",
        "NA-Study Protocol Parameters Name", "protocolParameters",
        "NA-Study Protocol Components Name", "protocolComponentsName",
        "NA-Study Protocol Components Type", "protocolComponentsType",
        "Study Sample-Sample Name (ID)", "NA",
        "NA-Protocol of sample collection", "NA",
        "NA-Sampling strategy", "NA",
        "NA-Type of sampling program", "NA",
        "NA-Sampling method", "NA",
        "NA-Sampling plan", "NA",
        "NA-Sampling weight", "NA",
        "NA-Sampling size", "NA",
        "NA-Lot size unit", "NA",
        "NA-Sampling point", "NA",
        "Dietary assessment method-Methodological tool to collect data", "NA",
        "NA-Number of non-consecutive one-day", "NA",
        "NA-Dietary software tool", "NA",
        "NA-Number of food items", "NA",
        "NA-Type of records", "NA",
        "NA-Food descriptors", "NA",
        "Laboratory-Laboratory accreditation", "NA",
        "NA-Laboratory Name", "NA",
        "NA-Laboratory country", "NA",
        "Assay-Assay Name", "NA",
        "NA-Assay description", "NA",
        "NA-Percentage of moisture", "NA",
        "NA-Percentage of fat", "NA",
        "NA-Limit of detection", "NA",
        "NA-Limit of quantification", "NA",
        "NA-Left-censored data", "NA",
        "NA-Range of contamination", "NA",
        "NA-Uncertainty value", "NA",
        "Parameter / Factor / Input / Output / Data column-Parameter ID", "NA",
        "NA-Parameter classification", "NA",
        "NA-Parameter name", "NA",
        "NA-Parameter description", "NA",
        "NA-Parameter unit", "NA",
        "NA-Parameter unit category", "NA",
        "NA-Parameter data type", "NA",
        "NA-Parameter source", "NA",
        "NA-Parameter subject", "NA",
        "NA-Parameter distribution", "NA",
        "NA-NA", "NA",
        "NA-NA", "NA",
        "NA-Parameter value", "NA",
        "NA-Parameter Reference", "NA",
        "NA-Parameter variability subject", "NA",
        "NA-Parameter value max", "NA",
        "NA-Parameter value min", "NA",
        "NA-Parameter error", "NA",
        "Quality measures-SSE / MSE / RMSE / Rsquared / AIC / BIC", "NA",
        "NA-Sensitivity analysis", "NA",
        "Model equation-Model equation name", "NA",
        "NA-Model equation class/distribution", "NA",
        "NA-NA", "NA",
        "NA-NA", "NA",
        "NA-NA", "NA",
        "NA-Model equation reference", "NA",
        "NA-Model equation / Script", "NA",
        "NA-Hypothesis of the model", "NA",
        "Fitting procedure-NA", "fittingProcedure",
        "Exposure-Methodological treatment of left-censored data", "treatmentOfLeftCensoredData",
        "NA-Level of contamination after left-censored data treatment", "levelOfContamination",
        "NA-Type of exposure", "typeOfExposure",
        "NA-Scenario", "scenario",
        "NA-Uncertainty estimation", "uncertaintyEstimation",
        "Events-NA", "NA",
        "Honorific", "honorific",
        "Name", "name",
        "Given name", "givenName",
        "Additional name", "additionalName",
        "Family name", "familyName",
        "Organization", "organization",
        "Telephone", "telephone",
        "Email", "email",
        "Country", "country",
        "city", "city",
        "ZIP code", "zipCode",
        "Post office box", "postOfficeBox",
        "Street address", "streetAddress",
        "Extended address", "extendedAddress",
        "Extended address ", "extendedAddress",
        "Region ", "region",

        ## References

        "Is_reference_description?", "is_descript",
        "Publication type", "publicationType",
        "Publication year", "publicationYear",
        "PubMed ID", "pubMedId",
        "Publication DOI", "publicationDOI",
        "Publication Author List", "publicationAuthorList",
        "Publication Title", "publicationTitle",
        "Publication Abstract", "publicationAbstract",
        "Publication Journal / Vol / Issue etc", "publicationJournal",
        "Publication Status", "publicationStatus",
        "Publication website", "publicationWebsite",
        "Comment", "comment",

        ## Product/Matrix

        "Product/matrix name", "productName",
        "Product/matrix description", "productDescription",
        "Product/matrix unit", "productUnit",
        "Method of production", "productionMethod",
        "Packaging", "packaging",
        "Product treatment", "productTreatment",
        "Country of origin", "countryOfOrigin",
        "Area of origin", "areaOfOrigin",
        "Fisheries area", "fisheriesArea",
        "Date of production", "dateOfProduction",
        "Date of expiry", "dateOfExpiry",

        ## Hazard

        "Hazard type", "hazardType",
        "Hazard name", "hazardName",
        "Hazard description", "hazardDescription",
        "Hazard unit", "hazardUnit",
        "Adverse effect", "adverseEffect",
        "Source of contamination", "sourceOfContamination",
        "Benchmark Dose (BMD)", "benchmarkDose",
        "Maximum Residue Limit (MRL)", "maximumResidueLimit",
        "No Observed Adverse Affect Level (NOAEL)", "noObservedAdverseEffectLevel",
        "Lowest Observed Adverse Effect Level (LOAEL)", "lowestObservedAdverseEffectLevel",
        "Acceptable Operator Exposure Level (AOEL)", "acceptableOperatorExposureLevel",
        "Acute Reference Dose (ARfD)", "acuteReferenceDose",
        "Acceptable Daily Intake (ADI)", "acceptableDailyIntake",
        "Hazard ind/sum", "hazardIndSum",

        ## Population

        "Population name", "populationName",
        "Target population", "targetPopulation",
        "Population Span (years)", "populationSpan",
        "Population description", "populationDescription",
        "Population age", "populationAge",
        "Population gender", "populationGender",
        "BMI", "bmi",
        "Special diet groups", "specialDietGroups",
        "Pattern consumption", "patternConsumption",
        "Region", "region",
        "Country ", "country",
        "Risk and population factors", "riskAndPopulationFactors",
        "Season", "season",

        ## Study/sample

        "Sample Name (ID)", "sampleName",
        "Protocol of sample collection", "protocolSampleCollection",
        "Sampling strategy", "samplingStrategy",
        "Type of sampling program", "typeOfSamplingProgram",
        "Sampling method", "samplingMethod",
        "Sampling plan", "samplingPlan",
        "Sampling weight", "samplingWeight",
        "Sampling size", "samplingSize",
        "Lot size unit", "lotSizeUnit",
        "Sampling point", "samplingPoint",

        ## Dietary

        "Methodological tool to collect data", "methodologicalTool",
        "Number of non-consecutive one-day", "nonConsecutiveOneDay",
        "Dietary software tool", "dietarySoftware",
        "Number of food items", "numberOfFoodItems",
        "Type of records", "typeOfRecords",
        "Food descriptors", "foodDescriptors",

        ## Laboratory

        "Laboratory accreditation", "laboratoryAccreditation",
        "Laboratory Name", "laboratoryName",
        "Laboratory country", "laboratoryCountry",

        ## Assay

        "Assay Name", "assayName",
        "Assay description", "assayDescription",
        "Percentage of moisture", "percentageOfMoisture",
        "Percentage of fat", "percentageOfFat",
        "Limit of detection", "limitOfDetection",
        "Limit of quantification", "limitOfQuantification",
        "Left-censored data", "leftCensoredData",
        "Range of contamination", "rangeOfContamination",
        "Uncertainty value", "uncertaintyValue",

        ## Equation

        "Model equation name", "modelEquationName",
        "Model equation class/distribution", "modelEquationClass",
        "Model equation reference", "modelEquationReference",
        "Model equation / Script", "modelEquationScript",
        "Hypothesis of the model", "hypothesisOfTheModel",

        ## Parameters

        "Parameter ID", "parameterID",
        "Parameter classification", "parameterClassification",
        "Parameter name", "parameterName",
        "Parameter description", "parameterDescription",
        "Parameter unit", "parameterUnit",
        "Parameter unit category", "parameterUnitCategory",
        "Parameter data type", "parameterDataType",
        "Parameter source", "parameterSource",
        "Parameter subject", "parameterSubject",
        "Parameter distribution", "parameterDistribution",
        "Parameter value", "parameterValue",
        "Parameter Reference", "parameterReference",
        "Parameter variability subject", "parameterVariabilitySubject",
        "Parameter value max", "parameterValueMax",
        "Parameter value min", "parameterValueMin",
        "Parameter error", "parameterError"



    )

}


#' From read_fsk_metadata_XX to FSK2R format
#'
#' Converts the contents of the Excel/Google Sheets template into a list
#' with the format of the FSK2R object.
#'
#' @param my_metadata A list generated by
#' @param fsk_version Version of the FSK template.
#'
#' @importFrom rlang .data
#'
metadata_list_to_fsk <- function(my_metadata, fsk_version = "1.0.5") {

    ## Version related variables

    this_version <- paste0("http://BfR/bund/de/knime/model/metadata_", fsk_version)
    this_eClass <- paste0("http://BfR/bund/de/knime/model/metadata_", fsk_version, "#//")

    my_map <- map_metadata_xml_template() %>%
        rename(my_name = "template", out = "xml")

    ## General information

    general_info <- my_metadata$general_info %>%
        mutate(my_name = paste(.data$Topic, .data$`Detailed metadata concept`, sep = "-")) %>%
        left_join(my_map) %>%
        filter(.data$out != "NA") %>%
        select("out", "Data") %>%
        spread("out", "Data") %>%
        as.list()

    foo <- tibble(my_name = names(my_metadata$creators)) %>%
        left_join(my_map)

    general_info$creators <- set_names(my_metadata$creators, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "Contact"))

    foo <- tibble(my_name = names(my_metadata$authors)) %>%
        left_join(my_map)

    general_info$authors <- set_names(my_metadata$authors, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "Contact"))


    foo <- tibble(my_name = names(my_metadata$references)) %>%
        left_join(my_map)

    general_info$reference <- set_names(my_metadata$references, foo$out) %>%
        filter(.data$is_descript == "Yes") %>%
        select(-"is_descript") %>%
        mutate(eClass = paste0(this_eClass, "Reference"))

    general_info$eClass <- paste0(this_eClass, "GeneralInformation")

    ## Scope

    scope <- my_metadata$scope %>%
        mutate(my_name = paste(.data$Topic, .data$`Detailed metadata concept`, sep = "-")) %>%
        left_join(my_map) %>%
        filter(.data$out != "NA") %>%
        select("out", "Data") %>%
        spread("out", "Data") %>%
        as.list()

    scope$eClass <- paste0(this_eClass, "Scope")

    foo <- tibble(my_name = names(my_metadata$product)) %>%
        left_join(my_map)

    scope$product <- set_names(my_metadata$product, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "Product"))

    foo <- tibble(my_name = names(my_metadata$hazard)) %>%
        left_join(my_map)
    scope$hazard <- set_names(my_metadata$hazard, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "Hazard"))

    foo <- tibble(my_name = names(my_metadata$population)) %>%
        left_join(my_map)
    scope$population <- set_names(my_metadata$population, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "Population"))


    ## Data background

    data_bkgrnd <- my_metadata$data_bckgrnd %>%
        mutate(my_name = paste(.data$Topic, .data$`Detailed metadata concept`, sep = "-")) %>%
        left_join(my_map) %>%
        filter(.data$out != "NA") %>%
        select("out", "Data") %>%
        spread("out", "Data") %>%
        as.list()

    data_bkgrnd$eClass <- paste0(this_eClass, "DataBackground")

    foo <- tibble(my_name = names(my_metadata$study_sample)) %>%
        left_join(my_map)
    data_bkgrnd$studySample <- set_names(my_metadata$study_sample, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "StudySample"))

    foo <- tibble(my_name = names(my_metadata$dietary)) %>%
        left_join(my_map)

    data_bkgrnd$dietary <- set_names(my_metadata$dietary, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "DietaryMethod"))

    foo <-  tibble(my_name = names(my_metadata$lab)) %>%
        left_join(my_map)

    data_bkgrnd$laboratory <- set_names(my_metadata$lab, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "Laboratory"))


    foo <- tibble(my_name = names(my_metadata$assay)) %>%
        left_join(my_map)
    data_bkgrnd$assay <- set_names(my_metadata$assay, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "Assay"))

    ## Model math

    model_math <- my_metadata$model_math %>%
        mutate(my_name = paste(.data$Topic, .data$`Detailed metadata concept`, sep = "-")) %>%
        left_join(my_map) %>%
        filter(.data$out != "NA") %>%
        select("out", "Data") %>%
        spread("out", "Data") %>%
        as.list()

    model_math$eClass <- paste0(this_eClass, "ModelMath")

    model_math$quality <- tibble(RMSE = NA, Rsquare = NA, AIC = NA, BIC = NA)  # TBD

    foo <- tibble(my_name = names(my_metadata$equation)) %>%
        left_join(my_map)
    model_math$equation <- set_names(my_metadata$equation, foo$out)

    foo <- tibble(my_name = names(my_metadata$parameters)) %>%
        left_join(my_map)
    model_math$parameter <- set_names(my_metadata$parameters, foo$out) %>%
        mutate(eClass = paste0(this_eClass, "Parameter"))

    ## Return the stuff

    list(
        version = this_version,
        generalInformation = general_info,
        scope = scope,
        dataBackground = data_bkgrnd,
        modelMath = model_math
    )

}



























































