#' Get Default Control List for ED - Data Preparators
#'
#' @return defaultControlList List with named arguments, 1 for each default parameter
#' @keywords internal
getDefaultPreparationControl <- function() {
    defaultControlList <- list(
        useNormalization = TRUE,
        useTimeSeriesFormat = TRUE
    )
}

#' Get Default Control List for ED - Forecast Model Building Algorithms
#'
#' @return defaultControlList List with named arguments, 1 for each default parameter
#' @keywords internal
getDefaultForecastModelControl <- function() {
    defaultControlList <- list(
    )
}

#' Get Default Control List for ED - NN Model Building Algorithms
#'
#' @return defaultControlList List with named arguments, 1 for each default parameter
#' @keywords internal
getDefaultNeuralNetModelControl <- function() {
    defaultControlList <- list(
        nn_hiddenlayers = 3,
        nn_threshold=0.01,
        nn_stepmax=1e+05,
        nn_rep=1,
        nn_startweights=NULL,nn_learningrate.limit = NULL, nn_learningrate.factor = list(minus = 0.5,plus = 1.2), nn_learningrate = NULL, nn_lifesign = "none",
        nn_lifesign.step = 1000, nn_algorithm = "rprop+", nn_err.fct = "sse",
        nn_act.fct = "logistic", nn_linear.output = TRUE, nn_exclude = NULL,
        nn_constant.weights = NULL, nn_likelihood = FALSE

    )
}

#' Get Default Control List for ED - PostProcessors
#'
#' @return defaultControlList List with named arguments, 1 for each default parameter
#' @keywords internal
getDefaultPostControl <- function() {
    defaultControlList <- list(
        nStandardDeviationsEventThreshhold = 2,
        eventThreshold = .7,
        bedWindowSize = 30
    )
}
