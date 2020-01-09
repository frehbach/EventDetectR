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
        hidden = 1,
        algorithm="rprop+",
        threshold=0.01,
        stepmax=1e+05,
        linear.output=TRUE,
        rep=1
    )
}

#' Get Default Control List for ED - PostProcessors
#'
#' @return defaultControlList List with named arguments, 1 for each default parameter
#' @keywords internal
getDefaultPostControl <- function() {
    defaultControlList <- list(
        nStandardDeviationseventThreshold = 2,
        eventThreshold = .7,
        bedWindowSize = 30
    )
}
