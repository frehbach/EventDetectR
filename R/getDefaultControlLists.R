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

#' Get Default Control List for ED - Model Building Algorithms
#'
#' @return defaultControlList List with named arguments, 1 for each default parameter
#' @keywords internal
getDefaultModelControl <- function() {
    defaultControlList <- list(

    )
}

#' Get Default Control List for ED - PostProcessors
#'
#' @return defaultControlList List with named arguments, 1 for each default parameter
#' @keywords internal
getDefaultPostControl <- function() {
    defaultControlList <- list(
        nStandardDeviationsEventThreshhold = 2
    )
}
