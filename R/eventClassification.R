#' Predict Univariate Models Forecast Package
#'
#' @param object fitted model that shall be predicted
#' @param ... additional parameters
#'
#' @return eventDetector fittedModel with
#' @keywords internal
eventClassification <- function(object, newData, ...) {

    ##Error checks
    ##
    if(is.null(object$predictions)){
        stop("Predictions Object was empty when trying to classify events")
    }
    if(!(all(dim(object$predictions) == dim(newData)))){
        stop("Predictions dimensions do not match newData dimensions when tying to classify events")
    }

    ## Apply Normalization -----
    ##
    newData <- scale(newData,center = object$normalization$scaleCenter,
                     scale = object$normalization$scaleSD)


    residuals <- abs(newData - object$predictions)

    events <- apply((residuals > object$postProcessorControl$nStandardDeviationsEventThreshhold),1,any)
    object$eventHistory <- c(object$eventHistory, events)

    ## Call postprocessing interface with the calculated predictions
    ## Return event detection results
    events <- object$postProcessing(object, events)
    return(events)
}



