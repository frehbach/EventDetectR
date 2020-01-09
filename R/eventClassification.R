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
    if(length(object$excludedVariables[[1]]) > 0){
        removeVars <- which(colnames(newData) %in% unlist(object$excludedVariables))
        newData <- newData[,-removeVars]
    }
    if(!(all(dim(object$predictions) == dim(newData)))){
        stop("Predictions dimensions do not match newData dimensions when trying to classify events")
    }

    ## Apply Normalization -----

    ##


    if(isTRUE(object$userConfig$dataPreparationControl$useNormalization)){
        if(object$buildModelAlgo!="NeuralNetwork")
        {
        newData <- scale(newData,center = object$normalization$scaleCenter,
                         scale = object$normalization$scaleSD)}
        if(object$buildModelAlgo=="NeuralNetwork")
        {
min_x <- object$normalization$min_x
max_x <- object$normalization$max_x
for (i in 1:ncol(newData)){
newData[,i] <- ((newData[,i] - min_x[i]) / (max_x[i] - min_x[i]))
}
        }
    }

    ## Calculate residuals from newData and predictions
    residuals <- abs(newData - object$predictions)

    events <- apply((residuals > object$userConfig$postProcessorControl$nStandardDeviationseventThreshold),1,any)
    object$eventHistory <- c(object$eventHistory, events)

    ## Call postprocessing interface with the calculated predictions
    ## Return event detection results
    object$lastPredictedEvents <- object$internal$postProcessing(object, events)
    return(object)
}



