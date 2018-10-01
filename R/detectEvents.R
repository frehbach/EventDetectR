#' detectEvents in a given data.frame
#'
#' detectEvents builds a prediction model (edObject) on the first 'windowSize' points of the given data x.
#' The next nIterationRefit data-points are classified as Event or not.
#' The window is moved iteratively and the next models are fitted.
#' The first windowSize points will always be classified as no Event and should only contain 'clean' data
#'
#' @param x data.frame, data which shall be classified as event or not
#' @param windowSize amount of data points to consider in each prediction model
#' @param nIterationsRefit amount of points into the future which will be predicted without fitting a new model.
#' E.g. if nIterationsRefit = 5 then the next five dataPoints are classified without refitting.
#' @param verbosityLevel Print output of function progress. 0 -> No output,
#' 1 -> every 100th model building iteration, 2 -> every 10th, 3 -> every iteration
#' @param dataPrepators string or vector of strings, that defines which preparators to use.
#' Lists are not accepted. Usage Example: dataPreparators = "ImputeTSInterpolation" results in the usage of
#' imputeTS::na.interpolation as a data preparator. All possible preparators are listed via:
#' getSupportedPreparations()
#' @param dataPreparationControl list, control-list containing all additional parameters that shall be passed
#' to the dataPreparators.
#' @param buildModelAlgo string, model name to be used. All possible preparators
#' are listed via: getSupportedModels().
#' @param buildModelControl list, control-list containing all additional parameters that shall be passed
#' to the modelling algo.
#' @param postProcessors string or vector of strings, that defines which postProcessors to use.
#' Lists are not accepted. Usage Example: postProcessors = "bedAlgo" results in the usage of
#' bed as a event postProcessing tool. All possible preparators are listed via:
#' getSupportedPostProcessors()
#' @param postProcessorControl list, control-list containing all additional parameters that shall be passed
#' to the postProcessirs.
#' @param ignoreVarianceWarning Ignores the continously appearing warning for missing variance in some variable
#' columns given a smaller windowSize
#'
#' @return edsResults edObject, list of results. $classification -> data.frame containing the T/F event classification
#' @export
#'
#' @examples
#' \dontrun{
#' ## Run event detection with default settings:
#' def <- detectEvents(x = stationBData[1:100,-1])
#'
#' ## Only refit the model after every 50th new datapoint,
#' ## have someoutput with verbosityLevel = 2 and ignore
#' ## the variance warning
#' ed <- detectEvents(stationBData[1000:2000,-1],nIterationsRefit = 50,
#'                    verbosityLevel = 2,ignoreVarianceWarning = TRUE)
#'
#' ## Switch to another model: Arima
#' ed2 <- detectEvents(stationBData[1000:2000,-1],nIterationsRefit = 50,
#'                     verbosityLevel = 2,ignoreVarianceWarning = TRUE,
#'                     buildModelAlgo = "ForecastArima")}
detectEvents <- function(x,
                         windowSize = 100,
                         nIterationsRefit = 50,
                         verbosityLevel = 0,
                         dataPrepators = "ImputeTSInterpolation",
                         dataPreparationControl = list(),
                         buildModelAlgo = "ForecastETS",
                         buildModelControl = list(),
                         postProcessors = "bedAlgo",
                         postProcessorControl = list(),
                         ignoreVarianceWarning = TRUE) {
    if(is.null(x)){
        stop("detectEvents: no data was specified for variable x")
    }
    if(!is.data.frame(x)){
        stop("detectEvents: x has to be a data.frame")
    }
    if(any(is.nan(unlist(x)))){
        stop("detectEvents: The specified data for x contained NaNs")
    }
    if(!all(apply(x,2,is.numeric))){
        stop("detectEvents: one or more columns in x contain non-numeric data")
    }
    if(is.null(windowSize)){
        stop("detectEvents: windowSize can't be NULL")
    }
    if(!is.numeric(windowSize)){
        stop("detectEvents: windowSize must be integer")
    }
    if(is.nan(windowSize)){
        stop("detectEvents: windowSize must be integer")
    }
    if(windowSize < 5){
        stop("detectEvents: windowSize too small, minimum size is 5")
    }
    if(nrow(x) < windowSize){
        stop("detectEvents: The windowSize can't exceed the amount of data given in x")
    }
    if(is.null(nIterationsRefit)){
        stop("detectEvents: nIterationsRefit can't be NULL")
    }
    if(!is.numeric(nIterationsRefit)){
        stop("detectEvents: nIterationsRefit must be integer")
    }
    if(is.nan(nIterationsRefit)){
        stop("detectEvents: nIterationsRefit must be integer")
    }
    if(nIterationsRefit < 1){
        stop("detectEvents: nIterationsRefit too small, minimum is 1")
    }
    if(is.null(verbosityLevel)){
        stop("detectEvents: verbosityLevel can't be NULL")
    }
    if(!is.numeric(verbosityLevel)){
        stop("detectEvents: verbosityLevel must be numeric")
    }
    if(is.nan(verbosityLevel)){
        stop("detectEvents: verbosityLevel must be numeric")
    }
    if(verbosityLevel < 0){
        stop("detectEvents: verbosityLevel too small, minimum is 0")
    }


    classification <- NULL
    Event <- rep(FALSE,windowSize)
    classification <- cbind(x[1:windowSize,,drop=F],Event)

    index <- 0
    verbosityCounter <- 0
    edModel <- NULL
    while(windowSize + index < nrow(x)){
        if(verbosityLevel > 0){
            verbosityCounter <- verbosityCounter + 1
            if((verbosityCounter %% (10^(3-as.integer(verbosityLevel))) == 0) || verbosityLevel > 3){
                print(paste0("EDS is working on index: ",index + nIterationsRefit))
            }
        }
        edModel <- buildEDModel(x[index:(index + windowSize),,drop=FALSE],dataPrepators,dataPreparationControl,
                                buildModelAlgo, buildModelControl,
                                postProcessors, postProcessorControl, ignoreVarianceWarning, edModel)
        newData <- x[(index + windowSize + 1):min(index + windowSize + nIterationsRefit, nrow(x)),,drop=FALSE]
        p <- predict(edModel,newData)$lastPredictedEvents
        classification <- rbind(classification, p)
        index <- index + nIterationsRefit
    }
    edModel$classification <- classification
    class(edModel) <- "edObject"
    return(edModel)
}

#' Print an Event Detection Object
#'
#' Prints the last classification results for an event detection object.
#' If 'nLast' (integer) is given, it specifies the amount of rows to be printed.
#'
#' @param x edObject, the event detection object that shall be printed
#' @param ... any additional parameters
#'
#' @export
print.edObject <- function(x, ...){
    args <- list(...)
    nModels <- length(x$modelList)
    if(nModels > 1){
        writeLines(paste0("Event Detection Object with ", nModels, " "
                     , class(x$modelList[[1]])[1], " submodels"))
    }else if(nModels == 1){
        writeLines(paste0("Event Detection Object with 1 "
                     , class(x$modelList[[1]])[1], " submodel"))
    }else{
        writeLines("Event Detection Object with no fitted models")
    }

    if(is.null(args$nLast)){
        nLast <- 10
    }else{
        nLast <- args$nLast
    }
    print(tail(x$classification, nLast))
}
