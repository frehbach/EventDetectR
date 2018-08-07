#' detectEvents in a given data.frame
#'
#' detectEvents builds a prediction model on the first 'windowSize' points of the given data x.
#' The next nIterationRefit data-points are classified as Event or not.
#' The window is moved iteratively and the next models are fitted.
#' The first windowSize points will always be classified as no Event and should only contain 'clean' data
#'
#' @param x data to classify as T/F event
#' @param windowSize amount of data points to consider in each prediction model
#' @param nIterationsRefit amount of points into the future which will be predicted
#' @param verbosityLevel Print output of function progress. 0 -> No output, 1 -> every 100th model building iteration, 2 -> every 10th, 3 -> every iteration
#' @param dataPrepators string or vector of strings, which preparators to use, !no list!
#' @param dataPreparationControl control list for data preparators
#' @param buildModelAlgo string name of modelling algo
#' @param buildModelControl control list for modelling algo
#' @param postProcessors string name of one or more postprocessors, !no list!
#' @param postProcessorControl control list for postprocessors
#' @param ignoreVarianceWarning Ignores the continously appearing warning for missing variance in some variable columns given a smaller windowSize
#'
#' @return edsResults, list of results. $classification -> data.frame containing the T/F event classification
#' @export
#'
#' @examples
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
#'                     buildModelAlgo = "ForecastArima")
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
    return(edModel)
}

