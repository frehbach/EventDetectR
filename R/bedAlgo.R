#' Dummy BED algo, has to be changed
#'
#' @param model model to which the postprocessor shall be added
#'
#' @return model model with added postprocessing step
#' @keywords internal
bedAlgo <- function(model){
    model$postProcessing <- function(model, events){
        nEvents <- length(events)
        hist <- model$eventHistory
        lenHist <- length(hist)

        eventThreshhold <- 4
        windowSize <- 10

        realEvents <- rep(F,nEvents)

        for(i in 1:nEvents){
            realEvents[i] <- sum(model$eventHistory[(max(1,lenHist-i+1 - windowSize)):(lenHist-i+1)]) > 1
        }

        return(realEvents)
    }
    return(model)
}
