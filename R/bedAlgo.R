#' Dummy BED algo, has to be changed
#'
#' @param model model to which the postprocessor shall be added
#'
#' @return model model with added postprocessing step
#' @keywords internal
bedAlgo <- function(model){
    model$internal$postProcessing <- function(model, events){
        hist <- events
        hist[1:length(hist)] <- F
        if(!is.null(model$oldModel)){
            hist <- model$oldModel$eventHistory
        }

        nEvents <- length(events)
        combinedEventVector <- c(hist, events)

        eventThreshhold <- 4#30
        windowSize <- min(10, nEvents)#40

        realEvents <- rep(F,nEvents)

        for(i in (nEvents + 1):length(combinedEventVector)){
            if((!is.na(combinedEventVector[i])) && combinedEventVector[i]){
                realEvents[i-nEvents] <- (sum(combinedEventVector[(i-windowSize):i]) > eventThreshhold)
            }
        }

        return(realEvents)
    }
    return(model)
}
