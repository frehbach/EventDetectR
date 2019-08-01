
#'  bedAlgo provides the continuous probability of an event at every prediction timestep.
#'  The memory about the recent past event is automatically included to predict the future.
#'  An event is initiated when the BED probability exceeds the eventThreshhold.
#' @author Sowmya
#' @param model model to which the postprocessor shall be added
#'
#' @return model model with added postprocessing results
#' @keywords internal
bedAlgo <- function(model){

    model$internal$postProcessing <- function(model, events){
         hist <- events
         hist[1:length(hist)] <- F
         if(!is.null(model$oldModel)){
            hist <- model$oldModel$eventHistory
         }
        postProcessorControl <- model$userConfig$postProcessorControl
        con <- getDefaultPostControl()
        con[names(postProcessorControl)] <- postProcessorControl
        postProcessorControl <- con
        rm(con)

        nEvents <- length(events)# outlier list

        eventThreshold <- postProcessorControl$eventThreshold ## a probability value from 0 to 1
        BEDWindowSize <- nEvents
        BEDProbList <- NULL
        baselinePositions <- NULL
        baselineCounter <- 0
        baselineLimit <- postProcessorControl$baselineLimit
        realEvents <- rep(F,nEvents)



for (k in 1:nEvents)
{
    combinedEventVector <-  c(hist, events[1:k])
    combinedEventVector <- tail(combinedEventVector,n=BEDWindowSize)
    r <- length(combinedEventVector[combinedEventVector==T])
    p<-0
      for(i in 1:r){
            p=p+factorial(BEDWindowSize)/factorial(BEDWindowSize-i)/factorial(i)*0.5^BEDWindowSize
      }
    BEDProbList<-c(BEDProbList,p)
    realEvents[k] <- (BEDProbList[(k)]> eventThreshold)
}
        return(realEvents)
    }
    return(model)
}
