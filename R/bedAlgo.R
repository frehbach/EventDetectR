#'  bedAlgo: postProcessors to classify events

#' @description bedAlgo provides the continuous probability of an event at every prediction timestep.
#'  The memory about the recent past event is automatically included to predict the future.
#'  An event is initiated when the BED probability exceeds the eventThreshold.
#'  The probability that the data represents expected normal behavior in 'n' trials is represented as \cr
#' \code{B(r;n,p)=(n!)/(r!(n-r)!) p^(r) q^((n-r))}\cr
#' where the 'n' trials is given by 'bedWindowSize', 'q' represents the probability that a trial succeeds and 'p' represents the probability that a trial fails as an event. We keep the value of both 'p' and 'q' as 0.5 and hence the equation is simplified to\cr
#'\code{B(r;n,p)=(n!)(r!(n-r)!)0.5^n}\cr
#' The advantage of this BED is that it helps in reducing the false alarm, while the disadvantage is the slight delay in identifying the true event
#'
#' @param model model to which the postprocessor shall be added
#' @return model model with added postprocessing results
#' @keywords internal
bedAlgo <- function(model){

    model$internal$postProcessing <- function(model, events){
        #return(events)
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
        BEDWindowSize <- model$userConfig$postProcessorControl$bedWindowSize
        BEDProbList <- NULL
        #baselinePositions <- NULL
        #baselineCounter <- 0
        #baselineLimit <- postProcessorControl$baselineLimit
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
