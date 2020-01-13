
#' qualityStatistics
#'
#' Wrapper function for caret::confusionMatrix.
#' qualityStatistics calculates statistics for judging the quality of the eventDetection based on the
#' fitted edModel and a reference dataset
#'
#' @param edObject The eventdetection object you obtain by running 'detectEvents'
#' @param reference true/false vector, reference vector based on labeled data: which datapoints are real events.
#'
#' @return list, Confusion Matrix and Statistics
#' @export
#'
#' @examples
#' train <- geccoIC2018Train[15000:17000,]
#' edObject <- detectEvents(train[,-c(1,11)],windowSize = 1000,
#'                 nIterationsRefit = 500,verbosityLevel = 2,
#'                 postProcessorControl = list(nStandardDeviationseventThreshold = 3))
#' qualityStatistics(edObject, train$EVENT)
qualityStatistics <- function(edObject, reference){
    if (!requireNamespace("caret", quietly = TRUE)) {
        stop("Package \"caret\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (!requireNamespace("e1071", quietly = TRUE)) {
        stop("Package \"e1071\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    return(caret::confusionMatrix(factor(edObject$classification$Event),
                                  reference = factor(reference),
                                  positive = "TRUE"))
}
