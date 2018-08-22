#' getSupportedPreparations
#'
#' Get a list of all data preparation methods that are currently supported in package 'eventDetectR'.
#'
#' @return allSupportedPreparations a list of strings with each supported method name.
#' The strings can be copied and used in calls to 'eventDetect' or 'buildEDModel'
#' @export
#'
#' @examples
#' preps <- getSupportedPreparations()
getSupportedPreparations <- function() {
    allSupportedPreparations <- list(
        supportedImputeTS = c("ImputeTSInterpolation",
                              "ImputeTSKalman",
                              "ImputeTSLOCF",
                              "ImputeTSMA",
                              "ImputeTSMean",
                              "ImputeTSRandom",
                              #"ImputeTSRemove",
                              "ImputeTSReplace"
                              #"ImputeTSSeadec",
                              #"ImputeTSSeasplit"
                              ),
        other = c()
    )
    return(allSupportedPreparations)
}

#' getSupportedModels
#'
#' Get a list of all data modelling methods that are currently supported in package 'eventDetectR'.
#'
#' @return allSupportedModels a list of strings with each supported method name. The strings can be copied and used in calls to 'eventDetect' or 'buildEDModel'
#' @export
#'
#' @examples
#' models <- getSupportedModels()
getSupportedModels <- function() {
    allSupportedModels <- list(
        supportedUnivariateForeCastModels = c("ForecastETS", "ForecastArima",
                                              "ForecastBats",
                                              "ForecastHolt",
                                              "ForecastMeanf",
                                              "ForecastRWF",
                                              "ForecastSplineF",
                                              "ForecastThetaf",
                                              "ForecastSES"
                                              ),
        other = c()
    )
    return(allSupportedModels)
}

#' getSupportedPostProcessors
#'
#' Get a list of all data postprocessing methods that are currently supported in package 'eventDetectR'.
#'
#' @return allSupportedPostProcessors a list of strings with each supported method name. The strings can be copied and used in calls to 'eventDetect' or 'buildEDModel'
#' @export
#'
#' @examples
#' preps <- getSupportedPostProcessors()
getSupportedPostProcessors <- function() {
    allSupportedPostProcessors <- list(
        other = c("bedAlgo")
    )
    return(allSupportedPostProcessors)
}
