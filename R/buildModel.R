
#' build Event Detection Model
#'
#' @param x data
#' @param dataPrepators string or vector of strings, which preparators to use
#' @param dataPreparationControl control list for data preparators
#' @param buildModelAlgo string name of modelling algo
#' @param buildModelControl control list for modelling algo
#'
#' @return model fittedModel
#' @export
#'
#' @import imputeTS
buildEDModel <- function(x,
                         dataPrepators = "ImputeTSInterpolation",
                         dataPreparationControl = list(),
                         buildModelAlgo = "ForecastETS",
                         buildModelControl = list()){

    ##InputCheck
    ##
    ##
    allSupportedPreparations <- list(
        supportedImputeTS <- c("ImputeTSInterpolation")
    )
    allSupportedModels <- list(
        supportedUnivariateForeCastModels <- c("ForecastETS")
    )
    if(!(buildModelAlgo %in% unlist(allSupportedModels))){
      stop("The specified model is not supported, please check your input for 'buildModelAlgo'")
    }
    if(!(dataPrepators %in% unlist(allSupportedPreparations))){
        stop("The specified preparator is not supported, please check your input for 'dataPreparators'")
    }


    ## Data Preparators & NA Handling
    ##
    if(dataPrepators %in% supportedImputeTS){
        prepStr <- substr(dataPrepators, nchar("ImputeTS") + 1, nchar(dataPrepators))
        x <- preparator_imputeTS(x, prepStr, dataPreparationControl)
    }

    ## Model Fitting
    ##
    if(buildModelAlgo %in% supportedUnivariateForeCastModels){
        modelStr <- substr(buildModelAlgo, nchar("Forecast") + 1, nchar(buildModelAlgo))
        model <- model_UnivariateForecast(x, modelStr, buildModelControl)
    }

    return(model)
}
