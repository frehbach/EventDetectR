
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
                         dataPrepators = c("ImputeTSInterpolation","uzvu","vueu"),
                         dataPreparationControl = list(),
                         buildModelAlgo = "ForecastETS",
                         buildModelControl = list()){

    ##InputCheck
    ##
    ##
    ##
    ##
    allSupportedPreparations <- list(
        supportedImputeTS <- c("ImputeTSInterpolation")
    )
    allSupportedModels <- list(
        supportedForeCastModels <- c("ForecastETS")
    )
    if(!(buildModelAlgo %in% unlist(allSupportedModels))){
      stop("The specified model is not supported, please check your input for 'buildModelAlgo'")
    }


    ## Data Preparators & NA Handling
    ##
    if(dataPrepators %in% supportedImputeTS){
        substr(dataPrepators, nchar("ImputeTS") + 1, nchar(dataPrepators))
        model <- preparator_imputeTS(data, dataPreparationControl)
    }

    ## Model Fitting
    ##
    if(buildModelAlgo %in% supportedForeCastModels){
        substr(buildModelAlgo, nchar("Forecast") + 1, nchar(buildModelAlgo))
        model <- model_Forecast(data, modelStr, buildModelControl)
    }

    return(model)
}
