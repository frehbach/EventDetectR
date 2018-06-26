
#' build Event Detection Model
#'
#' @param data
#' @param control
#'
#' @return
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
        model <- preparator_imputeTS(data, modelStr, dataPreparationControl)
    }

    ## Model Fitting
    ##
    if(buildModelAlgo %in% supportedForeCastModels){
        substr(buildModelAlgo, nchar("Forecast") + 1, nchar(buildModelAlgo))
        model <- model_Forecast(data, modelStr, buildModelControl)
    }

    return(model)
}
