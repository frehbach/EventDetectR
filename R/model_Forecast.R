
#' Fitting Forecast Models
#'
#' @param x data
#' @param strName name of the Forecast model that shall be fitted
#' @param control control list with settings
#'
#' @return fitted Forecast model
#' @keywords internal
#'
#' @import forecast
model_UnivariateForecast <- function(x, strName, control){
    ## Select Model by String
    model <- NULL
    modellingAlgo <- NULL

    #Real Models
    if(strName == "ETS") modellingAlgo <- forecast::ets
    if(strName == "Arima") modellingAlgo <- forecast::Arima
    if(strName == "Bats") modellingAlgo <- forecast::bats

    #Check if used model is a real model or a direct forecaster
    if(is.null(modellingAlgo)){
        model$isDirectForecast = T
    }else{
        model$isDirectForecast = F
    }

    #Direct Forecasters
    if(strName == "Holt") modellingAlgo <- forecast::holt
    if(strName == "Meanf") modellingAlgo <- forecast::meanf
    if(strName == "RWF") modellingAlgo <- forecast::rwf
    if(strName == "SplineF") modellingAlgo <- forecast::splinef
    if(strName == "Thetaf") modellingAlgo <- forecast::thetaf
    if(strName == "SES") modellingAlgo <- forecast::ses

    model$usedModellingAlgo <- modellingAlgo

    if(ncol(x) > 1){
        modelList <- list()
        for(i in 1:ncol(x)){
            control$y <- x[,i]
            modelList[[paste0("model",i)]] <- do.call(modellingAlgo,control)
        }
        model$modelList <- modelList
        class(model) <- "UnivariateForecast"
        return(model)
    }else{
        control$y <- x
        model$modelList <- do.call(modellingAlgo,control)
        class(model) <- "UnivariateForecast"
        return(model)
    }
}

#' Predict Univariate Models Forecast Package
#'
#' @param object fitted model that shall be predicted
#' @param newData data.frame with newData that is compared to the models forecast to judge if events occured or not
#' @param ... additional parameters
#'
#' @return predicted value
#' @import stats
#' @keywords internal
predict.UnivariateForecast <- function(object,newData = NULL, ...){
    ## How many points shall be predicted into the future? Default = 10
    if(!is.null(newData)){
        dataLength <- nrow(newData)
    }else{
        dataLength <- 10
    }

    ## Predict with each model in given modelList
    predictions <- matrix(, nrow=dataLength,ncol=length(object$modelList))
    for(i in 1:length(object$modelList)){
        if(object$isDirectForecast){
            predictions[,i] <- as.data.frame(do.call(object$usedModellingAlgo,
                                                     list(y = object$modelList[[i]]$x, h = dataLength)))[,1]
        }else{
            predictions[,i] <- as.data.frame(forecast(object$modelList[[i]], h = dataLength))[,1]
        }
    }

    object$predictions <- predictions

    ## If no newData is given, then only return the model predictions, no need for eventClassification
    if(is.null(newData)){
        return(predictions)
    }else{
        ## Call ED standard eventClassification Method
        object <- eventClassification(object,newData,...)
        Event <- object$lastPredictedEvents
        object$lastPredictedEvents <- cbind(newData,Event)
        return(object)
    }
}
