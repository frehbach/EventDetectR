
#' Fitting Forecast Models
#'
#' @param x data
#' @param strName name of the Forecast model that shall be fitted
#' @param control control list with settings
#'
#' @return fitted Forecast model
#' @export
#'
#' @import forecast
model_UnivariateForecast <- function(x, strName, control){
    ## Select Model by String
    model <- NULL
    if(strName == "ETS") prep <- forecast::ets

    if(ncol(x) > 1){
        modelList <- list()
        for(i in 1:ncol(x)){
            control$y <- x[,i]
            modelList[[paste0("model",i)]] <- do.call(forecast::ets,control)
        }
        model$modelList <- modelList
        class(model) <- "UnivariateForecast"
        return(model)
    }else{
        control$y <- x
        model$modelList <- do.call(forecast::ets,control)
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
#' @export
predict.UnivariateForecast <- function(object,newData = NULL, ...){
    if(!is.null(newData)){
        dataLength <- nrow(newData)
    }else{
        dataLength <- 10
    }
    predictions <- matrix(, nrow=dataLength,ncol=length(object$modelList))
    for(i in 1:length(object$modelList)){
        predictions[,i] <- as.data.frame(forecast(object$modelList[[i]], h = dataLength))[,2]
    }

    object$predictions <- predictions

    ## If no newData is given, then only return the model predictions, no need for eventClassification
    if(is.null(newData)){
        return(predictions)
    }else{
        ## Call ED standard eventClassification Method
        Event <- eventClassification(object,newData,...)
        return(cbind(newData,Event))
    }
}
