
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
#' @param ... additional parameters
#'
#' @return predicted value
#' @import stats
#' @export
predict.UnivariateForecast <- function(object,...){
    predictions <- matrix(, nrow=10,ncol=length(object$modelList))
    for(i in 1:length(object$modelList)){
        predictions[,i] <- as.data.frame(predict(object$modelList[[i]]))[,2]
    }
    predictions
}
