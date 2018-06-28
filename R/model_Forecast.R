
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
model_Forecast <- function(x, strName, control){
    if(ncol(data > 1)){
        modelList <- list()
        for(i in 1:ncol(data)){
            control$y <- data[,i]
            modelList[[i]] <- do.call(forecast::ets,control)
        }
        class(model) <- "UnivariateForecast"
        return(modelList)
    }else{
        control$y <- data
        model <- list(do.call(forecast::ets,control))
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
    ## Implementation missing
}
