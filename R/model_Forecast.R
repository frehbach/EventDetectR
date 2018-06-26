
#' Title
#'
#' @param data
#' @param control
#'
#' @return
#' @export
#'
#' @import forecast
model_Forecast <- function(data, control){
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

predict.UnivariateForecast <- function(object){
    predictUnivariateForecast(object)
}
