
#' Fitting Neuralnet Models
#' @details The Neuralnetworks are used to model the multivariate parameters through non-linear, weighted, parametrized functions. A Neuralnetwork model is formulated for each of the parameter using its own lagged values and rest of all parameters involved.
#' @param x data
#' @param control control list with settings
#'
#' @return fitted multivariate neural network model
#' @keywords internal
#'@author Sowmya
#' @import neuralnet
model_NeuralNetwork <- function(x, control){
    model <- NULL
    modelList <- list()
    for(i in 1:ncol(x)){

        past_data   <- x[1:(nrow(x)-1),i]
        model_input <- cbind(x[2:nrow(x),],past_data)
        model_input <- data.frame(model_input)
        n1<-names(model_input)
        nn_input <- as.formula(paste(n1[i],"~", paste(n1[!n1 %in% n1[i]], collapse = " + ")))
        nn_model <- neuralnet(nn_input,data=model_input,linear.output = T,hidden=3,algorithm = "rprop+")

     modelList[[paste0("model",i)]] <-nn_model
        }
        model$modelList <- modelList
        class(model) <- "MultivariateNeuralNetwork"
        return(model)
    }



#' Predict MultivariateNeuralNetwork Models neuralnet Package
#'
#' @param object fitted model that shall be predicted
#' @param newData data.frame with newData that is compared to the models prediction to judge if events occured or not
#' @param ... additional parameters
#'
#' @return predicted value
#' @import stats
#' @keywords internal
#'
#'
#'



predict.NeuralNetwork <- function(object,newData = NULL, ...){
    ## How many points shall be predicted into the future? Default = 10
    if(!is.null(newData)){
        dataLength <- nrow(newData)

    ## Predict with each model in given modelList
    predictions <- matrix(, nrow=dataLength,ncol=length(object$modelList))
    for(i in 1:length(object$modelList)){
        #if((object$modelList[[i]])!=NULL)
        {
        previousStep <- tail(object$modelList[[i]]$data$past_data,n=1)
        inputData <-  newData[, !(names(newData) %in% unlist(object$excludedVariables))]
        test_input <- data.frame(inputData[,-i],past_data=previousStep)
        computed_value <- compute(object$modelList[[i]],test_input)
        predictions[,i] <- as.data.frame(computed_value$net.result)[,1]
}
    }

    object$predictions <- predictions


        ## Call ED standard eventClassification Method
        object <- eventClassification(object,newData,...)
        Event <- object$lastPredictedEvents
        object$lastPredictedEvents <- cbind(newData,Event)
        return(object)
    }
}
