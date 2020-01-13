
#' Fitting Neuralnet Models
#' @details Multivariate Neuralnetwork model is formulated for each of the parameter using its own lagged values and rest of all parameters involved. For instance, considering 3 parameters 'p1', 'p2', 'p3', the value of 'p1' at time step 'i' is calclated as \cr
#' \code{p1[i] = p2[i] + p3[i] + p1[i-1]} \cr
#'
#' @param x data
#' @param control control list with settings
#' @return fitted multivariate neural network model
#' @keywords internal
#' @import neuralnet
model_NeuralNetwork <- function(x, control){
    model <- NULL
    modelList <- list()


    con <- getDefaultNeuralNetModelControl()
    con[names(control)] <- control
    control <- con
    rm(con)
hidden=control$hidden
threshold <- control$threshold
stepmax <- control$stepmax
rep <- control$rep
control$rep <- NULL
control$stepmax <- NULL
control$hidden <- NULL
control$threshold <- NULL

    for(i in 1:ncol(x)){

        past_data   <- x[1:(nrow(x)-1),i]
        model_input <- cbind(x[2:nrow(x),],past_data)
        model_input <- data.frame(model_input)
        n1<-names(model_input)
        nn_input <- as.formula(paste(n1[i],"~", paste(n1[!n1 %in% n1[i]], collapse = " + ")))
        nn_model <- neuralnet(nn_input,data=model_input,hidden=hidden,threshold=threshold, stepmax=stepmax,rep=rep,unlist(control))

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
            # Obtain past data from model
        previousStep <- tail(object$modelList[[i]]$data$past_data,n=dataLength)
        inputData <-  newData[, !(names(newData) %in% unlist(object$excludedVariables))]
        # Normalze inputData
        if(isTRUE(object$userConfig$dataPreparationControl$useNormalization)){
        min_x <- object$normalization$min_x
        max_x <- object$normalization$max_x
        for (j in 1:ncol(inputData)){
            inputData[,j] <- ((inputData[,j] - min_x[j]) / (max_x[j] - min_x[j]))
        }
}
        test_input <- data.frame(inputData[,-i],past_data=previousStep)
        # Predict with the neuralnet model
        computed_value <- predict(object$modelList[[i]],test_input)
        predictions[,i] <- as.data.frame(computed_value)[,1]
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
