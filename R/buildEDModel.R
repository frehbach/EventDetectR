#' build Event Detection Model
#'
#' Builds an event detection object (edObject) containing all models and configurations that are used
#' to detect events in given data.
#'
#' @param x data.frame containing initial data on which the model will be fitted.
#' Data should be free of events. The data should not include a timestamp column
#' @param dataPrepators string or vector of strings, that defines which preparators to use.
#' Lists are not accepted. Usage Example: dataPreparators = "ImputeTSInterpolation" results in the usage of
#' imputeTS::na.interpolation as a data preparator. All possible preparators are listed via:
#' getSupportedPreparations()
#' Can also be set to NULL in order to shut off data preparation
#' @param dataPreparationControl list, control-list containing all additional parameters that shall be passed
#' to the dataPreparators.
#' @param buildModelAlgo string, model name to be used. All possible preparators
#' are listed via: getSupportedModels().
#' @param buildForecastModelControl list, control-list containing all additional parameters that shall be passed to forecast modeling algorithm
#' @param buildNeuralNetModelControl list, control-list containing all additional parameters that shall be passed to the neuralnet modeling algorithm
#' @param postProcessors string or vector of strings, that defines which postProcessors to use.
#' Lists are not accepted. Usage Example: postProcessors = "bedAlgo" results in the usage of
#' bed as a event postProcessing tool. All possible preparators are listed via:
#' getSupportedPostProcessors()
#' Can also be set to NULL in order to shut off data postProcessing
#' @param postProcessorControl list, control-list containing all additional parameters that shall be passed
#' to the postProcessirs.
#' @param ignoreVarianceWarning Ignores the continously appearing warning for missing variance in some variable
#' columns given a smaller windowSize
#' @param oldModel If another model was previously fitted it can be passed to the next model fit.
#' By doing so the eventHistory is preserved
#'
#' @return model, event detection object (edObject) containing all models and configurations that are used
#' to detect events in given data.
#' @export
#'
#' @import imputeTS
#'
#' @examples
#'
#' ## build a simple event detection model with standard configuration
#' x <- stationBData[100:200,-1]
#' buildEDModel(x,ignoreVarianceWarning = TRUE)
#'
#' ## Set up a more complex event detection model defining some additional configuration
#' buildEDModel(x, buildModelAlgo = "ForecastArima",ignoreVarianceWarning = TRUE)
#'
#'  ## Set up a multivariate neuralnetwork model
#' buildEDModel(x, buildModelAlgo = "NeuralNetwork",ignoreVarianceWarning = TRUE)
buildEDModel <- function(x,
                         dataPrepators = "ImputeTSInterpolation",
                         dataPreparationControl = list(),
                         buildModelAlgo = "ForecastETS",
                         buildForecastModelControl = list(),buildNeuralNetModelControl = list(),
                         postProcessors = "bedAlgo",
                         postProcessorControl = list(),
                         ignoreVarianceWarning = FALSE,
                         oldModel = NULL){

    ## Input Control Defauls ------------------
    ##
    con <- getDefaultPreparationControl()
    con[names(dataPreparationControl)] <- dataPreparationControl
    dataPreparationControl <- con
    rm(con)

    con <- getDefaultForecastModelControl()
    con[names(buildForecastModelControl)] <- buildForecastModelControl
    buildForecastModelControl <- con
    rm(con)

    con <- getDefaultNeuralNetModelControl()
    con[names(buildNeuralNetModelControl)] <- buildNeuralNetModelControl
    buildNeuralNetModelControl <- con
    rm(con)

    con <- getDefaultPostControl()
    con[names(postProcessorControl)] <- postProcessorControl
    postProcessorControl <- con
    rm(con)

    ## InputChecks and Available Preparators, Models, PostProcessors ----
    ##      General Checks for Correct Data Types -----------------------
    ##
    if(is.null(x)){
        stop("no data was specified for variable x")
    }
    if(!is.data.frame(x)){
        stop("x has to be a data.frame")
    }
    if(any(is.nan(unlist(x)))){
        stop("The specified data for x contained NaNs")
    }
    if(!all(apply(x,2,is.numeric))){
        stop("one or more columns in x contain non-numeric data")
    }
    if(!is.null(dataPrepators)){
        if(!typeof(dataPrepators) == "character"){
            stop("dataPreparators has to be of type character or vector of character")
        }
    }
    if(!is.null(postProcessors)){
        if(!typeof(postProcessors) == "character"){
            stop("postProcessors has to be of type character or vector of character")
        }
    }

    ## Transform into timeseries object

    if(isTRUE(dataPreparationControl$useTimeSeriesFormat)){
        x <- ts(x)
    }

    ##      Lists of the supported Models/Pre-/Postprocessors ------------
    ##
    allSupportedPreparations <- getSupportedPreparations()
    allSupportedModels <- getSupportedModels()
    allSupportedPostProcessors <- getSupportedPostProcessors()

    ##      Check if modelAlgo is supported / does not have typos in it ----
    ##
    if(!(buildModelAlgo %in% unlist(allSupportedModels))){
      stop("The specified model is not supported, please check your input for 'buildModelAlgo'")
    }

    ##      Check each dataPreparator, multiple might be provided ----
    ##
    if(length(dataPrepators) <= 1 & !is.null(dataPrepators)){
        if(!(dataPrepators %in% unlist(allSupportedPreparations))){
            stop("The specified preparator is not supported, please check your input for 'dataPreparators'")
        }
    }else{
        for(p in dataPrepators){
            if(!(p %in% unlist(allSupportedPreparations))){
                stop("The specified preparator is not supported, please check your input for 'dataPreparators'")
            }
        }
    }

    ## -----------------------
    ##      Check each postProcessor, multiple might be provided
    ## -----------------------
    if(length(postProcessors) <= 1 & !is.null(postProcessors)){
        if(!(postProcessors %in% unlist(allSupportedPostProcessors))){
            warning("The specified preparator is not supported, please check your input for 'dataPreparators'")
        }
    }else{
        for(p in postProcessors){
            if(!(p %in% unlist(allSupportedPostProcessors))){
                stop("The specified preparator is not supported, please check your input for 'dataPreparators'")
            }
        }
    }

    ## -----------------------
    ##
    ## Run Data Preparators
    ##
    ## -----------------------
    if(!is.null(dataPrepators)){
        for(p in dataPrepators){
            if(p %in% allSupportedPreparations$supportedImputeTS){
                prepStr <- substr(p, nchar("ImputeTS") + 1, nchar(p))
                x <- preparator_imputeTS(x, prepStr, dataPreparationControl)
            }
            #%TODO Missing Add general code call for type 'other' preps
        }
    }


    ## Apply Normalization --------------

    ## Added by Sowmya
    ## Define minmax normalization method for neural network models

    minmax_normalize <- function(x) {
      return ((x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
    }
    ## Variables which have no variance will be removed!
    ## A warning is generated if this happens
    ##
    ## If all variables have 0 variance, an error is thrown
    removedVarNames <- NULL
    if(isTRUE(dataPreparationControl$useNormalization)){
        zeroVarianceVars <- (apply(x,2,sd) == 0)

        if(sum(zeroVarianceVars) == ncol(x)){
            stop("There is no variance in your data set, event detection is not possible")
        }else if(sum(zeroVarianceVars) > 0){
            if(!ignoreVarianceWarning){
                warning("Some of the variables in your data set contain no variance,
                        they will be ignored in the event detection process")
            }
            }
        removedVarNames <- list(colnames(x)[zeroVarianceVars])
        x <- x[,!zeroVarianceVars]
# Added by Sowmya
        min_x <- NULL
        max_x <- NULL
            if(buildModelAlgo=="NeuralNetwork"){

            for(i in 1:ncol(x)){
              min_x <- c(min_x,min(x[,i],na.rm = TRUE))
              max_x <- c(max_x,max(x[,i],na.rm = TRUE))
            }

            x <- as.data.frame(lapply(x, minmax_normalize))
        }
        else{
        x <- scale(x)
        }
    }

    ## -----------------------
    ##
    ## Build Model
    ##
    ## -----------------------
    if(buildModelAlgo %in% allSupportedModels$supportedUnivariateForeCastModels){
        modelStr <- substr(buildModelAlgo, nchar("Forecast") + 1, nchar(buildModelAlgo))
        model <- model_UnivariateForecast(x, modelStr, buildForecastModelControl)
    }
    #%TODO Missing Add general code call for type 'other' models
    ## Added by Sowmya --- Multivariate NeuralNetwork model
    if(buildModelAlgo %in% allSupportedModels$supportedMultivariateModels){

      model <- model_NeuralNetwork(x, buildNeuralNetModelControl)
    }

    model$oldModel <- oldModel

    ## -----------------------
    ##
    ## Add Postprocessor to model
    ##
    ## -----------------------
    if(!is.null(postProcessors)){
        for(p in postProcessors){
            if(p %in% allSupportedPostProcessors$other){
                postProcessingFunction <- get(p)
                model <- postProcessingFunction(model)
            }
        }
    }
    #%call to specified postprocessor. Function should accept the model and add
    # the postprocessor to it like $internal$postProcessing <- ...
    # Each predict function should then contain a call to the given postprocessor


    ## Add ControlLists to model -----
    ##
    ## ControlLists can then be called by each submodule
    model$userConfig$dataPreparationControl <- dataPreparationControl
    model$userConfig$buildNeuralNetModelControl <- buildNeuralNetModelControl
    model$userConfig$buildForecastModelControl <- buildForecastModelControl
    model$userConfig$postProcessorControl <- postProcessorControl

    ## Add Normalization Scale Factors to model ----
    ##
    if(buildModelAlgo!="NeuralNetwork")
      {
    model$buildModelAlgo <- "UnivariateForecast"
    model$normalization$scaleCenter <- attr(x,"scaled:center")
    model$normalization$scaleSD <- attr(x,"scaled:scale")
    }
    if((buildModelAlgo=="NeuralNetwork") && (isTRUE(dataPreparationControl$useNormalization)))
    {
    model$normalization$min_x <-min_x
    model$normalization$max_x <-max_x
    model$buildModelAlgo <- "NeuralNetwork"
    }
    ## Add remark for removed variables
    ##
    model$excludedVariables <- removedVarNames

    return(model)
}
