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
#' @param dataPreparationControl list, control-list containing all additional parameters that shall be passed
#' to the dataPreparators.
#' @param buildModelAlgo string, model name to be used. All possible preparators
#' are listed via: getSupportedModels().
#' @param buildModelControl list, control-list containing all additional parameters that shall be passed
#' to the modelling algo.
#' @param postProcessors string or vector of strings, that defines which postProcessors to use.
#' Lists are not accepted. Usage Example: postProcessors = "bedAlgo" results in the usage of
#' bed as a event postProcessing tool. All possible preparators are listed via:
#' getSupportedPostProcessors()
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
#' x <- stationBData[1:2000,-1]
#' buildEDModel(x)
#'
#' ## Set up a more complex event detection model defining some additional configuration
#' buildEDModel(x, dataPrepators = "ImputeTSMean", buildModelAlgo = "ForecastArima")
buildEDModel <- function(x,
                         dataPrepators = "ImputeTSInterpolation",
                         dataPreparationControl = list(),
                         buildModelAlgo = "ForecastETS",
                         buildModelControl = list(),
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

    con <- getDefaultModelControl()
    con[names(buildModelControl)] <- buildModelControl
    buildModelControl <- con
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
    if(!typeof(dataPrepators) == "character"){
        stop("dataPreparators has to be of type character or vector of character")
    }
    if(!typeof(postProcessors) == "character"){
        stop("postProcessors has to be of type character or vector of character")
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
    if(length(dataPrepators) <= 1){
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
    if(length(dataPrepators) <= 1){
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
    ##
    ## Run Data Preparators
    ##
    ## -----------------------
    for(p in dataPrepators){
        if(p %in% allSupportedPreparations$supportedImputeTS){
            prepStr <- substr(p, nchar("ImputeTS") + 1, nchar(p))
            x <- preparator_imputeTS(x, prepStr, dataPreparationControl)
        }
        #%TODO Missing Add general code call for type 'other' preps
    }

    ## Apply Normalization --------------
    ##
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
        x <- scale(x)
    }

    ## -----------------------
    ##
    ## Build Model
    ##
    ## -----------------------
    if(buildModelAlgo %in% allSupportedModels$supportedUnivariateForeCastModels){
        modelStr <- substr(buildModelAlgo, nchar("Forecast") + 1, nchar(buildModelAlgo))
        model <- model_UnivariateForecast(x, modelStr, buildModelControl)
    }
    #%TODO Missing Add general code call for type 'other' models

    ## -----------------------
    ##
    ## Add Postprocessor to model
    ##
    ## -----------------------
    for(p in postProcessors){
        if(p %in% allSupportedPostProcessors$other){
            postProcessingFunction <- get(p)
            model <- postProcessingFunction(model)
        }
    }
    #%call to specified postprocessor. Function should accept the model and add
    # the postprocessor to it like $postProcessing <- ...
    # Each predict function should then contain a call to the given postprocessor


    ## Add ControlLists to model -----
    ##
    ## ControlLists can then be called by each submodule
    model$dataPreparationControl <- dataPreparationControl
    model$buildModelControl <- buildModelControl
    model$postProcessorControl <- postProcessorControl

    ## Add Normalization Scale Factors to model ----
    ##
    model$normalization$scaleCenter <- attr(x,"scaled:center")
    model$normalization$scaleSD <- attr(x,"scaled:scale")

    ## Add remark for removed variables
    ##
    model$removedVariables <- removedVarNames

    ## Add information from oldModel
    if(!is.null(oldModel)){
        model$eventHistory <- oldModel$eventHistory
    }
    return(model)
}
