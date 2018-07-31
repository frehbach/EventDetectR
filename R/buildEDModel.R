
#' build Event Detection Model
#'
#' @param x data
#' @param dataPrepators string or vector of strings, which preparators to use, !no list!
#' @param dataPreparationControl control list for data preparators
#' @param buildModelAlgo string name of modelling algo
#' @param buildModelControl control list for modelling algo
#' @param postProcessors string name of one or more postprocessors, !no list!
#' @param postProcessorControl control list for postprocessors
#'
#' @return model fittedModel
#' @export
#'
#' @import imputeTS
buildEDModel <- function(x,
                         dataPrepators = "ImputeTSInterpolation",
                         dataPreparationControl = list(),
                         buildModelAlgo = "ForecastETS",
                         buildModelControl = list(),
                         postProcessors = "bedAlgo",
                         postProcessorControl = list()){

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
        edError("no data was specified for variable x")
    }
    if(!is.data.frame(x)){
        edError("x has to be a data.frame")
    }
    if(!all(apply(x,2,is.numeric))){
        edError("one or more columns in x contain non-numeric data")
    }
    if(!typeof(dataPrepators) == "character"){
        edError("dataPreparators has to be of type character or vector of character")
    }
    if(!typeof(postProcessors) == "character"){
        edError("postProcessors has to be of type character or vector of character")
    }

    ##      Lists of the supported Models/Pre-/Postprocessors ------------
    ##
    allSupportedPreparations <- list(
        supportedImputeTS = c("ImputeTSInterpolation"),
        other = c()
    )
    allSupportedModels <- list(
        supportedUnivariateForeCastModels = c("ForecastETS"),
        other = c()
    )
    allSupportedPostProcessors <- list(
        ## List 'other' should contain items which are called by exact name through the
        ## 'get' function
        other = c("bedAlgo")
    )

    ##      Check if modelAlgo is supported / does not have typos in it ----
    ##
    if(!(buildModelAlgo %in% unlist(allSupportedModels))){
      edError("The specified model is not supported, please check your input for 'buildModelAlgo'")
    }

    ##      Check each dataPreparator, multiple might be provided ----
    ##
    if(length(dataPrepators) <= 1){
        if(!(dataPrepators %in% unlist(allSupportedPreparations))){
            edError("The specified preparator is not supported, please check your input for 'dataPreparators'")
        }
    }else{
        for(p in dataPrepators){
            if(!(p %in% unlist(allSupportedPreparations))){
                edError("The specified preparator is not supported, please check your input for 'dataPreparators'")
            }
        }
    }

    ## -----------------------
    ##      Check each postProcessor, multiple might be provided
    ## -----------------------
    if(length(dataPrepators) <= 1){
        if(!(dataPrepators %in% unlist(allSupportedPreparations))){
            edError("The specified preparator is not supported, please check your input for 'dataPreparators'")
        }
    }else{
        for(p in dataPrepators){
            if(!(p %in% unlist(allSupportedPreparations))){
                edError("The specified preparator is not supported, please check your input for 'dataPreparators'")
            }
        }
    }

    ## Apply Normalization --------------
    ##
    ## TODO: This should generate a warning if not enough variance exists in any variable so that scaling only results in NAs
    x <- scale(x)

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

    return(model)
}
