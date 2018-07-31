#' Dummy BED algo, has to be changed
#'
#' @param model model to which the postprocessor shall be added
#'
#' @return model model with added postprocessing step
#' @export
bedAlgo <- function(model){
    model$postProcessing <- function(model, events){
        return(events)
    }
    return(model)
}
