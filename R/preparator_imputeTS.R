
#' Data Preparation with Impute TS
#'
#' @param x data to process
#' @param control control parameters, extra settings
#'
#' @return x prepared data
#' @export
#'
#' @import imputeTS
preparator_imputeTS <- function(x, prepStr, control){
    ## Select Correct Preparator by String
    prep <- NULL
    if(prepStr == "Interpolation") prep <- imputeTS::na.interpolation

    control$x <- x
    do.call(imputeTS::na.interpolation, control)
}
