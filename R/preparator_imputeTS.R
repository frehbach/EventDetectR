
#' Data Preparation with Impute TS
#'
#' @param x data to process
#' @param prepStr string of preparator name to use
#' @param control control parameters, extra settings
#'
#' @return x prepared data
#' @keywords internal
#'
#' @import imputeTS
preparator_imputeTS <- function(x, prepStr, control){
    ## Select Correct Preparator by String

    prep <- NULL
    if(prepStr == "Interpolation") prep <- imputeTS::na.interpolation
    if(prepStr == "Kalman") prep <- imputeTS::na.kalman
    if(prepStr == "LOCF") prep <- imputeTS::na.locf
    if(prepStr == "MA") prep <- imputeTS::na.ma
    if(prepStr == "Mean") prep <- imputeTS::na.mean
    if(prepStr == "Random") prep <- imputeTS::na.random
    if(prepStr == "Remove") stop("imputeTS::remove is currently not supported") #prep <- imputeTS::na.remove
    if(prepStr == "Replace") prep <- imputeTS::na.replace
    if(prepStr == "Seadec") stop("imputeTS::Seadec is currently not supported") #prep <- imputeTS::na.seadec
    if(prepStr == "Seasplit") stop("imputeTS::Seasplit is currently not supported") #prep <- imputeTS::na.seasplit

    control$x <- x
    do.call(prep, list("x" = x))
}
