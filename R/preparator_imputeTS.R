
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
    if(prepStr == "Interpolation") prep <- imputeTS::na_interpolation
    if(prepStr == "Kalman") prep <- imputeTS::na_kalman
    if(prepStr == "LOCF") prep <- imputeTS::na_locf
    if(prepStr == "MA") prep <- imputeTS::na_ma
    if(prepStr == "Mean") prep <- imputeTS::na_mean
    if(prepStr == "Random") prep <- imputeTS::na_random
    if(prepStr == "Remove") stop("imputeTS::remove is currently not supported") #prep <- imputeTS::na_remove
    if(prepStr == "Replace") prep <- imputeTS::na_replace
    if(prepStr == "Seadec") stop("imputeTS::Seadec is currently not supported") #prep <- imputeTS::na_seadec
    if(prepStr == "Seasplit") stop("imputeTS::Seasplit is currently not supported") #prep <- imputeTS::na_seasplit

    control$x <- x
    do.call(prep, list("x" = x))
}
