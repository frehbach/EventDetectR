
#' Data Preparation with Impute TS
#'
#' @param x data to process
#' @param control control parameters, extra settings
#'
#' @return x prepared data
#' @export
#'
#' @import imputeTS
preparator_imputeTS <- function(x, control){
    control$x <- x
    do.call(imputeTS::na.interpolation, control)
}
