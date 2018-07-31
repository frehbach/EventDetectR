#' Generate a Package EventDetectR specific Error
#'
#' @param strError Message to print
#'
#' @keywords internal
edError <- function(strError){
    stop(paste0("Pkg EventDetectR error: ",strError))
}

#' Generate a Package EventDetectR specific Warning
#'
#' @param strError Message to print
#'
#' @keywords internal
edWarning <- function(strError){
    warning(paste0("Pkg EventDetectR warning: ",strError))
}
