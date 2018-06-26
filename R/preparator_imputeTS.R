
preparator_imputeTS <- function(x, control){
    control$x <- x
    do.call(imputeTS::na.interpolation, control)
}
