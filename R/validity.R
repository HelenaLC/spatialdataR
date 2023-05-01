#' @importFrom methods is
.validateSpatialData <- function(obj) {
    msg <- NULL
    is_ia <- \(.) is(., "ImageArray")
    is_la <- \(.) is(., "LabelArray")
    is_df <- \(.) is(., "DFrame")
    is_r6 <- \(.) is(., "R6")
    if (length(obj$images)) {
        if (!all(vapply(obj$images, is_ia, logical(1))))
            msg <- c(msg, "'images' should be a list of 'ImageArray's")
    }
    if (length(obj$labels)) {
        if (!all(vapply(obj$labels, is_la, logical(1))))
            msg <- c(msg, "'labels' should be a list of 'LabelArray's")
    }
    if (length(obj$shapes)) {
        if (!all(vapply(obj$shapes, is_df, logical(1))))
            msg <- c(msg, "'shapes' should be a list of 'ShapeFrame's")
    }
    if (length(obj$points)) {
        if (!all(vapply(obj$points, is_r6, logical(1))))
            msg <- c(msg, "'points' should be a list of 'ArrowObject's")
    }
    if (length(msg))
        return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)
