#' @importFrom methods is
.validateSpatialData <- function(obj) {
    msg <- NULL
    is_ia <- \(.) is(., "ImageArray")
    is_la <- \(.) is(., "LabelArray")
    is_sf <- \(.) is(., "ShapeFrame")
    is_pf <- \(.) is(., "PointFrame")
    if (length(obj$images)) {
        if (!all(vapply(obj$images, is_ia, logical(1))))
            msg <- c(msg, "'images' should be a list of 'ImageArray's")
    }
    if (length(obj$labels)) {
        if (!all(vapply(obj$labels, is_la, logical(1))))
            msg <- c(msg, "'labels' should be a list of 'LabelArray's")
    }
    if (length(obj$shapes)) {
        if (!all(vapply(obj$shapes, is_sf, logical(1))))
            msg <- c(msg, "'shapes' should be a list of 'ShapeFrame's")
    }
    if (length(obj$points)) {
        if (!all(vapply(obj$points, is_pf, logical(1))))
            msg <- c(msg, "'points' should be a list of 'PointFrame's")
    }
    if (length(msg))
        return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)
