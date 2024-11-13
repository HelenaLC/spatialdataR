#' @importFrom methods is
.validateSpatialData <- \(x) {
    typ <- c(
        images="ImageArray", 
        labels="LabelArray", 
        points="PointFrame", 
        shapes="ShapeFrame", 
        tables="SingleCellExperiment")
    msg <- NULL
    for (. in names(typ)) if (length(x[[.]])) 
        if (!all(vapply(x[[.]], \(y) is(y, typ[.]), logical(1))))
            msg <- c(msg, sprintf("'%s' should be a list of '%s'", ., typ[.]))
    if (length(msg))
        return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)
