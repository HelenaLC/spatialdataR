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
    for (y in labels(x)) .validateZattrsLabelArray(y)
    if (length(msg))
        return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)

.validateZattrsLabelArray <- \(x) {
    msg <- c()
    za <- meta(x)
    if (is.null(ms <- za$multiscales))
        msg <- c(msg, "missing 'multiscales'")
    if (!is.list(ax <- ms$axes))
        msg <- c(msg, "missing or non-list 'axes'")
    ax <- ax[[1]]
    if (!all(dim(ax) == 2))
        msg <- c(msg, "'axes' should have dim. 2x2")
    if (!is.list(ct <- ms$coordinateTransformations))
        msg <- c(msg, "missing or non-list 'coordinateTransformations'")
    ct <- ct[[1]]
    for (. in c("input", "output", "type")) if (is.null(ct[[.]]))
        msg <- c(msg, sprintf("'coordinateTransformations' missing '%s'", .))
    if (length(msg)) return(msg) else return(TRUE)
}
