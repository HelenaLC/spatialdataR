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

.validateZattrs_multiscales <- \(x, mgs) {
    if (is.null(ms <- x$multiscales))
        msg <- c(msg, "missing 'multiscales'")
    # MUST contain
    for (. in c("axes", "datasets")) 
        if (is.null(ms[[.]]))
            msg <- c(msg, sprintf("missing 'multiscales$%s'", .))
    return(msg)
}
.validateZattrs_axes <- \(x, msg) {
    if (!is.list(ax <- x$axes))
        msg <- c(msg, "missing or non-list 'axes'")
    ax <- ax[[1]]
    if (is.null(ax$name)) 
        msg <- c(msg, "missing 'axes$name'")
    if (!is.null(ts <- ax$type)) 
        if (!all(ts %in% c("space", "time", "channel")))
            msg <- c(msg, "'axes$type' should be 'space/time/channel'")
    return(msg)
}
.validateZattrs_coordTrans <- \(x, msg) {
    if (!is.list(ct <- x$coordinateTransformations))
        msg <- c(msg, "missing or non-list 'coordTrans'")
    ct <- ct[[1]]
    for (. in c("input", "output", "type")) 
        if (is.null(ct[[.]]))
            msg <- c(msg, sprintf("'coordTrans' missing '%s'", .))
    return(msg)
}
.validateZattrsLabelArray <- \(x) {
    msg <- c()
    za <- meta(x)
    msg <- .validateZattrs_multiscales(za, msg)
    ms <- za$multiscales
    msg <- c(msg, .validateZattrs_axes(ms, msg))
    msg <- c(msg, .validateZattrs_coordTrans(ms, msg))
    if (length(msg)) return(msg) else return(TRUE)
}
