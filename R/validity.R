# TODO: everything...

.validateTable <- function(object) {
    msg <- c()
    for (i in seq_along(tables(object))) {
        t <- table(object, i)
        if (!is(t, "SingleCellExperiment")) {
            msg <- c(msg, paste0("Table ", i, " is not a 'SingleCellExperiment'"))
        }
        # TODO: validate int_metadata$spatialdata_attrs
        # md <- int_metadata(sce)[["spatialdata_attrs"]]
        # if (!all(c("region_key", "instance_key") %in% names(md))) {
        #     msg <- c(msg, paste0("region_key/instance_key not present in ",
        #                          i, "-th sce int_metadata"))
        # }
    }
    return(msg)
}

.validateImageArray <- \(object) {
    msg <- c()
    res <- length(object)
    for (k in seq_len(res)) {
        x <- data(object, k)
        if (length(dim(x)) != 3) msg <- c(msg, paste(
            "'ImageArray' resolution", k, "is not 3D"))
        if (!type(x) %in% c("double", "integer")) msg <- c(msg, paste(
            "'ImageArray' resolution", k, "is not of type double or integer"))
    }
    if (length(msg)) return(msg) else return(TRUE)
}
#' @importFrom S4Vectors setValidity2
setValidity2("ImageArray", .validateImageArray)

#' @importFrom ZarrArray type
.validateLabelArray <- \(object) {
    msg <- c()
    res <- length(object)
    for (k in seq_len(res)) {
        x <- data(object, k)
        if (length(dim(x)) != 2) msg <- c(msg, paste(
            "'LabelArray' resolution", k, "is not 2D"))
        if (type(x) != "integer") msg <- c(msg, paste(
            "'LabelArray' resolution", k, "is not of type integer"))
    }
    if (length(msg)) return(msg) else return(TRUE)
}
#' @importFrom S4Vectors setValidity2
setValidity2("LabelArray", .validateLabelArray)

.validatePointFrame <- \(object) {
    msg <- c()
    if (!length(object)) return(msg) 
    if (!"x" %in% names(object)) msg <- c(msg, "'PointFrame' missing 'x'.")
    if (!"y" %in% names(object)) msg <- c(msg, "'PointFrame' missing 'y'.")
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("PointFrame", .validatePointFrame)

.validateShapeFrame <- \(object) {
    msg <- c()
    if (!nrow(object)) return(msg) 
    if (!"geometry" %in% names(object)) msg <- c(msg, "'ShapeFrame' missing 'geometry'.")
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("ShapeFrame", .validateShapeFrame)

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
    msg <- c(msg, .validatePointFrame(x))
    msg <- c(msg, .validateTable(x))
    for (y in labels(x)) {
        ok <- .validateLabelArray(y)
        if (!isTRUE(ok)) msg <- c(msg, ok)
        ok <- .validateZattrsLabelArray(y)
        if (!isTRUE(ok)) msg <- c(msg, ok)
    }
    if (length(msg)) return(msg) else return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)

.validateZattrs_multiscales <- \(x, msg) {
    if (is.null(ms <- x$multiscales[[1]]))
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
    for (i in seq_along(ct))
        for (j in c("input", "output", "type"))
            if (is.null(ct[[i]][[j]]))
                msg <- c(msg, sprintf("'coordTrans' %s missing '%s'", i, j))
    return(msg)
}
.validateZattrsLabelArray <- \(x) {
    msg <- c()
    za <- meta(x)
    msg <- .validateZattrs_multiscales(za, msg)
    ms <- za$multiscales[[1]]
    msg <- .validateZattrs_axes(ms, msg)
    msg <- .validateZattrs_coordTrans(ms, msg)
    if (length(msg)) return(msg) else return(TRUE)
}
