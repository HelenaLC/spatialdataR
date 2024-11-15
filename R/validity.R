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

.validatePointFrame <- function(object) {
    msg <- NULL
    # Checks if the points have the x,y coordinates, as they are hard-coded
    # in the plot functions
    if (length(points(object))) { # there are some cases where the points are empty
        if (!is.null(data(point(object)))) {
            np <- length(points(object))
            for (i in seq_len(np)) {
                dfi <- data(point(object, i))
                if (!all(c("x", "y") %in% names(dfi))) {
                    msg <- c(msg, paste0("'x' and 'y' missing in data point ", i))
                }
            }
        }
    }
    return(msg)
}

.validateImageArray <- function(object) {
    msg <- c()
    if (ni <- length(images(object))) {
        for (i in seq_along(ni)) {
            ai <- as.array(aperm(data(image(x,1))/255, perm=c(3,2,1)))
            for (j in dim(ai)[3]) {
                if (!all(vapply(ai[,,j], is.numeric, logical(1)))) {
                    msg <- c(msg, paste0("Image ", i, " channel ", j, " not numeric"))
                }
            }
        }
    }
    return(msg)
}

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
    for (y in labels(x)) .validateZattrsLabelArray(y)
    if (length(msg))
        return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)

.validateZattrs_multiscales <- \(x, msg) {
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
    msg <- .validateZattrs_axes(ms, msg)
    msg <- .validateZattrs_coordTrans(ms, msg)
    if (length(msg)) return(msg) else return(TRUE)
}
