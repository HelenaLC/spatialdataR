# TODO: everything...

.tables_validity <- function(object) {
    msg <- NULL
    nt <- length(tables(object))
    for (i in c(1:nt)) {
        sce <- table(object, i)
        if (!is(sce, "SingleCellExperiment")) {
            msg <- c(msg, paste0("Table ", i, " is not a SingleCellExperiment"))
        }
        md <- SingleCellExperiment::metadata(se)[[1]]
        if (!all(c("region_key", "instance_key") %in% names(md))) {
            msg <- c(msg, paste0("region_key/instance_key not present in ",
                                 i, "-th sce metadata"))
        }
    }
    return(msg)
}

.points_validity <- function(object) {
    msg <- NULL
    # Checks if the points have the x,y coordinates, as they are hard-coded
    # in the plot functions
    if (!is.null(data(point(object)))) {
        np <- length(points(object))
        for (i in c(1:np)) {
            dfi <- data(point(object, i))
            if (!all(c("x", "y") %in% names(dfi))) {
                msg <- c(msg, paste0("'x' and 'y' missing in data point ", i))
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
    msg <- c(msg, .points_validity(x))
    msg <- c(msg, .tables_validity(x))
    if (length(msg))
        return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)
