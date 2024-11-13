# TODO: everything...

.tables_validity <- function(object) {
    msg <- NULL
    nt <- length(tables(object))
    for (i in c(1:nt)) {
        sce <- table(object, i)
        if (!is(sce, "SingleCellExperiment")) {
            msg <- c(msg, paste0("Table ", i, " is not a SingleCellExperiment"))
        }
        # md <- int_metadata(sce)[["spatialdata_attrs"]]
        # if (!all(c("region_key", "instance_key") %in% names(md))) {
        #     msg <- c(msg, paste0("region_key/instance_key not present in ",
        #                          i, "-th sce int_metadata"))
        # }
    }
    return(msg)
}

.points_validity <- function(object) {
    msg <- NULL
    # Checks if the points have the x,y coordinates, as they are hard-coded
    # in the plot functions
    if(length(points(object))) { # there are some cases where the points are empty
        if (!is.null(data(point(object)))) {
            np <- length(points(object))
            for (i in c(1:np)) {
                dfi <- data(point(object, i))
                if (!all(c("x", "y") %in% names(dfi))) {
                    msg <- c(msg, paste0("'x' and 'y' missing in data point ", i))
                }
            }
        }
    }
    return(msg)
}

.image_validity <- function(object) {
    msg <- NULL

    if (length(images(object))) {
        ni <- length(ni)
        for (i in c(1:ni)) {
            ai <- as.array(aperm(data(image(x,1))/255, perm=c(3,2,1)))
            for (j in dim(ai)[3]) {
                all_numeric <- all(sapply(ai[,,1], is.numeric))
                if (!all_numeric) {
                    msg <- c(msg, paste0("Image ", i, " channel ", j,
                        " not numeric"))
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
    msg <- c(msg, .points_validity(x))
    msg <- c(msg, .tables_validity(x))
    if (length(msg))
        return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)
