#' @name SD-miscellaneous
#' @title Miscellaneous `SpatialData` methods
#' @description
#' Miscellaneous methods for the \code{\link{SpatialData}}
#' and \code{\link{ImageArray}} classes that do not fit
#' into any other documentation category such as,
#' for example, show methods.
#'
#' @param object \code{\link{SpatialData}} or \code{\link{ImageArray}} object.
#'
#' @return \code{NULL}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' path <- system.file("extdata", "raccoon", package="SpatialData")
#' (ia <- readArray(file.path(path, "images", "raccoon")))
#' (sd <- readSpatialData(path))
NULL

#' @importFrom S4Vectors coolcat
.showSpatialData <- function(object) {
    imgs <- images(object)
    labs <- labels(object)
    shps <- shapes(object)
    pnts <- points(object)
    cat("class: SpatialData\n")
    cat("table:", if (!is.null(table(object)))
        dim(table(object)) else "nan", "\n")
    # available elements
    coolcat("images(%d): %s\n", imageNames(object))
    coolcat("labels(%d): %s\n", labelNames(object))
    coolcat("shapes(%d): %s\n", shapeNames(object))
    coolcat("points(%d): %s\n", pointNames(object))
    # shared coordinate system(s)
    # TODO: util for this? also, there's probably
    # an easier way, this is super hacky...
    lys <- list(imgs, labs, shps)
    lys <- lys[vapply(lys, length, numeric(1)) > 0]
    cs <- lapply(lys, \(.) lapply(., \(.) getCoordTrans(.)$output$name))
    cs <- Reduce(intersect, lapply(cs, Reduce, f=intersect))
    cat(sprintf("coords(%s):", length(cs)), cs)
}

#' @rdname SD-miscellaneous
setMethod("show", "SpatialData", .showSpatialData)

.showZarrArray <- function(object) {
    d <- dim(object)
    if (length(d) == 1) d <- 0
    axs <- metadata(object)$multiscales$axes[[1]]
    cat(sprintf("axiis(%s):", paste(axs$name, collapse = "")), d, "\n")
    t <- axs$type == "time"
    s <- axs$type == "space"
    c <- axs$type == "channel"
    cat(sprintf("|-time(%s):", sum(t)), axs$name[t], "\n")
    cat(sprintf("|-space(%s):", sum(s)), axs$name[s], "\n")
    cat(sprintf("|-channel(%s):", sum(c)), axs$name[c], "\n")
    cs <- coords(object)$output$name
    cat(sprintf("coords(%s):", length(cs)), cs)
}
.showImageArray <- function(object) {
    cat("class: ImageArray\n")
    cat("channels:", channels(object), "\n")
    callNextMethod(object)
}
.showLabelArray <- function(object) {
    cat("class: LabelArray\n")
    callNextMethod(object)
}

.showShapeFrame <- function(object) {
    cat("class: ShapeFrame\n")
    cat("geoms:", n <- nrow(object), "\n")
    if (n) cat("type:", object$type[1], "\n")
    cs <- coords(object)$output$name
    cat(sprintf("coords(%s):", length(cs)), cs)
}

.showPointFrame <- function(object) {
    cat("class: PointFrame\n")
    cat("length:", length(object), "\n")
    cs <- coords(object)$output$name
    cat(sprintf("coords(%s):", length(cs)), cs)
}

#' @rdname SD-miscellaneous
setMethod("show", "ZarrArray", .showZarrArray)

#' @rdname SD-miscellaneous
setMethod("show", "ImageArray", .showImageArray)

#' @rdname SD-miscellaneous
setMethod("show", "LabelArray", .showLabelArray)

#' @rdname SD-miscellaneous
setMethod("show", "ShapeFrame", .showShapeFrame)

#' @rdname SD-miscellaneous
setMethod("show", "PointFrame", .showPointFrame)
