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

.showSpatialData <- function(object) {
    imgs <- images(object)
    labs <- labels(object)
    shps <- shapes(object)
    pnts <- points(object)
    cat("class: SpatialData\n")
    # available elements
    cat(sprintf("images(%s):", length(imgs)), names(imgs), "\n")
    cat(sprintf("labels(%s):", length(labs)), names(labs), "\n")
    cat(sprintf("shapes(%s):", length(shps)), names(shps), "\n")
    cat(sprintf("points(%s):", length(pnts)), names(pnts), "\n")
    cat("table:", if (!is.null(table(object)))
        dim(table(object)) else "nan", "\n")
    # shared coordinate systems
    # TODO: util for this?
    lys <- list(imgs, labs, shps)
    cs <- Reduce(intersect, lapply(lys, \(.)
        vapply(., \(.) getCoordTrans(.)$output$name, character(1))))
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
    cs <- coords(object)$output.name
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
