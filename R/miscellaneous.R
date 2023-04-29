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
#' (ia <- readImage(file.path(path, "images", "raccoon")))
#' (sd <- readSpatialData(path))
NULL

.showSpatialData <- function(object) {
    imgs <- images(object)
    labs <- labels(object)
    shps <- shapes(object)
    pnts <- points(object)
    cat("class: SpatialData\n")
    cat(sprintf("images(%s):", length(imgs)), names(imgs), "\n")
    cat(sprintf("labels(%s):", length(labs)), names(labs), "\n")
    cat(sprintf("shapes(%s):", length(shps)), names(shps), "\n")
    cat(sprintf("points(%s):", length(pnts)), names(pnts), "\n")
    cat("table:", if (!is.null(table(object)))
        dim(table(object)) else "nan")
}

#' @rdname SD-miscellaneous
setMethod("show", "SpatialData", .showSpatialData)

.showImageArray <- function(object) {
    axs <- metadata(object)$multiscales$axes[[1]]
    cat("class: ImageArray\n")
    d <- dim(object)
    if (length(d) == 1) d <- 0
    cat(sprintf("axiis(%s):", paste(axs$name, collapse = "")), d, "\n")

    t <- axs$type == "time"
    s <- axs$type == "space"
    c <- axs$type == "channel"
    cat(sprintf("|-time(%s):", sum(t)), axs$name[t], "\n")
    cat(sprintf("|-space(%s):", sum(s)), axs$name[s], "\n")
    cat(sprintf("|-channel(%s):", sum(c)), axs$name[c], "\n")

    chs <- metadata(object)$channels_metadata$channels$label
    cat("channels:", chs, "\n")
}

#' @rdname SD-miscellaneous
setMethod("show", "ImageArray", .showImageArray)
