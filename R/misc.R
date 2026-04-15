#' @name misc
#' @title Miscellaneous `SpatialData` methods
#' @aliases show,SpatialData-method
#' 
#' @description 
#' Miscellaneous methods (e.g., \code{show}) for the
#' \code{\link{SpatialData}} class and its elements.
#' 
#' @param object 
#'   \code{\link{SpatialData}} object or one of its elements,
#'   i.e., an \code{Image/LabelArray} or \code{Point/ShapeFrame}.
#'
#' @return \code{NULL}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' (sd <- readSpatialData(zs, anndataR=TRUE))
#' 
#' # show element
#' image(sd)
#' label(sd)
#' point(sd)
#' shape(sd)
#' 
#' # show .zattrs
#' meta(label(sd))
#' meta(image(sd, 2))
NULL

#' @importFrom RBGL sp.between
#' @importFrom S4Vectors coolcat
#' @importFrom graph nodeData nodes
.showSpatialData <- function(object) {
    cat("class: SpatialData\n")
    i <- imageNames(object)
    l <- labelNames(object)
    p <- pointNames(object)
    s <- shapeNames(object)
    t <- tableNames(object)
    # images
    d <- lapply(images(object), dim)
    d <- lapply(d, paste, collapse=",")
    cat(sprintf("- images(%s):\n", length(i)))
    for (. in seq_along(i)) 
        cat(sprintf("  - %s (%s)\n", i[.], d[.]))
    # labels
    d <- lapply(labels(object), dim)
    d <- lapply(d, paste, collapse=",")
    cat(sprintf("- labels(%s):\n", length(l)))
    for (. in seq_along(l)) 
        cat(sprintf("  - %s (%s)\n", l[.], d[.]))
    # points
    d <- lapply(points(object), length)
    cat(sprintf("- points(%s):\n", length(p)))
    for (. in seq_along(p)) 
        cat(sprintf("  - %s (%s)\n", p[.], d[.]))
    # shapes
    nc <- vapply(shapes(object), ncol, numeric(1))
    geom <- ifelse(nc == 1, "polygon", "circle")
    d <- vapply(shapes(object), nrow, numeric(1))
    d <- paste(d, unname(geom), sep=",")
    cat(sprintf("- shapes(%s):\n", length(s)))
    for (. in seq_along(s)) 
        cat(sprintf("  - %s (%s)\n", s[.], d[.]))
    # tables
    d <- lapply(tables(object), dim)
    d <- lapply(d, paste, collapse=",")
    cat(sprintf("- tables(%s):\n", length(t)))
    for (. in seq_along(t)) {
        r <- paste(region(table(object, t[.])), collapse=",")
        cat(sprintf("  - %s (%s) [%s]\n", t[.], d[.], r))
    }
    # spaces
    e <- c(i, l, s, p)
    g <- CTgraph(object)
    t <- nodeData(g, nodes(g), "type")
    n <- sum(i <- (t == "space"))
    cat(sprintf("coordinate systems(%s):\n", n))
    for (c in nodes(g)[i]) {
        pa <- suppressWarnings(sp.between(g, paste0("_", e), c))
        ss <- strsplit(gsub("^_", "", names(pa)), ":")
        ss <- ss[vapply(pa, \(.) !is.na(.$length), logical(1))]
        coolcat(
            paste0("- ", c, "(%d): %s"),
            vapply(ss, \(.) .[1], character(1)))
    }
}

#' @rdname misc
setMethod("show", "SpatialData", .showSpatialData)

#' @importFrom S4Vectors coolcat
.showsdArray <- function(object) {
    n.object <- length(object@data)
    cat("class: ", class(object), ifelse(n.object > 1, "(MultiScale)", ""),"\n")
    scales <- vapply(object@data, \(x) sprintf("(%s)", paste0(dim(x), collapse=",")), character(1))
    coolcat("Scales (%d): %s", scales)
}

#' @rdname misc
setMethod("show", "sdArray", .showsdArray)

#' @importFrom S4Vectors coolcat
.showPointFrame <- function(object) {
    cat("class: PointFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
}

#' @rdname misc
setMethod("show", "PointFrame", .showPointFrame)

#' @importFrom S4Vectors coolcat
.showShapeFrame <- function(object) {
    cat("class: ShapeFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
}

#' @rdname misc
setMethod("show", "ShapeFrame", .showShapeFrame)