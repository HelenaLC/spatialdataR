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
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' (sd <- readSpatialData(zs)
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
    cat(sprintf("  - %s (%s)\n", i, d), sep="")
    # labels
    d <- lapply(labels(object), dim)
    d <- lapply(d, paste, collapse=",")
    cat(sprintf("- labels(%s):\n", length(l)))
    cat(sprintf("  - %s (%s)\n", l, d), sep="")
    # points
    d <- lengths(points(object))
    cat(sprintf("- points(%s):\n", length(p)))
    cat(sprintf("  - %s (%s)\n", p, d), sep="")
    # shapes
    nc <- vapply(shapes(object), ncol, numeric(1))
    geom <- ifelse(nc == 1, "polygon", "circle")
    d <- vapply(shapes(object), nrow, numeric(1))
    d <- paste(d, unname(geom), sep=",")
    cat(sprintf("- shapes(%s):\n", length(s)))
    cat(sprintf("  - %s (%s)\n", s, d), sep="")
    # tables
    d <- lapply(tables(object), dim)
    d <- lapply(d, paste, collapse=",")
    cat(sprintf("- tables(%s):\n", length(t)))
    for (. in seq_along(t)) {
        r <- paste(region(SpatialData::table(object, t[.])), collapse=",")
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
    scales <- vapply(object@data, \(x) paste0(dim(x), collapse=","), character(1))
    coolcat("Scales (%d): (%s)", scales)
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

#' @importFrom S4Vectors coolcat
.showZattrs <- function(object) {
    cat("class: Zattrs\n")
    # axes
    ax <- axes(object)
    cat(sprintf("axes(%d):\n", length(ax)))
    if (is.character(ax[[1]])) {
        cat("- name:", unlist(ax), "\n")
    } else {
        cat("- name:", vapply(ax, \(.) .$name, character(1)), "\n")
        cat("- type:", vapply(ax, \(.) .$type, character(1)), "\n")
    }
    # coordinate transformations
    CTshow <- \(l) {
        f <- \(.) {
            . <- paste(unlist(.), collapse=",")
            ifelse(grepl(",", .), sprintf("[%s]", .), .)
        }
        g <- \(.) {
            na <- is.null(.) || !length(unlist(.))
            ifelse(na, "", paste0(":", f(lapply(., f))))
        }
        h <- \(.) sprintf("(%s%s)", .$type, g(.[[.$type]]))
        if (l$type == "sequence") {
            l$transformations |>
                vapply(\(.) h(.), character(1)) |>
                paste(collapse=", ")
        } else {
            h(l)
        }
    }
    ct <- CTlist(object)
    cat(sprintf("coordTrans(%d):\n", length(ct)))
    for (l in ct) {
        cat(sprintf("- %s: %s\n", l$output$name, CTshow(l)))
    }
    # datasets (multiscales)
    if (!is.null(ms <- multiscales(object)[[1]])) {
        ps <- vapply(ms$datasets, \(.) .$path, character(1))
        coolcat("datasets(%d): %s\n", ps)
        for (d in ms$datasets) {
            l <- d$coordinateTransformations[[1]]
            cat(sprintf("- %s: %s\n", d$path, CTshow(l)))
        }
    }
    # channels
    if (!is.null(cs <- unlist(channels(object))))
        coolcat("channels(%d): %s\n", cs)
}

setMethod("show", "Zattrs", .showZattrs)
