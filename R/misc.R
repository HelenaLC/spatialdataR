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
#'   i.e., a \code{SpatialDataImage/Label/Point/Shape}.
#'
#' @return \code{NULL}
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="spatialdataR")
#' (sd <- readSpatialData(zs))
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
#' 
#' @importFrom S4Vectors coolcat
NULL

#' @importFrom RBGL sp.between
#' @importFrom graph nodeData nodes
.showSpatialData <- function(object) {
    cat("class: SpatialData\n")
    i <- imageNames(object)
    l <- labelNames(object)
    p <- pointNames(object)
    s <- shapeNames(object)
    t <- tableNames(object)
    # maximum elements shown per layer;
    # override via 'options(SpatialData.show.max=...)'
    n <- getOption("SpatialData.show.max", 5L)
    # print up to 'n' items with descriptions; 
    # append ellipsis when truncated
    .cat <- function(type, nms, descs) {
        total <- length(nms)
        cat(sprintf("%s(%d):\n", type, total))
        shown <- seq_len(min(total, n))
        cat(sprintf("- %s (%s)\n", nms[shown], descs[shown]), sep="")
        if (total > n) cat("    ...\n")
    }
    # images & labels
    if (length(i)) coolcat("images(%d): %s\n", i)
    if (length(l)) coolcat("labels(%d): %s\n", l)
    # points
    if (length(p)) .cat("points", p, as.character(lengths(points(object))))
    # shapes
    if (length(s)) {
        dm <- vapply(shapes(object), dim, numeric(2))
        nr <- dm[1, ]
        nc <- dm[2, ]
        geom <- c("circle", "polygon")[(nc == 1) + 1L]
        .cat("shapes", s, paste(nr, unname(geom), sep=","))
    }
    # tables (special: each entry also shows the region)
    if (length(t)) {
        total <- length(t)
        d <- vapply(tables(object), \(x) paste(dim(x), collapse=","), character(1))
        cat(sprintf("tables(%d):\n", total))
        for (. in seq_len(min(total, n))) {
            r <- paste(region(table(object, t[.])), collapse=",")
            cat(sprintf("- %s (%s) [%s]\n", t[.], d[.], r))
        }
        if (total > n) cat("    ...\n")
    }
    # spaces
    e <- c(i, l, s, p)
    g <- CTgraph(object)
    node_types <- nodeData(g, nodes(g), "type")
    is_space <- node_types == "space"
    cat(sprintf("coords(%d):\n", sum(is_space)))
    for (cs in nodes(g)[is_space]) {
        pa <- suppressWarnings(sp.between(g, paste0("_", e), cs))
        ss <- strsplit(sub("^_", "", names(pa)), ":", fixed=TRUE)
        ss <- ss[vapply(pa, \(.) !is.na(.$length), logical(1))]
        coolcat(
            paste0("- ", cs, "(%d): %s"),
            vapply(ss, \(.) .[1], character(1)))
    }
}

#' @rdname misc
setMethod("show", "SpatialData", .showSpatialData)

.showArrayDims <- \(.) sprintf("(%s)", paste(dim(.), collapse=" "))
.showCoordTrans <- \(.) coolcat("coords(%d): %s", CTname(object))

.showArray <- function(object) {
    nd <- length(ax <- axes(object, "name"))
    cat("class:", class(object), sprintf("(%sd:%s)\n", nd, paste(ax, collapse="")))
    coolcat("scales(%d): %s", vapply(object@data, .showArrayDims, character(1)))
    .showCoordTrans(object)
}

#' @rdname misc
setMethod("show", "SpatialDataArray", .showArray)

.showImage <- function(object) {
    callNextMethod()
    coolcat("channels(%d): %s", channels(object))
}

#' @rdname misc
setMethod("show", "SpatialDataImage", .showImage)

.showFrame <- function(object) {
    cat(sprintf("class: %s (%d)\n", class(object)[1], length(object)))
    coolcat("names(%d): %s\n", names(object))
    .showCoordTrans(object)
}

#' @rdname misc
setMethod("show", "SpatialDataFrame", .showFrame)

.dims <- \(.) vapply(., \(.) paste(vapply(data(., NULL), .showArrayDims, character(1)), collapse=" "), character(1))
.geom <- \(.) c("polygon", "circle")[1L+("radius" %in% names(.))]

.showImages <- function(object) {
    cat(sprintf("class: %s\n", class(object)))
    axes <- vapply(object, \(.) paste(axes(., "name"), collapse=""), character(1))
    cat(sprintf("- %s [axes: %s, dims: %s]\n", names(object), axes, .dims(object)), sep="")
}
.showLabels <- function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("- %s [dims: %s]\n", names(object), .dims(object)), sep="")
}
.showPoints <- function(object) {
    cat(sprintf("class: %s\n", class(object)))
    n <- vapply(object, length, integer(1))
    cat(sprintf("- %s [nobs: %s]\n", names(object), n), sep="")
    
}
.showShapes <- function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("- %s [nobs: %s, geom: %s)\n", names(object), length(object), .geom(object)), sep="")
    
}

#' @rdname misc
setMethod("show", "sdImageList", .showImages)
#' @rdname misc
setMethod("show", "sdLabelList", .showLabels)
#' @rdname misc
setMethod("show", "sdPointList", .showPoints)
#' @rdname misc
setMethod("show", "sdShapeList", .showShapes)

.showAttrs <- function(object) {
    cat("class: SpatialDataAttrs\n")
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
            if (length(.) <= 1) return(.)
            sprintf("[%s]", paste(unlist(.), collapse=","))
        }
        g <- \(.) {
            na <- !length(unlist(.))
            if (na) "" else paste0(":", f(lapply(., f)))
        }
        h <- \(.) sprintf("(%s%s)", .$type, g(.[[.$type]]))
        if (l$type == "sequence") {
            l$transformations |>
                vapply(\(.) h(.), character(1)) |>
                toString()
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

setMethod("show", "SpatialDataAttrs", .showAttrs)
