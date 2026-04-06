#' @name query
#' @title spatial queries
#'
#' @param x \code{SpatialData} element.
#' @param j scalar character or integer; index or name of coordinate space.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return same as input
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' sd <- readSpatialData(zs, tables=FALSE)
#' 
#' image(sd, "box") <- query(image(sd), xmin=0, xmax=30, ymin=30, ymax=50)
#' 
#' image(sd)
#' image(sd, "box")
NULL

# TODO: query with polygonal boundary region

.check_bb <- \(args) {
    m <- match(names(args), c("xmin", "xmax", "ymin", "ymax"))
    if (any(is.na(m)) || !identical(sort(m), seq_len(4)))
        stop("currently only supporting bounding box query;", 
            " please provide 'xmin/xmax/ymin/ymax' as ...")
    stopifnot(length(args) == 4, is.numeric(unlist(args)))
}

#' @rdname query
#' @export
setMethod("query", "SpatialData", \(x, j=NULL, ...) {
    # check validity of dots
    args <- list(...)
    .check_bb(args)
    # guess coordinate space
    stopifnot(length(j) == 1)
    j <- if (is.null(j)) {
        .guess_space(x)
    } else {
        if (is.character(j)) {
            match.arg(j, CTname(x))
        } else if (is.numeric(j)) {
            stopifnot(j > 0, j == round(j))
            CTname(x)[j]
        }
    }
    # execute query
    for (l in rownames(x))
        for (e in colnames(x)[[l]])
            x[[l]][[e]] <- query(x[[l]][[e]], j, ...)
    return(x)  
})

#' @rdname query
#' @export
setMethod("query", "ImageArray", \(x, ...) {
    args <- list(...)
    .check_bb(args)
    d <- dim(x)
    args$ymax <- min(args$ymax, d[2])
    args$xmax <- min(args$xmax, d[3])
    j <- seq(args$ymin, args$ymax)
    k <- seq(args$xmin, args$xmax)
    return(x[, j, k])
})

#' @rdname query
#' @export
setMethod("query", "LabelArray", \(x, ...) {
    args <- list(...)
    .check_bb(args)
    d <- dim(x)
    args$ymax <- min(args$ymax, d[1])
    args$xmax <- min(args$xmax, d[2])
    i <- seq(args$ymin, args$ymax)
    j <- seq(args$xmin, args$xmax)
    return(x[i, j])
})

#' @rdname query
#' @importFrom sf st_as_sf st_bbox st_crop
#' @export
setMethod("query", "ShapeFrame", \(x, ...) {
    # TODO: this will drop geometries where any coordinate 
    # is out of bounds; keep but crop to boundary region?
    args <- list(...)
    .check_bb(args)
    sf <- st_as_sf(data(x))
    bb <- st_bbox(unlist(args))
    # note: non-spatial attributes (e.g., radius) gives warnings?
    suppressWarnings(sf <- st_crop(sf, bb))
    x@data <- sf[names(x)]
    return(x)  
})

#' @rdname query
#' @export
setMethod("query", "PointFrame", \(x, j, ...) {
    args <- list(...)
    .check_bb(args)
    y <- filter(x, 
        x >= args$xmin, x <= args$xmax, 
        y >= args$ymin, y <= args$ymax)    
    x@data <- y@data
    return(x)
})
