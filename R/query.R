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

.check_box <- \(bb) {
    xy <- c("xmin", "xmax", "ymin", "ymax")
    ok <- c(
        length(bb) == 4, setequal(names(bb), xy),
        is.numeric(bb <- unlist(bb)), !is.na(bb))
    if (!all(ok)) stop(
        "Invalid bounding box query; should be length-4 ",
        "numeric vector with names 'xmin/xmax/ymin/ymax'")
}

.check_pol <- \(mx) {
    ok <- c(
        is.matrix(mx), is.numeric(mx), 
        nrow(mx) >= 3, ncol(mx) == 2)
    if (!all(ok)) stop(
        "Invalid polygon query; should be numeric matrix ",
        "with ≥ 3 rows and 2 columns (= xy-coordinates)")
    # ensure polygon is closed
    top <- mx[1, ]
    bot <- mx[nrow(mx), ]
    if (!all(top == bot)) 
        mx <- rbind(mx, top)
    return(mx)
}

#' @rdname query
#' @export
setMethod("query", "SpatialData", \(x, j=NULL, ...) {
    # check validity of dots
    args <- list(...)
    .check_box(args)
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
    .check_box(args)
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
    .check_box(args)
    d <- dim(x)
    args$ymax <- min(args$ymax, d[1])
    args$xmax <- min(args$xmax, d[2])
    i <- seq(args$ymin, args$ymax)
    j <- seq(args$xmin, args$xmax)
    return(x[i, j])
})

#' @rdname query
#' @importFrom sf st_as_sf st_intersects st_polygon st_bbox st_crop
#' @export
setMethod("query", "ShapeFrame", \(x, ...) {
    # TODO: this will drop geometries where any coordinate 
    # is out of bounds; keep but crop to boundary region?
    args <- list(...)
    if (length(args) == 1) {
        # TODO: currently ignoring 'radius' for circles (i.e.,
        # query based on centroids only); what does Python do?
        mx <- .check_pol(mx <- args[[1]])
        sf <- st_as_sf(data(x))
        ok <- st_intersects(sf, st_polygon(list(mx)), sparse=FALSE)
        x@data <- x@data[which(ok), ]
        return(x)
    }
    # note: non-spatial attributes (e.g., radius) give warnings?
    .check_box(args)
    sf <- st_as_sf(data(x))
    bb <- st_bbox(unlist(args))
    suppressWarnings(sf <- st_crop(sf, bb))
    x@data <- sf[names(x)]
    return(x)  
})

#' @rdname query
#' @importFrom sf st_as_sf st_polygon st_intersects
#' @importFrom dplyr collect filter
#' @export
setMethod("query", "PointFrame", \(x, ...) {
    args <- list(...)
    if (length(args) == 1) {
        mx <- .check_pol(mx <- args[[1]])
        xy <- st_as_sf(collect(data(x)[c("x", "y")]), coords=c("x", "y"))
        ok <- st_intersects(xy, st_polygon(list(mx)), sparse=FALSE)
        return(x[which(ok[, 1])])
    } else {
        .check_box(args)
        y <- filter(x, 
            x >= args$xmin, x <= args$xmax, 
            y >= args$ymin, y <= args$ymax)    
        x@data <- y@data
        return(x)
    }
})
