#' @name query
#' @title spatial queries
#'
#' @description Spatial queries serve to subset \code{SpatialData} elements 
#' according to a rectangular bounding box or arbitrary polygonal shapes. 
#' Queries rely on lesser-/greater-equal and \code{sf::st_intersects} for 
#' spatial operations (i.e., instances that intersect the query region 
#' in any way are kept). For circle shapes, radii are currently ignored
#' (i.e., a circle is kept if its centroid intersects the query region).
#'
#' @param x \code{SpatialData} element.
#' @param y query specification; 
#' bounding box: length-4 numeric list with names 'xmin/xmax/ymin/ymax' 
#' (order is irrelevant); polygon: numeric matrix with ≥ 3 rows and 2 columns.
#' @param i for \code{SpatialData}, index or name of table to query.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return same as input
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' sd <- readSpatialData(zs, tables=FALSE)
#' 
#' # helper for visualizing point coordinates
#' .xy <- \(.) data.frame(data(.)[c("x", "y")])
#' 
#' # bounding box
#' y <- list(xmin=11, xmax=44, ymin=22, ymax=55)
#' q <- query(p <- point(sd), y)
#' 
#' plot(.xy(p), asp=1)
#' points(.xy(q), col="red")
#' rect(y$xmin, y$ymin, y$xmax, y$ymax, border="blue")
#' 
#' # polygon
#' y <- rbind(c(20,10), c(50,30), c(20,50), c(30,30))
#' q <- query(p <- point(sd), y)
#' 
#' plot(.xy(p), asp=1)
#' points(.xy(q), col="red")
#' lines(rbind(y, y[1, ]), col="blue")
#' 
#' # shapes that intersect the query region are kept
#' y <- rbind(c(30,45), c(40,45), c(35,50))
#' t <- query(s <- shape(sd, 3), y)
#' 
#' require(sf, quietly=TRUE)
#' df <- st_coordinates(st_as_sf(data(s)))
#' fd <- st_coordinates(st_as_sf(data(t)))
#' plot(
#'   asp=1, xlim=c(15, 60), ylim=c(15, 60),
#'   rbind(y, y[1, ]), type="l", col="blue") 
#' foo <- by(df, df[, "L2"], \(x) points(x, type="b", col="black"))
#' foo <- by(fd, fd[, "L2"], \(x) points(x, type="b", col="red"))
NULL

.check_box <- \(bb) {
    xy <- c("xmin", "xmax", "ymin", "ymax")
    ok <- c(is.list(bb), 
        length(bb) == 4, setequal(names(bb), xy),
        bb$xmin <= bb$xmax, bb$ymin <= bb$ymax, 
        is.numeric(bb <- unlist(bb)), !is.na(bb))
    if (!all(ok)) stop(
        "Invalid bounding box query; should be length-4 ",
        "numeric list with names 'xmin/xmax/ymin/ymax'")
}

.check_pol <- \(mx) {
    ok <- c(
        is.matrix(mx), is.numeric(mx), 
        nrow(mx) >= 3, ncol(mx) == 2,
        !is.na(mx), is.finite(mx))
    if (!all(ok)) stop(
        "Invalid polygon query; should be numeric matrix ",
        "with ≥ 3 rows and 2 columns (= xy-coordinates)")
    # ensure polygon is closed
    top <- mx[1, ]
    bot <- mx[nrow(mx), ]
    if (!all(top == bot)) 
        mx <- rbind(mx, top)
    dup <- duplicated(as.data.frame(mx[-1, , drop=FALSE]))
    if (any(dup)) stop("Invalid polygon query; found duplicated vertices")
    return(mx)
}

#' @rdname query
#' @importFrom dplyr filter
#' @export
setMethod("query", "SpatialData", \(x, ..., i) {
    # TODO: need more example data to properly implement this; 
    # for now, just a proof of concept using 'spatialdata_attrs'
    if (missing(i)) i <- 1
    if (!length(tables(x))) 
        stop("There aren't any tables")
    if (is.numeric(i)) {
        i <- tableNames(x)[i]
    } else if (is.character(i)) {
        i <- match.arg(i, tableNames(x))
    }
    t <- x$tables[[i]]
    ns <- vapply(nm <- colnames(x), length, integer(1))
    nm <- data.frame(layer=rep.int(names(nm), ns), region=unlist(nm))
    nm <- filter(nm, ...)
    i <- match(nm$layer, .LAYERS)
    j <- split(nm$region, nm$layer)
    x <- x[i, j]
    x$tables$table <- t
    return(x)
})

#' @rdname query
#' @export
setMethod("query", "ImageArray", \(x, y) {
    if (is.matrix(y)) stop("Polygon query not supported for images")
    .check_box(y)
    d <- dim(x)
    y$ymax <- min(y$ymax, d[2])
    y$xmax <- min(y$xmax, d[3])
    j <- seq(y$ymin, y$ymax)
    k <- seq(y$xmin, y$xmax)
    return(x[, j, k])
})

#' @rdname query
#' @export
setMethod("query", "LabelArray", \(x, y) {
    if (is.matrix(y)) stop("Polygon query not supported for labels")
    .check_box(y)
    d <- dim(x)
    y$ymax <- min(y$ymax, d[1])
    y$xmax <- min(y$xmax, d[2])
    i <- seq(y$ymin, y$ymax)
    j <- seq(y$xmin, y$xmax)
    return(x[i, j])
})

#' @rdname query
#' @importFrom sf st_as_sf st_intersects st_polygon st_bbox st_crop
#' @export
setMethod("query", "ShapeFrame", \(x, y) {
    # TODO: this will drop geometries where any coordinate 
    # is out of bounds; keep but crop to boundary region?
    if (is.matrix(y)) {
        # TODO: currently ignoring 'radius' for circles (i.e.,
        # query based on centroids only); what does Python do?
        mx <- .check_pol(y)
        sf <- st_as_sf(data(x))
        ok <- st_intersects(sf, st_polygon(list(mx)), sparse=FALSE)
        x@data <- x@data[which(ok), ]
        return(x)
    }
    # note: non-spatial attributes (e.g., radius) give warnings?
    .check_box(y)
    sf <- st_as_sf(data(x))
    bb <- st_bbox(unlist(y))
    suppressWarnings(sf <- st_crop(sf, bb))
    x@data <- sf[names(x)]
    return(x)  
})

#' @rdname query
#' @importFrom sf st_as_sf st_polygon st_intersects
#' @importFrom dplyr collect filter
#' @export
setMethod("query", "PointFrame", \(x, y) {
    if (is.matrix(y)) {
        mx <- .check_pol(y)
        xy <- st_as_sf(collect(data(x)[c("x", "y")]), coords=c("x", "y"))
        ok <- st_intersects(xy, st_polygon(list(mx)), sparse=FALSE)
        return(x[which(ok[, 1])])
    } else {
        .check_box(bb <- y)
        filter(x, 
            x >= bb$xmin, x <= bb$xmax, 
            y >= bb$ymin, y <= bb$ymax)
    }
})
