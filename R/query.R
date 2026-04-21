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
#' bounding box: length-4 numeric list with names 'xmin/xmax/ymin/ymax';
#' polygon: numeric matrix with at least 3 rows and exactly 2 columns.
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

#' @rdname query
#' @importFrom dplyr filter pull
#' @importFrom SummarizedExperiment colData
#' @importFrom SingleCellExperiment int_colData
#' @export
setMethod("query", "SpatialData", \(x, ..., i) {
    if (missing(i)) i <- 1
    if (!length(tables(x))) 
        stop("There aren't any tables")
    if (is.numeric(i)) {
        i <- tableNames(x)[i]
    } else if (is.character(i)) {
        i <- match.arg(i, tableNames(x))
    }
    t <- x$tables[[i]]
    df <- data.frame(.i=seq_len(ncol(t)), colData(t), int_colData(t))
    df <- filter(df, ...)
    if (!nrow(df)) stop("Nothing left after query")
    t <- t[, df$.i]
    colData(t) <- droplevels(colData(t))
    int_colData(t) <- droplevels(int_colData(t))
    region(t) <- levels(int_colData(t)[[region_key(t)]])
    for (l in setdiff(.LAYERS, "tables")) {
        j <- !names(x[[l]]) %in% region(t)
        if (sum(j)) x[[l]] <- x[[l]][-which(j)]
    }
    for (r in region(t)) {
        l <- layer(x, r)
        if (l == "labels") next
        e <- x[[l]][[r]]
        ik <- instance_key(t)
        j <- pull(data(e), ik)
        j <- j %in% instances(t)
        x[[l]][[r]] <- e[which(j), ]
    }
    table(x, i) <- t
    return(x)
})

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
        "Invalid polygon query; should be numeric matrix with at ",
        "least 3 rows and exactly 2 columns (= xy-coordinates)")
    # ensure polygon is closed
    top <- mx[1, ]
    bot <- mx[nrow(mx), ]
    if (!all(top == bot)) 
        mx <- rbind(mx, top)
    dup <- duplicated(as.data.frame(mx[-1, , drop=FALSE]))
    if (any(dup)) stop("Invalid polygon query; found duplicated vertices")
    return(mx)
}

.query_sdArray <- \(x, y) {
    if (is.matrix(y)) stop(
        "Polygon query not supported for ",
        "element of type 'image/labelArray'")
    .check_box(y)
    # protect image channels (i.e., 
    # only query spatial dimensions)
    n <- length(d <- dim(x))
    if (n == 3) d <- d[-1]
    # assure query is within bounds
    y$xmin <- max(y$xmin, 0)
    y$ymin <- max(y$ymin, 0)
    y$ymax <- min(y$ymax, d[1])
    y$xmax <- min(y$xmax, d[2])
    # subset spatial dimensions
    i <- seq(y$ymin, y$ymax)
    j <- seq(y$xmin, y$xmax)
    wh <- list(
        y[c("xmin", "xmax")], 
        y[c("ymin", "ymax")])
    wh <- lapply(wh, unlist)
    metadata(x)$wh <- wh
    if (n == 3) {
        return(x[, i, j])
    } else {
        return(x[i, j])
    }
}

#' @rdname query
#' @export
setMethod("query", "ImageArray", \(x, y) .query_sdArray(x, y))

#' @rdname query
#' @export
setMethod("query", "LabelArray", \(x, y) .query_sdArray(x, y))

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
#' @importFrom dplyr filter
#' @importFrom sf st_as_sf st_polygon st_intersects
#' @export
setMethod("query", "PointFrame", \(x, y) {
    if (is.matrix(y)) {
        mx <- .check_pol(y)
        xy <- st_as_sf(as.data.frame(x)[xy <- c("x", "y")], coords=xy)
        ok <- st_intersects(xy, st_polygon(list(mx)), sparse=FALSE)
        return(x[which(ok[, 1]), ])
    } else {
        .check_box(bb <- y)
        filter(x, 
            x >= bb$xmin, x <= bb$xmax, 
            y >= bb$ymin, y <= bb$ymax)
    }
})
