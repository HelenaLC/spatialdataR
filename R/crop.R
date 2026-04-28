#' @name crop
#' @title spatial cropping
#'
#' @description \code{crop} subsets \code{SpatialData} elements according 
#' to a rectangular bounding box or arbitrary polygonal shapes.
#' 
#' For \code{SpatialData} objects, \code{crop} propagates the operation 
#' across all layers that share the coordinate space \code{j}.
#' 
#' For \code{sdFrame}s (points and shapes), cropping relies on 
#' \code{sf::st_intersects} (i.e., instances that intersect the 
#' query region in any way are kept). For circle shapes, radii 
#' are currently ignored (i.e., a circle is kept if its centroid 
#' intersects the query region).
#' 
#' For \code{sdArray}s (images and labels), only bounding box 
#' cropping is supported. The requested spatial bounding box is 
#' projected into pixel coordinates, and the underlying array is 
#' sliced accordingly. The \code{wh} metadata is updated to 
#' reflect the new spatial extent.
#'
#' @param x \code{SpatialData} object or element.
#' @param y query specification;
#' bounding box: length-4 numeric list with names 'xmin/xmax/ymin/ymax';
#' polygon: numeric matrix with at least 3 rows and exactly 2 columns.
#' @param j character string specifying a coordinate system.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return same as input
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' sd <- readSpatialData(zs, tables=FALSE)
#'
#' # bounding box crop of a SpatialData object
#' y <- list(xmin=10, xmax=50, ymin=10, ymax=50)
#' crop(sd, y, j="global")
#'
#' # cropping individual elements
#' q <- crop(p <- point(sd), y)
NULL

.check_box <- \(bb) {
    xy <- c("xmin", "xmax", "ymin", "ymax")
    ok <- c(is.list(bb),
        length(bb) == 4, setequal(names(bb), xy),
        bb$xmin <= bb$xmax, bb$ymin <= bb$ymax,
        is.numeric(bb <- unlist(bb)), !is.na(bb))
    if (!all(ok)) stop(
        "Invalid bounding box; should be length-4 ",
        "numeric list with names 'xmin/xmax/ymin/ymax'")
}

.check_pol <- \(mx) {
    ok <- c(
        is.matrix(mx), is.numeric(mx),
        nrow(mx) >= 3, ncol(mx) == 2,
        !is.na(mx), is.finite(mx))
    if (!all(ok)) stop(
        "Invalid polygon; should be numeric matrix with at ",
        "least 3 rows and exactly 2 columns (= xy-coordinates)")
    # ensure polygon is closed
    top <- mx[1, ]
    bot <- mx[nrow(mx), ]
    if (!all(top == bot))
        mx <- rbind(mx, top)
    dup <- duplicated(as.data.frame(mx[-1, , drop=FALSE]))
    if (any(dup)) stop("Invalid polygon; found duplicated vertices")
    return(mx)
}

#' @importFrom sf st_as_sf st_coordinates
.box2rev <- \(x, y) {
    # assure x coordinates come first for 
    # correct boundary retrieval at the end
    y <- y[c("xmin", "xmax", "ymin", "ymax")]
    df <- data.frame(
        x=c(y$xmin, y$xmax, y$xmax, y$xmin, y$xmin),
        y=c(y$ymin, y$ymin, y$ymax, y$ymax, y$ymin),
        id=seq_len(length(y)+1))
    names(df) <- c("x", "y", "id")
    # TODO: utils for transformations from 
    # array to frame with optional reverse
    ct <- CTlist(x)
    ts <- ct[[1]]$transformations
    for (. in seq_along(ts))
        ct[[1]]$transformations[[.]][[names(CTdata(x))[.]]] <- rev(CTdata(x)[[.]][-1])
    md <- Zattrs(type="frame", trans=ct)
    z <- ShapeFrame(df, meta=md)
    z <- transform(z, 1, rev=TRUE)
    z <- st_coordinates(st_as_sf(data(z)))
    z <- as.list(c(range(z[, 1]), range(z[, 2])))
    names(z) <- names(y)
    return(z)
}

#' @export
#' @rdname crop
#' @importFrom methods is
#' @importFrom sf st_bbox
setMethod("crop", "sdArray", \(x, y, j=1, ...) {
    if (is.matrix(y)) stop("Polygon cropping not supported for 'sdArray'")
    if (is(y, "sf")) y <- as.list(st_bbox(y))
    # coordinate space alignment
    .check_box(y)
    z <- .box2rev(x, y)
    # offset current origin
    wh <- metadata(x)$wh
    if (!is.null(wh)) {
        z$xmin <- z$xmin - wh[[1]][1]
        z$xmax <- z$xmax - wh[[1]][1]
        z$ymin <- z$ymin - wh[[2]][1]
        z$ymax <- z$ymax - wh[[2]][1]
    }
    # assure query is within bounds (n=3: cyx; n=2: yx)
    n <- length(d <- dim(x))
    z$xmin <- floor(max(z$xmin, 0))
    z$ymin <- floor(max(z$ymin, 0))
    z$xmax <- ceiling(min(z$xmax, d[n]))
    z$ymax <- ceiling(min(z$ymax, d[n-1]))
    # update origin
    if (is.null(wh)) {
        # set from bounding box
        wh <- list(
            c(z$xmin, z$xmax), 
            c(z$ymin, z$ymax))
    } else {
        # offset current origin
        wh[[1]] <- wh[[1]][1] + c(z$xmin, z$xmax)
        wh[[2]] <- wh[[2]][1] + c(z$ymin, z$ymax)
    }
    metadata(x)$wh <- wh
    # subset array
    i <- seq(z$ymin+1, z$ymax)
    j <- seq(z$xmin+1, z$xmax) 
    if (n == 3) x[, i, j] else x[i, j]
})

#' @export
#' @rdname crop
#' @importFrom dplyr pull
#' @importFrom methods is
#' @importFrom duckspatial ddbs_intersects
#' @importFrom sf st_sf st_sfc st_as_sfc st_bbox st_polygon st_geometry st_geometry<-
setMethod("crop", "sdFrame", \(x, y, j=1, ...) {
    x <- transform(x, j)
    if (is(y, "sf")) {
        polygon <- y
        st_geometry(polygon) <- "geometry"
    } else if (is(y, "sfc")) {
        polygon <- st_sf(geometry=y)
    } else if (is(y, "sfg")) {
        polygon <- st_sf(geometry=st_sfc(y))
    } else if (is.matrix(y)) {
        mx <- .check_pol(y)
        polygon <- st_sf(geometry=st_sfc(st_polygon(list(mx))))
    } else {
        # bounding box
        .check_box(y)
        polygon <- st_sf(geometry=st_as_sfc(st_bbox(unlist(y))))
    }
    ok <- ddbs_intersects(data(x), polygon, sparse=TRUE)
    x <- x[pull(ok, id_x), ]
    return(x)
})

#' @export
#' @rdname crop
setMethod("crop", "SpatialData", \(x, y, j=1, ...) {
    if (is.numeric(j)) j <- CTname(x)[j]
    ls <- setdiff(.LAYERS, "tables")
    for (l in ls) {
        for (e in names(x[[l]])) {
            z <- x[[l]][[e]]
            z <- if (j %in% CTname(z))
                crop(z, y, j=j)
            x[[l]][[e]] <- z
        }
    }
    return(x)
})
