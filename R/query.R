#' @name query
#' @title spatial queries
#'
#' @param x \code{SpatialData} element.
#' @param j scalar character or integer; index or name of coordinate space.
#' @param ... option arguments passed to and from other methods.
#'
#' @return same as input
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' image(x, "box") <- query(image(x), xmin=0, xmax=30, ymin=30, ymax=50)
#' 
#' image(x)
#' image(x, "box")
NULL

.check_bb <- \(args) {
    m <- match(names(args), c("xmin", "xmax", "ymin", "ymax"))
    if (any(is.na(m)) || !identical(sort(m), seq_len(4)))
        stop("currently only supporting bounding box query;", 
            " please provide 'xmin/xmax/ymin/ymax' as ...")
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
setMethod("query", "ImageArray", \(x, j, ...) {
    qu <- list(...)
    .check_bb(qu)
    if (missing(j)) j <- 1
    if (is.numeric(j)) j <- CTname(x)[j]
    stopifnot(length(j) == 1)
    . <- grep(j, CTname(x))
    if (!length(.) || is.na(.)) stop("invalid 'j'")
    # transform query into target space
    ts <- CTpath(x, j)
    xy <- list(c(qu$xmin, qu$xmax), c(qu$ymin, qu$ymax))
    xy <- data.frame(xy); names(xy) <- c("x", "y")
    xy <- .trans_xy(xy, ts, TRUE)
    x <- x[, # crop (i.e., subset) array dimensions 2-3
        do.call(seq, as.list(xy[[2]])),
        do.call(seq, as.list(xy[[1]]))]
    # transform array dimensions into target space
    os <- data.frame(x=c(0, dim(x)[3]), y=c(0, dim(x)[2]))
    os <- vapply(.trans_xy(os, ts), min, numeric(1))
    # add transformation of type translation as to
    # offset origin by difference between new & old
    os <- unlist(qu[c("xmin", "ymin")]) - os
    if (!all(os == 0)) x <- addCT(x, 
        name=j, type="translation", 
        data=c(0, os[2], os[1]))
    return(x)
})

#' @rdname query
#' @export
setMethod("query", "LabelArray", \(x, ...) {
    args <- list(...)
    .check_bb(args)
    d <- dim(x)
    if (args$ymax > d[1]) args$ymax <- d[1]
    if (args$xmax > d[2]) args$xmax <- d[2]
    a <- data(x)[
        seq(args$ymin, args$ymax),
        seq(args$xmin, args$xmax)] 
    x@data <- a
    return(x)
})

#' @rdname query
#' @export
setMethod("query", "ShapeFrame", \(x, ...) {
    args <- list(...)
    .check_bb(args)
    df <- st_as_sf(data(x))
    xy <- st_coordinates(df)
    i <- 
        xy[, 1] >= args$xmin & 
        xy[, 1] <= args$xmax & 
        xy[, 2] >= args$ymin & 
        xy[, 2] <= args$ymax 
    x@data <- data(x)[which(i), ]
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
