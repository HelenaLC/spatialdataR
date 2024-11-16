#' @name query
#' @title spatial queries
#'
#' @param x \code{SpatialData} element
#'
#' @return ...
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' image(x, "i") <- query(image(x), xmin=0, xmax=60, ymin=0, ymax=40)
#' label(x, "l") <- query(label(x), xmin=0, xmax=60, ymin=0, ymax=40)
#' shape(x, "s") <- query(shape(x), xmin=0, xmax=60, ymin=0, ymax=40)
#' point(x, "p") <- query(point(x), xmin=0, xmax=60, ymin=0, ymax=40)
#' 
#' plotSpatialData() + 
#'   plotImage(x, "i") +
#'   plotLabel(x, "l") +
#'   plotPoint(x, "p") +
#'   plotShape(x, "s")
NULL

setGeneric("query", \(x, ...) standardGeneric("query"))

.check_bb <- \(args) {
    if (!identical(names(args), c("xmin", "xmax", "ymin", "ymax")))
        stop("currently only supporting bounding box query;", 
            " please provide 'xmin/xmax/ymin/ymax' as ...")
}

#' @rdname query
#' @export
setMethod("query", "SpatialData", \(x, ...) {
    args <- list(...)
    .check_bb(args)
    for (l in setdiff(.LAYERS, "tables"))
        for (e in names(x[[l]]))
            x[[l]][[e]] <- query(x[[l]][[e]], ...)
    return(x)  
})

#' @noRd
.get_bounding_box_corners_in_intrinsic_coordinates <- \(x, cs = 1, ...){
  args <- list(...)
  .check_bb(args)
  if (!is.null(t <- getTS(x, cs))){
    for (. in rev(seq(nrow(t)))) {
      typ <- t$type[.]
      dat <- t[[typ]][.][[1]]
      dat <- dat[-1]
      switch(typ,
             translation={
               args$ymin <- args$ymin-dat[1]
               args$xmin <- args$xmin-dat[2]
               args$ymax <- args$ymax-dat[1]
               args$xmax <- args$xmax-dat[2]
             },
             scale={
               args$ymin <- args$ymin/dat[1]
               args$xmin <- args$xmin/dat[2]
               args$ymax <- args$ymax/dat[1]
               args$xmax <- args$xmax/dat[2]
             })
    } 
  }
  args[c("xmin", "ymin")] <- sapply(args[c("xmin", "ymin")], floor)
  args[c("xmax", "ymax")] <- sapply(args[c("xmax", "ymax")], ceiling)
  args
}

.set_transformation <- \(x, cs, t_name, new_t){
  if (!is.null(t <- getTS(x, cs))){
    t <- getTS(x, cs)
    ind <- which(t[["type"]]==t_name)
    t[[t_name]][[ind]] <- new_t
    x <- setTS(x, cs, t)
  }
  x
}

#' @rdname query
#' @export
setMethod("query", "ImageArray", \(x, cs, ...) {
    args <- list(...)
    .check_bb(args)
    old_args <- args
    args <- .get_bounding_box_corners_in_intrinsic_coordinates(x, cs = 1, ...)
    d <- dim(x)[-1]
    if (args$ymax > d[1]) args$ymax <- d[1]
    if (args$xmax > d[2]) args$xmax <- d[2]
    a <- data(x)[,
        seq(args$ymin, args$ymax),
        seq(args$xmin, args$xmax), drop = FALSE]
    x@data <- list(a)
    x <- .set_transformation(x, cs = 1, t_name = "translation", new_t = c(0, old_args$ymin, old_args$xmin))
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
        xy[, 1] > args$xmin & 
        xy[, 1] < args$xmax & 
        xy[, 2] > args$ymin & 
        xy[, 2] < args$ymax 
    x@data <- data(x)[which(i), ]
    return(x)  
})

#' @rdname query
#' @export
setMethod("query", "PointFrame", \(x, ...) {
    args <- list(...)
    .check_bb(args)
    filter(x, 
        x > args$xmin, x < args$xmax, 
        y > args$ymin, y < args$ymax)    
})
