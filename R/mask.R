#' @name mask
#' @title Masking
#'
#' @description ...
#' 
#' @param x,y \code{\link{SpatialData}} element
#'
#' @return \code{\link{SingleCellExperiment}}
#'
#' @examples
#' library(SingleCellExperiment)
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' (x <- readSpatialData(x, tables=FALSE))
#'
#' # count points in circles
#' (t <- mask(point(x), shape(x)))
#' assay(t)
#'
#' @export
NULL

# TODO: table from point + shape, image + label etc. etc. etc.

#' @rdname mask
#' @importFrom methods as
#' @importFrom Matrix sparseVector t
#' @importFrom sf st_as_sf st_geometry_type st_sfc st_point st_distance
#' @export
setMethod("mask", c("PointFrame", "ShapeFrame"), \(x, y) {
    #x <- point(x0); y <- shape(x0)
    n <- nrow(y <- st_as_sf(data(y)))
    fk <- meta(x)$spatialdata_attrs$feature_key
    switch(paste(st_geometry_type(y)[1]),
        POINT={
            # realize one feature at a time
            is <- split(seq_len(length(x)), x[[fk]])
            a <- lapply(is, \(.) {
                # make points 'sf'-compliant
                xy <- as.data.frame(x[., c("x", "y")])
                ps <- st_sfc(lapply(asplit(xy, 1), st_point))
                # for each circle, count points within radius
                z <- rowSums(st_distance(y, ps) < y$radius)
                # sparsify counts
                sv <- sparseVector(z[i <- z > 0], which(i), n)
                sm <- as(sv, "sparseMatrix")
            })
            # collect intro matrix w/ dim. features x circles
            a <- t(as(do.call(cbind, a), "dgCMatrix"))
            rownames(a) <- names(is)
        })
    SingleCellExperiment(list(counts=a))
})
