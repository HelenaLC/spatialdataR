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
#' x <- readSpatialData(x, tables=FALSE)
#'
#' # count points in circles
#' x <- mask(x, "blobs_points", "blobs_circles")
#' x <- mask(x, "blobs_image", "blobs_labels")
#' tables(x)
#'
#' @export
NULL

# TODO: table from point + shape, image + label etc. etc. etc.

#' @rdname mask
#' @importFrom SingleCellExperiment int_colData int_colData<- int_metadata<-
#' @export
setMethod("mask", "SpatialData", \(x, i, j, ...) {
    stopifnot(length(i) == 1, is.character(i), i %in% unlist(colnames(x)))
    stopifnot(length(j) == 1, is.character(j), j %in% unlist(colnames(x)))
    # get element types
    ls <- vapply(list(i, j), \(e) rownames(x)[vapply(colnames(x), \(es) e %in% es, logical(1))], character(1))
    a <- element(x, ls[[1]], i)
    b <- element(x, ls[[2]], j)
    t <- .mask(a, b, ...)
    md <- list(region=j, 
        region_key="region", 
        instance_key="instance")
    int_metadata(t)$spatialdata_attrs <- md
    cd <- data.frame(region=j, instance=colnames(t))
    int_colData(t) <- cbind(int_colData(t), cd)
    nm <- paste0(i, "_masked_by_", j)
    `table<-`(x, nm, value=t)
})

setGeneric(".mask", \(a, b, ...) standardGeneric(".mask"))

#' @importFrom methods as
#' @importFrom Matrix rowSums sparseVector t
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom sf st_as_sf st_geometry_type st_sfc st_point st_distance
setMethod(".mask", c("PointFrame", "ShapeFrame"), \(a, b) {
    n <- nrow(b <- st_as_sf(data(b)))
    fk <- meta(a)$spatialdata_attrs$feature_key
    switch(paste(st_geometry_type(b)[1]),
        POINT={
            # realize one feature at a time
            is <- split(seq_len(length(a)), a[[fk]])
            ns <- lapply(is, \(.) {
                # make points 'sf'-compliant
                xy <- as.data.frame(a[., c("x", "y")])
                ps <- st_sfc(lapply(asplit(xy, 1), st_point))
                # for each circle, count points within radius
                z <- rowSums(st_distance(b, ps) < b$radius)
                # sparsify counts
                sv <- sparseVector(z[i <- z > 0], which(i), n)
                sm <- as(sv, "sparseMatrix")
            })
            # collect intro matrix w/ dim. features x circles
            ns <- t(as(do.call(cbind, ns), "dgCMatrix"))
            rownames(ns) <- names(is)
            colnames(ns) <- seq(ncol(ns))
        })
    SingleCellExperiment(list(counts=ns))
})

#' @importFrom methods as
#' @importFrom SingleCellExperiment SingleCellExperiment
setMethod(".mask", c("ImageArray", "LabelArray"), \(a, b, fun=mean) {
    # TODO: rewrite w/o realizing everything at once
    stopifnot(dim(a)[-1] == dim(b))
    w <- c(data(b)); w[w == 0] <- NA 
    n <- length(i <- unique(w[!is.na(w)]))
    ns <- vapply(seq_len(dim(a)[1]), \(.) {
        v <- c(data(a, 1)[., , ]) 
        tapply(v, w, sum, na.rm=TRUE)
    }, numeric(n))
    ns <- t(as(ns, "dgCMatrix"))
    dimnames(ns) <- list(seq(dim(a)[1]), i)
    SingleCellExperiment(list(counts=ns))
})
setMethod(".mask", c("ANY", "ANY"), \(a, b) 
    stop("'mask'ing between these element types not yet supported."))
    
