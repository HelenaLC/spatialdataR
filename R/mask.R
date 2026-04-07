#' @name mask
#' @title Masking
#'
#' @description ...
#' 
#' @param x \code{\link{SpatialData}} object.
#' @param i,j character string; names of elements to mask,
#'   specifically, \code{i} will be masked by \code{j},
#'   adding a \code{table} for \code{j} in \code{x}.
#' @param how character string; statistic to use for masking.
#'   Defaults to "mean" when masking images, ignored when masking points.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return 
#' Input \code{SpatialData} object \code{x} with an additional table named 
#' \code{<j>_masking_<i>}; or a \code{SingleCellExperiment} object when 
#' masking elements directly (i.e., without \code{x} as input).
#'
#' @examples
#' library(SingleCellExperiment)
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#'
#' # count points in shapes
#' y <- mask(x, "blobs_points", "blobs_circles")
#' (sce <- mask(i=point(x), j=shape(x, 1)))
#' identical(assay(table(y)), assay(sce))
#' 
#' # average image channels by labels
#' y <- mask(x, "blobs_image", "blobs_labels")
#' (sce <- mask(i=image(x), j=label(x)))
#' identical(assay(table(y)), assay(sce))
#'
#' @export
NULL

# TODO: table from point + shape, image + label etc. etc. etc.

#' @rdname mask
#' @importFrom SingleCellExperiment int_colData int_colData<- int_metadata<-
#' @export
setMethod("mask", c("SpatialData", "ANY", "ANY"), \(x, i, j, how=NULL) {
    stopifnot(length(i) == 1, is.character(i), i %in% unlist(colnames(x)))
    stopifnot(length(j) == 1, is.character(j), j %in% unlist(colnames(x)))
    # get element types
    f <- \(i) names(which(rapply(colnames(x), \(.) i %in% ., "character")))
    t <- mask(i=element(x, f(i), i), j=element(x, f(j), j), how=how)
    md <- list(region=j, 
        region_key="region", 
        instance_key="instance")
    int_metadata(t)$spatialdata_attrs <- md
    cd <- data.frame(region=j, instance=colnames(t))
    int_colData(t) <- cbind(int_colData(t), cd)
    nm <- paste0(j, "_masking_", i)
    `table<-`(x, nm, value=t)
})

#' @importFrom methods as
#' @importFrom Matrix rowSums sparseVector t
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom sf st_as_sf st_geometry_type st_distance
setMethod("mask", c("missing", "PointFrame", "ShapeFrame"), \(x, i, j, how=NULL) {
    if (!is.null(how)) warning("Can only count when masking points; ignoring 'how'")
    n <- nrow(j <- st_as_sf(data(j)))
    fun <- switch(as.character(st_geometry_type(j[1, ])),
        POINT=\(i, j) rowSums(st_distance(j, i) <= j$radius),
        \(i, j) vapply(st_intersects(j, i), length, integer(1)))
    # realize one feature at i time
    is <- split(seq_len(length(i)), i[[feature_key(i)]])
    ns <- lapply(is, \(.) {
        # make points 'sf'-compliant
        i <- as.data.frame(i[., c("x", "y")])
        i <- st_as_sf(i, coords=c("x", "y"))
        # for each shape, count intersecting points
        z <- fun(i, j)
        # sparsify counts
        sv <- sparseVector(z[i <- z > 0], which(i), n)
        sm <- as(sv, "sparseMatrix")
    })    
    # collect into matrix w/ dim. features x shapes
    ns <- t(do.call(cbind, ns))
    rownames(ns) <- names(is)
    colnames(ns) <- seq(ncol(ns))
    SingleCellExperiment(list(counts=ns))
})

#' @importFrom methods as
#' @importFrom DelayedArray realize
#' @importFrom S4Arrays as.array.Array
#' @importFrom SingleCellExperiment SingleCellExperiment
setMethod("mask", c("missing", "ImageArray", "LabelArray"), \(x, i, j, how=NULL) {
    if (is.null(how)) how <- "mean"
    stopifnot(dim(i)[-1] == dim(j))
    .j <- as(data(j), "sparseVector")
    .j <- as.vector(.j[ok <- .j > 0])
    mx <- apply(data(i), 1, \(.i) {
        .i <- as(.i, "sparseVector")
        .i <- as.vector(.i[ok])
        tapply(.i, .j, how)
    })
    colnames(mx) <- channels(i)
    se <- SingleCellExperiment(list(t(mx)))
    assayNames(se) <- how
    return(se)
})

setMethod("mask", c("missing", "ANY", "ANY"), \(x, i, j, how=NULL) 
    stop("'mask'ing between these element types not yet supported"))
