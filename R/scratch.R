#' @importFrom methods as
#' @importFrom anndataR read_zarr
#' @importFrom SpatialExperiment SpatialExperiment
readSDasSPE <- \(x, i=1) { 
    if (!requireNamespace("SpatialExperiment", quietly=TRUE))
        stop("install the 'SpatialExperiment' package to use this function.")
    y <- list.files(file.path(x, "tables"), full.names=TRUE)
    if (!length(y)) 
        stop("couldn't find any tables")
    if (is.character(i)) 
        i <- match(i, basename(y))
    sce <- readTable(y[i])
    colData(sce) <- NULL
    for (l in setdiff(.LAYERS, "tables")) {
        y <- list.files(file.path(x, l), full.names=TRUE)
        y <- y[match(region(sce), basename(y))]
        if (!is.na(y)) {
            t <- basename(dirname(y))
            f <- paste0("read", 
                toupper(substr(t, 1, 1)), 
                substr(t, 2, nchar(t)-1))
            y <- get(f)(y)
            xy <- centroids(y, "matrix")
            xy <- xy[, c("x", "y")]
        }
    }
    toSpatialExperiment(sce, spatialCoords=xy)
}

require(sf, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
(spe <- readSDasSPE(x))
