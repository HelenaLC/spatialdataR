#' @rdname readTable
#' @title `SingleCellExperiment` from `AnnData`-Zarr
#' @description ...
#'
#' @param path A character string specifying
#'   the path to a `table/` subdirectory.
#'
#' @return \code{SingleCellExperiment}
#'
#' @examples
#' path <- file.path("extdata", "blobs", "table", "table")
#' path <- system.file(path, package="SpatialData")
#' (sce <- readTable(path))
#'
#' @author Constantin Ahlmann-Eltze
#'
#' @importFrom reticulate import
#' @importFrom zellkonverter AnnData2SCE
#' @importFrom basilisk basiliskStart basiliskStop basiliskRun
#' @export
readTable <- function(path) {
    proc <- basiliskStart(.env)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, function(zarr) {
        # return value MUST be a pure R object,
        # i.e., no reticulate Python objects
        # or pointers to shared memory
        ad <- import("anndata")
        obj <- ad$read_zarr(zarr)
        sce <- AnnData2SCE(obj)
    }, zarr=path)
}
