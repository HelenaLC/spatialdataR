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
#' path <- file.path("extdata", "mibitof", "table", "table")
#' path <- system.file(path, package="SpatialData")
#' (sce <- readTable(path))
#'
#' @importFrom reticulate import
#' @importFrom zellkonverter AnnData2SCE
#' @importFrom basilisk basiliskStart basiliskStop basiliskRun
#' @export
readTable <- function(path){
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

.env <- basilisk::BasiliskEnvironment(
  pkgname="SpatialData",
  envname="anndata_loader_env",
  packages=c("anndata==0.9.1", "zarr==2.14.2"))
