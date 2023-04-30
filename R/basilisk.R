#' @importFrom basilisk BasiliskEnvironment
.env <- BasiliskEnvironment(
    pkgname="SpatialData",
    envname="anndata_env",
    packages=c("anndata==0.9.1", "zarr==2.14.2"))
