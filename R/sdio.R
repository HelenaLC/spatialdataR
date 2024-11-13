
#' enumerate modules
#' @examples
#' available_sdio()
#' @export
available_sdio = function() {
 proc = basilisk::basiliskStart(.env, testload="spatialdata") # avoid package-specific import
 on.exit(basilisk::basiliskStop(proc))
 basilisk::basiliskRun(proc, function() {
     sdio = reticulate::import("spatialdata_io")
     setdiff(names(sdio), c("readers", "version"))
   })
}

#' use spatialdata-io to transform manufacturer output to zarr with specific folder structure
#' @param platform character(1) must be an element of `available_sdio()` output
#' @param srcdir character(1) path to folder holding manufacturer output files
#' @param dest character(1) a path to a desired destination for zarr representation
#' @examples
#' p1 = path_to_10x_xen_demo()
#' td = tempfile()
#' dir.create(td)
#' unzip(p1, exdir=td)
#' target = tempfile()
#' use_sdio("xenium", srcdir=td, dest=target)
#' br2fov = SpatialData::readSpatialData(target)
#' br2fov
#' @export
use_sdio = function(platform="xenium", srcdir, dest) {
 if (dir.exists(dest)) stop("won't write to existing folder, give non-existent path")
 proc = basilisk::basiliskStart(.env, testload="spatialdata") # avoid package-specific import
 on.exit(basilisk::basiliskStop(proc))
 basilisk::basiliskRun(proc, function(platform, srcdir, dest) {
     sdio = reticulate::import("spatialdata_io")
     avail = names(sdio)
     stopifnot(platform %in% available_sdio())
     sdio[[platform]](srcdir)$write(dest)
   }, platform=platform, srcdir=srcdir, dest=dest)
}

