
# Note as of 14 Nov 2024 this code requires use of spatialdata python version https://github.com/scverse/spatialdata/tree/fix/transform_to_data_extent
# Signature:
#<function transform_to_data_extent at 0x170633370>
# signature: (
#   sdata: 'SpatialData', 
#   coordinate_system: 'str', 
#   maintain_positioning: 'bool' = True, 
#   target_unit_to_pixels: 'float | None' = None, 
#   target_width: 'float | None' = None, 
#   target_height: 'float | None' = None, 
#   target_depth: 'float | None' = None
#) -> 'SpatialData'


#' Use Python's 'spatialdata' 'transform_to_data_extent' on a spatialdata zarr store
#' @param srcdir character(1) path to folder holding a zarr store
#' @param dest character(1) a path to a desired destination for new zarr representation
#' @param coordinate_system character(1) defaults to "global"
#' @param ... arguments for passage to python `transform_to_data_extent`;
#'   can include "maintain_positioning" (logical (1)) or numerics for
#'   target_unit_to_pixels, target_width, target_height, target_depth.
#'   
#' @examples
#' src <- system.file("extdata", "blobs.zarr", package="SpatialData")
#' td <- tempfile()
#' do_tx_to_ext(
#'   srcdir=src, dest=td, 
#'   coordinate_system="global",
#'   maintain_positioning=FALSE, 
#'   target_width=400.)
#' (sd <- readSpatialData(td))
#' 
#' @export
do_tx_to_ext <- function(srcdir, dest, coordinate_system, ...) {
    if (dir.exists(dest)) 
        stop("Won't write to existing folder;",
            " please provide a non-existent path.")
    # avoid package-specific import
    proc <- basilisk::basiliskStart(.env, testload="spatialdata") 
    on.exit(basilisk::basiliskStop(proc))
    basilisk::basiliskRun(proc, \(srcdir, dest, coordinate_system, ...) {
        sd <- reticulate::import("spatialdata")
        ini <- sd$read_zarr(srcdir)
        txfun <- sd$`_core`$operations$`_utils`$transform_to_data_extent
        post <- txfun(ini, coordinate_system, ... )
        post$write(dest)
    }, srcdir=srcdir, dest=dest, coordinate_system=coordinate_system, ...)
}

# sd$`_core`$operations$`_utils`$transform_to_data_extent(merpy, "global", target_height=400.) -> tt
