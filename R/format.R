#' @name sdFormat
#' @title The `sdFormat` class
#' 
#' @param version SpatialData version: 0.1 or 0.2.
#' 
#' @details
#' 
#' @return \code{sdFormat}
#' 
#' @noRd
sdFormat <- function(version = "0.1") {
  switch(as.character(version), 
         "0.2" = {
           .sdFormat(
             version = "0.2",
             zarr_version = 3L,
             ome_version = "0.5",
             image = "0.3",
             label = "0.3",
             shape = "0.3",
             point = "0.2",
             table = "0.2"
           )
         }, 
         "0.1" = {
           .sdFormat(
             version = "0.1",
             zarr_version = 2L,
             ome_version = "0.5",
             image = "0.2",
             label = "0.2",
             shape = "0.2",
             point = "0.1",
             table = "0.1"
           )
         },
         stop("Incorrect SpatialData version. Must be 0.1 or 0.2!")
  )
}

setMethod("image", "sdFormat", \(x) x@image)
setMethod("label", "sdFormat", \(x) x@label)
setMethod("shape", "sdFormat", \(x) x@shape)
setMethod("point", "sdFormat", \(x) x@point)
setMethod("table", "sdFormat", \(x) x@table)
setMethod("zarr_version", "sdFormat", \(x) x@zarr_version)
setMethod("zarr_version", "character", \(x) zarr_version(sdFormat(x)))
setMethod("ome_version", "sdFormat", \(x) x@ome_version)
setMethod("version", "sdFormat", \(x) x@version)