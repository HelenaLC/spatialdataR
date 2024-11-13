bsklenv_sdio <- basilisk::BasiliskEnvironment(envname="bsklenv",
    pkgname="SpatialData",
    packages=c("spatialdata==0.2.3", "spatialdata-io==0.1.4"))


#' enumerate modules
#' @examples
#' available_sdio()
#' @export
available_sdio = function() {
 proc = basilisk::basiliskStart(bsklenv_sdio, testload="spatialdata") # avoid package-specific import
 on.exit(basilisk::basiliskStop(proc))
 basilisk::basiliskRun(proc, function() {
     sdio = reticulate::import("spatialdata_io")
     setdiff(names(sdio), c("readers", "version"))
   })
}

#' use a reader
#' @examples
#' use_sdio("xenium") # not basilisk-standards compliant -- leaks python reference into R
#' @export
use_sdio = function(platform="xenium") {
 proc = basilisk::basiliskStart(bsklenv_sdio, testload="spatialdata") # avoid package-specific import
 on.exit(basilisk::basiliskStop(proc))
 basilisk::basiliskRun(proc, function(platform) {
     sdio = reticulate::import("spatialdata_io")
     avail = names(sdio)
     stopifnot(platform %in% available_sdio())
     sdio[[platform]]
   }, platform=platform)
}

