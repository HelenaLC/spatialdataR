OSN_PATH_MERFISH_DEMO <- "https://mghp.osn.xsede.org/bir190004-bucket01/BiocSpatialData/merfish.zarr.zip"
OSN_PATH_MIBITOF_DEMO <- "https://mghp.osn.xsede.org/bir190004-bucket01/BiocSpatialData/mibitof.zip"
OSN_PATH_VISIUM_HD_DEMO <- "https://mghp.osn.xsede.org/bir190004-bucket01/BiocSpatialData/visium_hd_3.0.0_io.zip"

build_osn_path <- function(zipname) {
    sprintf("https://mghp.osn.xsede.org/bir190004-bucket01/BiocSpatialData/%s", zipname)
}

build_osn_xdemo_path <- function(zipname) {
    sprintf("https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenDemo/%s", zipname)
}

build_sandbox_path <- function(zipname) {
    sprintf("https://s3.embl.de/spatialdata/spatialdata-sandbox/%s", zipname)
}

#' check cache for merfish.zarr.zip and return path; 
#' retrieve/stash/return path if not found
#' 
#' @param cache defaults to BiocFileCache::BiocFileCache(), 
#'   will serve as destination or source for data
#' @param zipname character(1) name of zip archive to find
#' 
#' @return Returns character(1) path to cached zip file.
#' 
#' @import BiocFileCache
#' @export
merfish_demo_path <- function(cache=BiocFileCache::BiocFileCache(), zipname="merfish.zarr.zip") {
    .get_spdzip_path_in_cache_add_if_needed(cache=cache, zipname=zipname, source="biocOSN")
}

#' unzip cached merfish demo data to specified folder
#' @param destination character(1) a path to a folder that must exist
#' @param cache defaults to BiocFileCache::BiocFileCache(), will serve as destination or source
#' @return Returns path to 'merfish.zarr'.
#' @examples
#' tf <- tempfile()
#' dir.create(tf)
#' pa <- unzip_merfish_demo(tf)
#' dir(pa, full.names=TRUE)
#' 
#' @importFrom utils unzip
#' @export
unzip_merfish_demo <- function(destination, cache=BiocFileCache::BiocFileCache()) {
    stopifnot(dir.exists(destination))
    unzip(merfish_demo_path(cache=cache), exdir=destination)
    dir(destination, pattern="merfish.zarr", full.names=TRUE)
}

#' use 'paws::s3' to interrogate an NSF Open Storage Network 
#' bucket for zipped zarr archives for various platforms
#' @examples
#' if (requireNamespace("paws")) {
#'   available_spd_zarr_zips()
#' }
#' @export
available_spd_zarr_zips <- function() {
    if (!requireNamespace("paws")) 
        stop("install 'paws' to use this function; without it",
            " we can't check existence of data in OSN bucket")
#  x = curl::curl("https://mghp.osn.xsede.org/bir190004-bucket01")
#  y = xml2::read_xml(x)
#  z = xml2::as_list(y)
    message("checking Bioconductor OSN bucket...")
    s3 <- paws::s3(
        credentials=list(anonymous=TRUE),
        endpoint="https://mghp.osn.xsede.org")
    zz <- s3$list_objects("bir190004-bucket01") 
    allk <- lapply(zz$Contents, "[[", "Key")
    basename(grep("BiocSpatialData\\/", allk, value=TRUE))
}

#' obtain path to cached zip archive of SpatialData zarr
#' @param cache inherits from \code{BiocFileCache::BiocFileCache()}
#' @param zipname character(1) name of zip archive to find
#' @param source character(1) one of "biocOSN", "sandbox", "local"
#' @examples
#' spdzPath(zipname="merfish.zarr.zip", source="biocOSN")
#' @export
spdzPath <- function(cache=BiocFileCache::BiocFileCache(), zipname, source) {
    if (missing(zipname)) stop("zipname must be supplied")
    # protect user from bad request if paws is available
    if (requireNamespace("paws")) { 
        avail <- available_spd_zarr_zips()
        stopifnot(zipname %in% avail)
    }
    .get_spdzip_path_in_cache_add_if_needed(cache=cache, zipname=zipname, source)
}

.get_spdzip_path_in_cache_add_if_needed <- function(cache=BiocFileCache::BiocFileCache(), zipname, source) {
    info <- BiocFileCache::bfcquery(cache, zipname)
    nrec <- nrow(info)
    if (nrec > 1) {
        message(sprintf("multiple %s found in cache, using last recorded", zipname))
    }
    if (nrec == 1) {
        message("returning path to cached zip")
        return(info$rpath[nrec])
    }
    pathbuilder <- switch(source,
        biocOSN=build_osn_path,
        sandbox=build_sandbox_path, 
        other=force)
    message(sprintf("retrieving from %s, caching, and returning path", source))
    BiocFileCache::bfcadd(cache, rname=zipname, fpath=pathbuilder(zipname), rtype="web")
}

#' check cache for demonstration .zarr.zip and return path; 
#' retrieve data, cache it, return path if not found
#' 
#' @param cache defaults to \code{BiocFileCache::BiocFileCache()}
#' @param source character(1) one of "biocOSN", "sandbox", "local"
#' @param zipname character(1) should be found in Bioconductor OSN bucket
#' 
#' @return Returns character(1) path to cached zip file.
#' 
#' @import BiocFileCache
#' @export
spd_demo_cached_path <- function(cache=BiocFileCache::BiocFileCache(), zipname="mibitof.zip", source) {
    .get_spdzip_path_in_cache_add_if_needed(cache=cache, zipname=zipname, source=source)
}

#' unzip selected demo data to specified folder
#' @param zipname character(1) should be name of zipped zarr archive found in Bioconductor OSN bucket
#' @param destination character(1) a path to a folder that must exist
#' @param cache defaults to BiocFileCache::BiocFileCache(), will serve as destination or source
#' @param source character(1) one of "biocOSN", "sandbox", "local"
#' @return Returns path to base of unzipped archive
#' @examples
#' tf <- tempfile()
#' dir.create(tf)
#' pa <- unzip_spd_demo(zipname="mibitof.zip", destination=tf, source="biocOSN")
#' dir(pa, full.names=TRUE)
#' @export
unzip_spd_demo <- function(zipname="mibitof.zip", destination, cache=BiocFileCache::BiocFileCache(), source) {
    stopifnot(dir.exists(destination))
    chk <- try({
        fnm <- spd_demo_cached_path(cache=cache, zipname=zipname, source=source)
        unzip(fnm, exdir=destination)
    })
    if (inherits(chk, "try-error")) 
        stop("problem with unzipping cached zip archive")
    dir(destination, full.names=TRUE)
}

#' use 'paws::s3' to interrogate an NSF Open Storage Network 
#' bucket for zipped 10x-produced Xenium outputs
#' @examples
#' if (requireNamespace("paws")) {
#'   available_10x_xen_zips()
#' }
#' @export
available_10x_xen_zips <- function() {
    if (!requireNamespace("paws")) 
        stop("install 'paws' to use this function; without it",
            " we can't check existence of data in OSN bucket")
#  x = curl::curl("https://mghp.osn.xsede.org/bir190004-bucket01")
#  y = xml2::read_xml(x)
#  z = xml2::as_list(y)
    message("checking Bioconductor OSN bucket...")
    s3 <- paws::s3(
        credentials=list(anonymous=TRUE),
        endpoint="https://mghp.osn.xsede.org")
    zz <- s3$list_objects("bir190004-bucket01") 
    allk <- lapply(zz$Contents, "[[", "Key")
    basename(grep("BiocXenDemo\\/", allk, value=TRUE))
}

#' provide path to a zip file from 10x genomics for Xenium platform
#' @param zipname character(1) name of zip archive to find
#' @examples
#' path_to_10x_xen_demo()
#' # see ?use_sdio
#' @export
path_to_10x_xen_demo <- function(zipname="Xenium_V1_human_Breast_2fov_outs.zip") {
    .cache_add_if_needed_xendemo(
        cache=BiocFileCache::BiocFileCache(),
        zipname=zipname, source="biocOSN")
}

.cache_add_if_needed_xendemo <- function(cache=BiocFileCache::BiocFileCache(), 
       zipname="Xenium_V1_human_Breast_2fov_outs.zip", source="biocOSN") {
    info <- BiocFileCache::bfcquery(cache, zipname)
    nrec <- nrow(info)
    if (nrec > 1) {
        message(sprintf("multiple %s found in cache, using last recorded", zipname))
    }
    if (nrec == 1) {
        message("returning path to cached zip")
        return(info$rpath[nrec])
    }
    fp <- build_osn_xdemo_path(zipname)
    message(sprintf("retrieving from %s, caching, and returning path", source))
    BiocFileCache::bfcadd(cache, rname=zipname, fpath=fp, rtype="web")
}
