OSN_PATH_MERFISH_DEMO = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocSpatialData/merfish.zarr.zip"

#' check cache for merfish.zarr.zip and return path; retrieve/stash/return path if not found
#' @import BiocFileCache
#' @param cache defaults to BiocFileCache::BiocFileCache(), will serve as destination or source
#' for data
#' @return Returns character(1) path to cached zip file.
#' @export
merfish_demo_path = function(cache = BiocFileCache::BiocFileCache()) {
  info = bfcquery(cache, "merfish.zarr.zip")
  nrec = nrow(info)
  if (nrec > 1) {
    message("multiple 'merfish.zarr.zip' found in cache, using last recorded")
    }
  if (nrec == 1) {
    message("returning path to cached zip")
    return(info$rpath[nrec])
    }
  message("retrieving from OSN, caching, and returning path")
  bfcadd(cache, rname="merfish.zarr.zip", fpath=OSN_PATH_MERFISH_DEMO, rtype="web")
}

#' unzip cached merfish demo data to specified folder
#' @param destination character(1) a path to a folder that must exist
#' @param cache defaults to BiocFileCache::BiocFileCache(), will serve as destination or source
#' @return Returns path to 'merfish.zarr'.
#' @examples
#' tf = tempfile()
#' dir.create(tf)
#' pa = unzip_merfish_demo(tf)
#' dir(pa, full.names=TRUE)
#' @export
unzip_merfish_demo = function(destination, cache=BiocFileCache::BiocFileCache()) {
  stopifnot(dir.exists(destination))
  unzip( merfish_demo_path(cache=cache), exdir=destination )
  dir(destination, patt="merfish.zarr", full.names=TRUE)
}
