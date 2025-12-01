#' @name sd_data
#' @title Demo 'SpatialData'sets
#' 
#' @aliases sd_osn_list sd_osn_load
#' 
#' all logic for finding, caching, loading an OSN-based dataset, hidden
#' @param patt string sufficient to identify an OSN resource
#' @param cache like \code{BiocFileCache}
#' @param target sting, defaults to \code{tempfile()}; use a different 
#'   value if you wish to retain the unzipped .zarr store persistently.
#' @note This function checks for stale element in cache and uses 
#'   \code{bfcupdate()} to rectify before retrieving from cache.
#' 
#' @return 
#' \code{sd_osn_list}: character vector of .zip archives,
#' \code{sd_osn_load}: \code{\link{SpatialData}} object.
#' 
#' @examples
#' # use 'paws::s3' to interrogate an NSF Open Storage Network 
#' # bucket for zipped .zarr archives for various platforms
#' Sys.setenv(AWS_REGION="us-east-1")
#' if (requireNamespace("paws")) sd_osn_list()
#'
#' # retrieve dataset & ingest
#' (sd <- sd_osn_load("merfish"))
NULL

.require <- \(x) if (!requireNamespace(x, quietly=TRUE)) 
    stop(sprintf("Install '%s' to use this function.", x))

#' @rdname sd_data
#' @export
sd_osn_list <- \() {
    #  x = curl::curl("https://mghp.osn.xsede.org/bir190004-bucket01")
    #  y = xml2::read_xml(x)
    #  z = xml2::as_list(y)
    .require("paws")
    message("checking Bioconductor OSN bucket...")
    s3 <- paws::s3(
        credentials=list(anonymous=TRUE),
        endpoint="https://mghp.osn.xsede.org")
    zz <- s3$list_objects(
        Bucket="bir190004-bucket01", 
        Prefix="BiocSpatialData") 
    keys <- lapply(zz$Contents, "[[", "Key")
    basename(grepv("/", keys))
}

#' @rdname sd_data
#' @importFrom utils unzip
#' @importFrom BiocFileCache BiocFileCache 
#'   bfcquery bfcadd bfcupdate bfcneedsupdate
#' @export
sd_osn_load <- \(patt, 
    target=tempfile(),
    cache=BiocFileCache::BiocFileCache()) {
    
    # Bioconductor's OSN bucket
    buckprefix <- "https://mghp.osn.xsede.org/bir190004-bucket01"
    
    # work on zipped Zarr archives from scverse SpatialData datasets page
    sdfold <- "BiocSpatialData"
    
    # sdzips <- c(
    #     "mcmicro_io.zip", "merfish.zarr.zip", 
    #     "mibitof.zip", "steinbock_io.zip", 
    #     "visium_associated_xenium_io_aligned.zip", "visium_hd_3.0.0_io.zip",
    #     "xenium_rep1_io_aligned.zip", "xenium_rep2_io_aligned.zip",
    #     "HuLungXenmulti.zip")
    suppressMessages(sdzips <- sd_osn_list())
    sdurls <- paste(buckprefix, sdfold, sdzips, sep="/")
    
    # work on zipped Xenium minimal outputs, retrieved and zipped in OSN
    # these must be expanded and processed with use_sdio
    xdfold <- "BiocXenDemo"
    xdzips <- c(
        "Xenium_V1_human_Breast_2fov_outs.zip",
        "Xenium_V1_human_Lung_2fov_outs.zip")
    
    # collect names of all zip files
    allz <- c(sdzips, xdzips)
    # build a tibble with all relevant information
    xdurls <- paste(buckprefix, xdfold, xdzips, sep="/")
    allurls <- c(sdurls, xdurls)
    
    ca <- BiocFileCache::BiocFileCache()
    chk <- lapply(allurls, \(x) BiocFileCache::bfcquery(ca, x))
    chkdf <- do.call(rbind, chk)
    ind <- grep(patt, chkdf$rname)
    nupatt <- "pattern does not uniquely identify a resource, please be more specific"
    if (length(ind) > 1) stop(nupatt)
    if (length(ind) == 0) {
        chkxen <- grep(patt, xdzips)
        if (length(chkxen) > 1) stop(nupatt)
        if (length(chkxen) == 0) {   # add a zipped zarr
            zipind = grep(patt, sdzips)  # already ruled out xenium group, must be from spatialdata archive
            if (length(zipind) == 0) stop("patt not matched in available resources")
            zipname <- sdzips[zipind]
            message(sprintf("caching %s", zipname))
            fpath <- sdurls[zipind]
            loc <- BiocFileCache::bfcadd(cache, rname=zipname, fpath=fpath, rtype="web")
            td <- target
            dir.create(td)
            unzip(loc, exdir=td)
            return(SpatialData::readSpatialData(dir(td, full.names=TRUE)))
        } # end zipped zarr, now retrieve Xenium, run use_sdio, then read
        zipname <- xdzips[chkxen]
        message(sprintf("caching %s", zipname))
        fpath <- xdurls[chkxen]
        preloc <- BiocFileCache::bfcadd(cache, rname=zipname, fpath=fpath, rtype="web")
        td <- tempfile() # can't use target'
        dir.create(td)
        unzip(preloc, exdir=td)  # manufacturer output
        if (dir.exists(target)) print("target exists")
        use_sdio("xenium", srcdir=td, dest=target) # zarr in target
        return(SpatialData::readSpatialData(target))
    }
    # so a single pattern has hit, and it is in cache
    if (chkdf[ind,]$rname %in% xdzips) { # it is a Xenium 10x output resource
        # check if update needed
        stale = BiocFileCache::bfcneedsupdate(cache, chkdf[ind,]$rid)
        if (stale) BiocFileCache::bfcupdate(cache, chkdf[ind,]$rid, fpath=chkdf[ind,]$fpath, rtype="web")
        preloc <- chkdf[ind,]$rpath
        td <- tempfile() # can't use target
        dir.create(td)
        unzip(preloc, exdir=td)  # manufacturer output
        use_sdio("xenium", srcdir=td, dest=target) # zarr in target
        return(SpatialData::readSpatialData(target))
    }
    # check again, this one is not in xdzips
    stale = BiocFileCache::bfcneedsupdate(cache, chkdf[ind,]$rid)
    if (stale) BiocFileCache::bfcupdate(cache, chkdf[ind,]$rid, fpath=chkdf[ind,]$fpath, rtype="web")
    loc <- chkdf[ind,]$rpath
    td <- target
    dir.create(td)
    unzip(loc, exdir=td)
    readSpatialData(dir(td, full.names=TRUE))
}

#' Use Python's 'spatialdata-io' to transform manufacturer 
#' output to .zarr with specific folder structure.
#' 
#' @param platform string; must be an element of `sdio_list()` output
#' @param srcdir string; path to folder holding manufacturer output files
#' @param dest string; path to a desired destination for .zarr representation
#' @param env (optional) string passed to `reticulate::use_condaenv()`;
#'   can be used to specify a conda environment with the 
#'   `spatialdata_io` Python module already installed.
#'   .
#' @return none, but a .zarr store is created 
#'   in the specified \code{dest} location.
#' 
#' @examples
#' # unzip flat files
#' pa <- SpatialData.data:::.path_to_10x_xen_demo()
#' dir.create(td <- tempfile())
#' unzip(pa, exdir=td)
#' 
#' # read & write to .zarr w/ 'spatialdata-io'
#' out <- tempfile()
#' sd_make("xenium", 
#'   srcdir=td, dest=out,
#'   env="spatialdata-io")
#' (sd <- readSpatialData(out))
#' 
#' @export
sd_make <- \(platform, srcdir, dest, env) {
    if (dir.exists(dest))
        stop("Won't write to existing folder;",
            " please provide a non-existent path.")
    opts <- c(
        "codex", "cosmx", "curio", "dbit", "macsima", "mcmicro", "merscope", 
        "seqfish", "steinbock", "stereoseq", "visium", "visium_hd", "xenium")
    if (missing(env)) {
        proc <- basilisk::basiliskStart(SpatialData:::.env)#, testload="spatialdata")
        on.exit(basilisk::basiliskStop(proc))
        basilisk::basiliskRun(proc, \(x, srcdir, dest) {
            sdio <- reticulate::import("spatialdata_io")
            stopifnot(platform %in% sdio_list())
            sdio[[platform]](srcdir)$write(dest)
        }, platform=platform, srcdir=srcdir, dest=dest)
    } else {
        reticulate::use_condaenv(env)
        sdio <- reticulate::import("spatialdata_io")
        sdio[[match.arg(platform, opts)]](srcdir)$write(dest)
    }
}

#' @rdname sd_data
#' @export
sdio_list <- \(env) {
    .require("basilisk")
    .require("reticulate")
    if (missing(env)) {
        proc <- basilisk::basiliskStart(.env, testload="spatialdata") 
        on.exit(basilisk::basiliskStop(proc))
        basilisk::basiliskRun(proc, \() {
            sdio <- reticulate::import("spatialdata_io")
        })
    } else {
        reticulate::use_condaenv(env)
        sdio <- reticulate::import("spatialdata_io")
    }
    setdiff(names(sdio), c("readers", "version"))
}
