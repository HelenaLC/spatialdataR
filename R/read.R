#' @name readSpatialData
#' @title Reading `SpatialData`
#' 
#' @aliases readImage readLabel readPoint readShape readTable
#'
#' @param x 
#'   For \code{readImage/Label/Point/Shape/Table}, 
#'   path to a \code{SpatialData} element.
#'   For \code{readSpatialData},
#'   path to a \code{SpatialData}-.zarr store.
#' @param images,labels,points,shapes,tables
#'   Control which elements should be read for each layer.
#'   The default, NULL, reads all elements; alternatively, may be FALSE 
#'   to skip a layer, or a integer vector specifying which elements to read.
#' @param anndataR, logical, default FALSE
#'   Uses \code{anndataR} to read tables if TRUE.
#'   Uses \code{basilisk}, \code{anndata} and \code{zellkonverter} to read tables if FALSE.
#'
#' @return 
#' \itemize{
#' \item{For element readers, a \code{ImageArray}, \code{LabelArray}, 
#' \code{PointFrame}, \code{ShapeFrame}, or \code{SingleCellExperiment}.}
#' \item{For \code{readSpatialData}, a \code{SpatialData}.}}
#'
#' @examples
#' tf <- tempfile()
#' dir.create(tf)
#' base <- unzip_merfish_demo(tf)
#' (x <- readSpatialData(base))
NULL

#' @rdname readSpatialData
#' @importFrom Rarr ZarrArray
#' @importFrom jsonlite fromJSON
#' @export
readImage <- function(x, ...) {
    md <- fromJSON(file.path(x, ".zattrs"))
    za <- ZarrArray(file.path(x, "0"))
    ImageArray(data=za, meta=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom Rarr ZarrArray
#' @importFrom jsonlite fromJSON
#' @export
readLabel <- function(x, ...) {
    md <- fromJSON(file.path(x, ".zattrs"))
    za <- ZarrArray(file.path(x, "0"))
    LabelArray(data=za, meta=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @export
readPoint <- function(x, ...) {
    md <- fromJSON(file.path(x, ".zattrs"))
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    PointFrame(data=open_dataset(pq), meta=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @export
readShape <- function(x, ...) {
    require(geoarrow, quietly=TRUE)
    md <- fromJSON(file.path(x, ".zattrs"))
    # TODO: previously had read_parquet(), 
    # but that doesn't work with geoparquet?
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    ShapeFrame(data=open_dataset(pq), meta=Zattrs(md))
}

#' @importFrom basilisk BasiliskEnvironment
.env <- BasiliskEnvironment(
    pkgname="SpatialData", envname="anndata_env",
    packages=c("anndata==0.9.1", "zarr==2.14.2"))

readTable <- function(x, anndataR = FALSE) {
    if(anndataR) {
        readTable_anndataR(x)
    } else {
        readTable_basilisk(x)
    }
}

#' @rdname readSpatialData
#' @importFrom reticulate import
#' @importFrom zellkonverter AnnData2SCE
#' @importFrom basilisk basiliskStart basiliskStop basiliskRun
#' @export
readTable_basilisk <- function(x) {
    proc <- basiliskStart(.env)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, zarr=x, \(zarr) {
        ad <- import("anndata")
        ad <- ad$read_zarr(zarr)
        AnnData2SCE(ad)
    })
}

#' @rdname readSpatialData
#' @importFrom anndataR read_zarr to_SingleCellExperiment
#' @export
readTable_anndataR <- function(x) {
    if (!requireNamespace('anndataR', quietly = TRUE)){
        stop("Install 'anndataR' to use this function.\nInstall this version: `remotes::install_github(\"keller-mark/anndataR\", ref = \"spatialdata\")`")
    }
    adata <- anndataR::read_zarr(x)
    anndataR::to_SingleCellExperiment(adata)
}

#' @rdname readSpatialData
#' @export
readSpatialData <- function(x, images=NULL, labels=NULL, points=NULL, shapes=NULL, tables=NULL, anndataR = FALSE) {
    # TODO: validity checks
    args <- as.list(environment())[.LAYERS]
    skip <- vapply(args, isFALSE, logical(1))
    lapply(.LAYERS[!skip], \(i) {
        j <- list.files(file.path(x, i), full.names=TRUE)
        if (is.numeric(args[[i]])) j <- j[args[[i]]]
        i <- paste0(toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)-1))
        setNames(lapply(j, get(paste0("read", i)), anndataR = anndataR), basename(j))
    }) |> do.call(what=SpatialData)
}
