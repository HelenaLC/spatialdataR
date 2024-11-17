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
#' @param anndataR logical specifying whether 
#'   to use \code{anndataR} to read tables; by default (FALSE), 
#'   \code{basilisk}, \code{anndata} and \code{zellkonverter} are used.
#' @param ... option arguments passed to and from other methods.
#'
#' @return 
#' \itemize{
#' \item{For \code{readSpatialData}, a \code{SpatialData}.},
#' \item{For element readers, a \code{ImageArray}, \code{LabelArray}, 
#' \code{PointFrame}, \code{ShapeFrame}, or \code{SingleCellExperiment}.}}
#'
#' @examples
#' tf <- tempfile()
#' dir.create(tf)
#' base <- unzip_merfish_demo(tf)
#' (x <- readSpatialData(base))


#' @rdname readSpatialData
#' @importFrom Rarr ZarrArray
#' @importFrom jsonlite fromJSON
#' @export
readImage <- function(x, ...) {
    md <- fromJSON(file.path(x, ".zattrs"))
    paths <- .get_multiscales_dataset_paths(md)
    za_list <- lapply(paths, function(ps)
        ZarrArray(file.path(x, as.character(ps))))
    meta <- Zattrs(md)
    ImageArray(data=za_list, meta=meta, ...)
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
    requireNamespace("geoarrow", quietly=TRUE)
    md <- fromJSON(file.path(x, ".zattrs"))
    # TODO: previously had read_parquet(), 
    # but that doesn't work with geoparquet?
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    ShapeFrame(data=open_dataset(pq), meta=Zattrs(md))
}

#' @importFrom basilisk BasiliskEnvironment
.env <- BasiliskEnvironment(
    pkgname="SpatialData", envname="anndata_env",
    packages=c("anndata==0.9.1", "zarr==2.14.2", "spatialdata==0.2.3", "spatialdata-io==0.1.4"))

#' @importFrom reticulate import
#' @importFrom zellkonverter AnnData2SCE
#' @importFrom S4Vectors metadata metadata<-
#' @importFrom SingleCellExperiment int_metadata int_metadata<-
#' @importFrom basilisk basiliskStart basiliskStop basiliskRun
.readTable_basilisk <- function(x) {
    proc <- basiliskStart(.env)
    on.exit(basiliskStop(proc))
    sce <- basiliskRun(proc, zarr=x, \(zarr) {
        ad <- import("anndata")
        ad <- ad$read_zarr(zarr)
        AnnData2SCE(ad)
    })
    nm <- "spatialdata_attrs"
    md <- metadata(sce)[[nm]]
    int_metadata(sce)[[nm]] <- md[[nm]]
    metadata(sce)[[nm]] <- NULL
    return(sce)
}

.readTable_anndataR <- function(x) {
    if (!requireNamespace('anndataR', quietly=TRUE)) {
        stop("To use this function, install the 'anndataR' package via\n",
            "`BiocManager::install(\"keller-mark/anndataR\", ref=\"spatialdata\")`")
    }
    if (!requireNamespace('pizzarr', quietly=TRUE)) {
        stop("To use this function, install the 'pizzarr' package via\n",
            "`BiocManager::install(\"keller-mark/pizzarr\")`")
    }
    suppressWarnings({ # suppress warnings related to hidden files
        adata <- anndataR::read_zarr(x)
        anndataR::to_SingleCellExperiment(adata)
    })
}

#' @rdname readSpatialData
#' @export
readTable <- function(x, anndataR=FALSE) {
    if (anndataR) {
        .readTable_anndataR(x)
    } else {
        .readTable_basilisk(x)
    }
}

#' @rdname readSpatialData
#' @export
readSpatialData <- function(x, 
    images=TRUE, labels=TRUE, points=TRUE, 
    shapes=TRUE, tables=TRUE, anndataR=TRUE) {
    args <- as.list(environment())[.LAYERS]
    skip <- vapply(args, isFALSE, logical(1))
    lapply(.LAYERS[!skip], \(i) {
        y <- file.path(x, i)
        j <- list.files(y, full.names=TRUE)
        names(j) <- basename(j)
        if (!isTRUE(opt <- args[[i]])) {
            if (is.numeric(opt) && opt > (. <- length(j))) 
                stop("'", i, "=", opt, "', but only ", ., " elements found")
            if (is.character(opt) && length(. <- setdiff(opt, basename(j))))
                stop("couln't find ", i, " of name", .)
            j <- j[opt]
        } 
        args <- if (i == "tables") list(anndataR=anndataR)
        f <- get(paste0("read", toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)-1)))
        lapply(j, \(.) do.call(f, c(list(.), args)))
    }) |> do.call(what=SpatialData)
}
