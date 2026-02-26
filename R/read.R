#
# allp = c("session_info==1.0.0", "spatialdata==0.3.0", "spatialdata_io==0.1.7", 
# "pillow==11.1.0", "anndata==0.11.3", "annotated_types==0.7.0", "asciitree==0.3.3", 
# "attr==0.3.2", "certifi==2025.01.31", "charset_normalizer==3.4.1", 
# "click==8.1.8", "cloudpickle==3.1.1", "cycler==0.12.1", "dask==2024.4.1", 
# "dask_image==2024.5.3", "datashader==0.17.0", 
# "deprecated==1.2.18", "distributed==2024.4.1", 
# "flowio==1.3.0", "fsspec==2025.2.0", "geopandas==1.0.1", "h5py==3.12.1", 
# "idna==3.10", "imagecodecs==2024.12.30", "imageio==2.37.0", "jinja2==3.1.5", 
# "joblib==1.4.2", "kiwisolver==1.4.8", "lazy_loader==0.4", "legacy_api_wrap==1.4.1", 
# "llvmlite==0.44.0", "locket==1.0.0", "markupsafe==3.0.2", "matplotlib==3.10.0", 
# "more_itertools==10.3.0", "msgpack==1.1.0", "multipledispatch==0.6.0", 
# "multiscale_spatial_image==2.0.2", "natsort==8.4.0", "networkx==3.4.2", 
# "numba==0.61.0", "numcodecs==0.15.1", "numpy==2.1.3", "ome_types==0.5.3", 
# "ome_zarr==0.10.3", "packaging==24.2", "pandas==2.2.3", "param==2.2.0", 
# "pims==0.7", "platformdirs==4.3.6", "psutil==7.0.0", "pyarrow==19.0.0", 
# "pyct==0.5.0", "pydantic==2.10.6", "pydantic_compat==0.1.2", 
# "pydantic_core==2.27.2", "pygments==2.19.1", "pyparsing==3.2.1", 
# "pyproj==3.7.0", "pytz==2025.1", "readfcs==2.0.1", "requests==2.32.3", 
# "rich==13.9.4", "scanpy==1.11.0", "scipy==1.15.1", "setuptools==75.8.0", 
# "shapely==2.0.7", "six==1.17.0", "scikit-image==0.25.1", "scikit-learn==1.5.2", 
# "slicerator==1.1.0", "sortedcontainers==2.4.0", "spatial_image==1.1.0", 
# "tblib==3.0.0", "threadpoolctl==3.5.0", "tifffile==2025.1.10", 
# "toolz==1.0.0", "tornado==6.4.2", "tqdm==4.67.1", 
# "typing_extensions==4.12.2", "urllib3==2.3.0", "wrapt==1.17.2", 
# "xarray==2024.11.0", "xarray_dataclasses==1.9.1", "xarray_schema==0.0.3", 
# "zarr==2.18.4", "zict==3.0.0")
allp = c("zarr==3.1.5", "spatialdata==0.7.0", "spatialdata_io==0.6.0", 
         "spatialdata_plot==0.2.14", "setuptools==75.8.0")
# notes from VJC -- readSpatialData was modified below so
# that if anndataR = FALSE, spatialdata.read_zarr is used
# to get the whole zarr store, and then the tables are
# transformed via zellkonverter.  this gives a 10x speedup
# for ingesting the visium_hd_3.0.0 example but fails on
# the blobs dataset in example("table-utils") because
# of matters related to metadata/hasTable behavior
#

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
#'   to use \code{anndataR} to read tables; 
#'   defaults to FALSE in `readSpatialData`, and `readTable`,
#'   so that pythonic \code{spatialdata} and \code{zellkonverter} are used.
#' @param ... option arguments passed to and from other methods.
#'
#' @return 
#' \itemize{
#' \item{For \code{readSpatialData}, a \code{SpatialData}.},
#' \item{For element readers, a \code{ImageArray}, \code{LabelArray}, 
#' \code{PointFrame}, \code{ShapeFrame}, or \code{SingleCellExperiment}.}}
#'
#' @examples
#' library(SpatialData.data)
#' dir.create(tf <- tempfile())
#' base <- SpatialData.data:::.unzip_merfish_demo(tf)
#' (x <- readSpatialData(base, anndataR=TRUE))
NULL

readsdlayer <- function(x, ...) {
  md <- fromJSON(file.path(x, ".zattrs"))
  ps <- .get_multiscales_dataset_paths(md)
  list(array = lapply(ps, \(.) ZarrArray(file.path(x, as.character(.)))), 
       md = md)
}

#' @rdname readSpatialData
#' @importFrom Rarr ZarrArray
#' @importFrom jsonlite fromJSON
#' @export
readImage <- function(x, ...) {
    lyrs <- readsdlayer(x, ...)
    ImageArray(data=lyrs$array, meta=Zattrs(lyrs$md), ...)
}

#' @rdname readSpatialData
#' @importFrom Rarr ZarrArray
#' @importFrom jsonlite fromJSON
#' @export
readLabel <- function(x, ...) {
  lyrs <- readsdlayer(x, ...)
  LabelArray(data=lyrs$array, meta=Zattrs(lyrs$md), ...)
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
#' @import geoarrow   
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
    packages=c( "python==3.13.0"),
    pip=allp)

#' @importFrom reticulate import
#' @importFrom S4Vectors metadata
#' @importFrom zellkonverter AnnData2SCE
#' @importFrom SingleCellExperiment int_metadata
#' @importFrom basilisk basiliskStart basiliskStop basiliskRun
.readTables_basilisk <- function(x) {
    proc <- basiliskStart(.env)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, x=x, \(x) {
        # read in 'SpatialData' from .zarr store
        sd <- import("spatialdata")
        zs <- sd$read_zarr(x)
        # return (named) list of SCEs
        names(ts) <- ts <- names(zs$tables$data)
        lapply(ts, \(z) {
            se <- AnnData2SCE(zs$tables[z])
            nm <- "spatialdata_attrs"
            md <- metadata(se)[[nm]]
            int_metadata(se)[[nm]] <- md
            metadata(se)[[nm]] <- NULL
            se
        }) 
    })
}

.readTable_anndataR <- function(x) {
    if (!requireNamespace('anndataR', quietly=TRUE)) {
        stop("To use this function, install the 'anndataR' package via\n",
            "`BiocManager::install(\"keller-mark/anndataR\", ref=\"keller-mark/zarr\")`")
    }
    suppressWarnings({ # suppress warnings related to hidden files
        anndataR::read_zarr(x, as="SingleCellExperiment")
    })
}

#' @rdname readSpatialData
#' @importFrom jsonlite fromJSON
#' @importFrom S4Vectors metadata metadata<-
#' @importFrom SummarizedExperiment colData colData<- 
#' @importFrom SingleCellExperiment 
#'   int_metadata int_metadata<- 
#'   int_colData int_colData<-
#' @export
readTable <- function(x) {
    sce <- .readTable_anndataR(x)
    # move these to 'int_metadata'
    nm <- "spatialdata_attrs"
    md <- metadata(sce)[[nm]]
    int_metadata(sce)[[nm]] <- md
    metadata(sce)[[nm]] <- NULL
    # move these to 'int_colData'
    md <- unlist(md)
    cd <- colData(sce)
    icd <- int_colData(sce)
    . <- match(md, names(cd), nomatch=0)
    int_colData(sce) <- cbind(icd, cd[.])
    colData(sce) <- cd[-.]
    return(sce)
}

#' @rdname readSpatialData
#' @export
readSpatialData <- function(x, 
    images=TRUE, labels=TRUE, points=TRUE, 
    shapes=TRUE, tables=TRUE, anndataR=FALSE) {
    if (!anndataR) tables <- FALSE # will do manually below
    args <- as.list(environment())[.LAYERS]
    skip <- vapply(args, isFALSE, logical(1))
    sd <- lapply(.LAYERS[!skip], \(i) {
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
        f <- get(paste0("read", toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)-1)))
        lapply(j, \(.) do.call(f, list(.)))
    }) 
    if (!anndataR) sd$tables <- .readTables_basilisk(x)
    do.call(SpatialData, sd)
}
