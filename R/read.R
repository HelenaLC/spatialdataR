#' @name readSpatialData
#' @title Reading `SpatialData`
#' 
#' @aliases readImage readLabel readPoint readShape readTable
#' 
#' @param x
#'   For \code{readSpatialData},
#'   path to a \code{SpatialData}.zarr store.
#'   For \code{readImage/Label/Point/Shape/Table}, 
#'   path to a \code{SpatialData} element.
#' @param images,labels,points,shapes,tables
#'   control which layers and elements to read;
#'   FALSE to skip a layer, integer or character 
#'   vector to read some element(s).
#' 
#' @return \code{\link{SpatialData}} object, or one of its elements.
#' 
#' @examples
#' # package demo data
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' (sd <- readSpatialData(pa))
#' 
#' # view data available on OSN, 
#' # then retrieve & ingest one
#' Sys.setenv(AWS_REGION="us-east-1")
#' if (requireNamespace("paws")) sd_osn_list()
#' (sd <- sd_osn_load("merfish"))
NULL

#' @importFrom S4Vectors isSequence
.get_multiscales_datasets_path <- \(md) {
    .validate_multiscales_datasets_path(md)
    ps <- md$multiscales[[1]]$datasets[[1]]$path
    ps <- suppressWarnings(as.numeric(ps))
    ps <- sort(ps, decreasing=FALSE)
    if (!all(ps == round(ps)) && 
        isSequence(as.integer(ps)))
        stop("ImageArray paths are ill-defined; should be",
            " an integer sequence, i.e., 0, 1, 2, ..., n.")
    return(ps)
}

.validate_multiscales_datasets_path <- \(md) {
    ms <- md$multiscales
    if (!is.null(ms)) {
        ds <- ms[[1]]$datasets
        if (!is.null(ds)) {
            ex <- vapply(ds, \(.) !is.null(.$path), logical(1))
            if (!all(ex)) {
                stop("'ImageArray' paths are ill-defined;",
                    " no 'path' attribute under 'multiscales-datasets'.")
            }

        } else {
            stop("'ImageArray' paths are ill-defined;",
                " no 'datasets' attribute under 'multiscales'.")
        }
    } else {
        stop("'ImageArray' paths are ill-defined;",
            " no 'multiscales' attribute under '.zattrs'.")
    }
}

#' @importFrom Rarr read_zarr_attributes
.read_za <- \(x) {
    md <- read_zarr_attributes(x)
    ps <- .get_multiscales_datasets_path(md)
    as <- lapply(ps, \(.) ZarrArray(file.path(x, .)))
    list(array=as, md=md)
}

#' @rdname readSpatialData
#' @importFrom Rarr ZarrArray
#' @export
readImage <- \(x) {
    za <- .read_za(x)
    ImageArray(data=za$array, zattrs=Zattrs(za$md))
}

#' @rdname readSpatialData
#' @importFrom Rarr ZarrArray
#' @export
readLabel <- \(x) {
    za <- .read_za(x)
    LabelArray(data=za$array, zattrs=Zattrs(za$md))
}

#' @rdname readSpatialData
#' @importFrom arrow open_dataset
#' @importFrom Rarr read_zarr_attributes
#' @export
readPoint <- \(x) {
    md <- read_zarr_attributes(x)
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    PointFrame(data=open_dataset(pq), zattrs=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom arrow open_dataset
#' @importFrom Rarr read_zarr_attributes
#' @import geoarrow   
#' @export
readShape <- \(x) {
    requireNamespace("geoarrow", quietly=TRUE)
    md <- read_zarr_attributes(x)
    # TODO: previously had read_parquet(), 
    # but that doesn't work with geoparquet?
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    ShapeFrame(data=open_dataset(pq), zattrs=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom anndataR read_zarr
#' @importFrom Rarr read_zarr_attributes
#' @importFrom S4Vectors metadata metadata<-
#' @importFrom SingleCellExperiment int_metadata int_metadata<-
#' @export
readTable <- \(x) {
    # suppress warnings related to hidden files
    za <- read_zarr_attributes(x)
    suppressWarnings(x <- read_zarr(x, as="SingleCellExperiment"))
    # move .zattrs to 'int_metadata'
    md <- int_metadata(x)
    md$zattrs <- za
    nm <- "spatialdata_attrs"
    md[[nm]] <- metadata(x)[[nm]]
    int_metadata(x) <- md
    metadata(x) <- list()
    # # move these to 'int_colData'
    # md <- unlist(md)
    # cd <- colData(x)
    # icd <- int_colData(x)
    # . <- match(md, names(cd), nomatch=0)
    # int_colData(x) <- cbind(icd, cd[.])
    # colData(x) <- cd[-.]
    return(x)
}

#' @rdname readSpatialData
#' @export
readSpatialData <- \(x, 
    images=TRUE, labels=TRUE, points=TRUE, shapes=TRUE, tables=TRUE)
{
    #if (!anndataR) tables <- FALSE # will do manually below
    args <- as.list(environment())[.LAYERS]
    skip <- vapply(args, isFALSE, logical(1))
    sd <- lapply(.LAYERS[!skip], \(i) {
        y <- file.path(x, i)
        j <- list.dirs(y, recursive=FALSE)
        names(j) <- basename(j)
        if (!isTRUE(opt <- args[[i]])) {
            if (is.numeric(opt) && any(opt > (. <- length(j)))) 
                stop("'", i, "=", opt, "', but found only ", .)
            if (is.character(opt) && length(. <- setdiff(opt, basename(j))))
                stop("couln't find ", i, " of name", .)
            j <- j[opt]
        }
        f <- get(paste0("read", toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)-1)))
        lapply(j, \(.) do.call(f, list(.)))
    }) 
    #if (!anndataR) sd$tables <- .readTables_basilisk(x)
    do.call(SpatialData, sd)
}
