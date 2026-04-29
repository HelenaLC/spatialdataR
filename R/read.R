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
#'   so that pythonic \code{anndata} are used.
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
#' zs <- get_demo_SDdata("merfish")
#'
#' # read complete Zarr store
#' (sd <- readSpatialData(zs))
#'
#' # helper that gets path to first element in layer 'l'
#' fn <- \(l) list.files(file.path(zs, l), full.names=TRUE)[1]
#'
#' # read individual element
#' readImage(fn("images"))
#' readShape(fn("shapes"))
#' readPoint(fn("points"))
NULL

#' @importFrom Rarr read_zarr_attributes
#' @importFrom ZarrArray ZarrArray
.readArray <- function(x, ...) {
    md <- read_zarr_attributes(x)
    ps <- .get_multiscales_dataset_paths(md)
    ps <- file.path(x, as.character(ps))
    as <- lapply(ps, ZarrArray)
    list(array=as, md=md)
}

#' @rdname readSpatialData
#' @export
readImage <- function(x, ...) {
    l <- .readArray(x, ...)
    ImageArray(data=l$array, meta=Zattrs(l$md), ...)
}

#' @rdname readSpatialData
#' @export
readLabel <- function(x, ...) {
    l <- .readArray(x, ...)
    LabelArray(data=l$array, meta=Zattrs(l$md), ...)
}

#' @rdname readSpatialData
#' @importFrom duckspatial ddbs_open_dataset as_duckspatial_df
#' @importFrom Rarr read_zarr_attributes
#' @importFrom dplyr sql
#' @export
readPoint <- function(x, ...) {
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    md <- read_zarr_attributes(x)
    ax <- unlist(md$axes)
    df <- ddbs_open_dataset(pq) |>
        mutate(geometry=sql(sprintf("ST_Point(%s, %s)", ax[1], ax[2]))) |>
        as_duckspatial_df(crs=NA_character_) |>
        select(-all_of(ax))
    PointFrame(data=df, meta=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom Rarr read_zarr_attributes
#' @importFrom duckspatial ddbs_open_dataset
#' @import geoarrow
#' @export
readShape <- function(x, ...) {
    md <- read_zarr_attributes(x)
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    ShapeFrame(data=ddbs_open_dataset(pq), meta=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom S4Vectors metadata metadata<-
#' @importFrom SummarizedExperiment colData colData<-
#' @importFrom SingleCellExperiment
#'   int_metadata int_metadata<-
#'   int_colData int_colData<-
#' @importFrom anndataR read_zarr
#' @export
readTable <- function(x) {
    suppressWarnings({ # suppress warnings related to hidden files
      sce <- anndataR::read_zarr(x, as="SingleCellExperiment")
    })
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
    shapes=TRUE, tables=TRUE) {
    args <- as.list(environment())[.LAYERS]
    skip <- vapply(args, isFALSE, logical(1))
    
    # helper for layer reading
    .readLayer <- \(l) {
        j <- list.dirs(file.path(x, l), recursive=FALSE, full.names=TRUE)
        names(j) <- basename(j)
        opt <- args[[l]]
        if (!isTRUE(opt)) {
            if (is.numeric(opt) && opt > (. <- length(j)))
                stop("'", l, "=", opt, "', but only ", ., " elements found")
            if (is.character(opt) && length(. <- setdiff(opt, basename(j))))
                stop("couldn't find ", l, " of name", .)
            j <- j[opt]
        }
        f <- get(paste0("read", toupper(substr(l, 1, 1)), substr(l, 2, nchar(l)-1)))
        lapply(j, \(.) do.call(f, list(.)))
    }
    
    sd <- lapply(setNames(nm=.LAYERS[!skip]), .readLayer)
    do.call(SpatialData, sd)
}
