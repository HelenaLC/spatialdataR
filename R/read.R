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
#' @param ... option arguments passed to and from other methods.
#'
#' @return
#' \itemize{
#' \item{For \code{readSpatialData}, a \code{SpatialData}.},
#' \item{For element readers, 
#' a \code{SpatialDataImage/Label/Point/Shape} 
#' or \code{SingleCellExperiment}.}}
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="spatialdataR")
#'
#' # read complete Zarr store
#' (sd <- readSpatialData(zs))
#'
#' # helper that gets path to last element in layer 'l'
#' fn <- \(.) tail(list.files(file.path(zs, .), full.names=TRUE), 1)
#'
#' # read individual elements
#' (i <- readImage(fn("images")))
#' channels(i)
#' 
#' (p <- readPoint(fn("points")))
#' as.data.frame(head(p))
#' 
#' (s <- readShape(fn("shapes")))
#' data(s)
NULL

#' @importFrom Rarr read_zarr_attributes
#' @importFrom ZarrArray ZarrArray
.readArray <- function(x, ...) {
    md <- read_zarr_attributes(x)
    mdattr <- SpatialDataAttrs(md)
    # TODO: paths to datasets have to be validated properly in the future
    # https://ngff.openmicroscopy.org/specifications/0.5/index.html#images
    # The name of the array is arbitrary with the ordering defined by
    # by the "multiscales" metadata, but is often a sequence starting at 0.
    ds <- .validate_multiscales_paths(x, datasets(mdattr))
    ds <- file.path(x, as.character(ds))
    as <- lapply(ds, ZarrArray)
    list(array=as, mdattr=mdattr)
}

#' @rdname readSpatialData
#' @export
readImage <- function(x, ...) {
    l <- .readArray(x, ...)
    SpatialDataImage(data=l$array, meta=l$mdattr, ...)
}

#' @rdname readSpatialData
#' @export
readLabel <- function(x, ...) {
    l <- .readArray(x, ...)
    SpatialDataLabel(data=l$array, meta=l$mdattr, ...)
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
    df <- ddbs_open_dataset(pq, conn=.conn()) |>
        mutate(geometry=sql(sprintf("ST_Point(%s, %s)", ax[1], ax[2]))) |>
        as_duckspatial_df(crs=NA_character_) |>
        select(-all_of(ax))
    attr(df, "source_path") <- pq
    SpatialDataPoint(data=df, meta=SpatialDataAttrs(md))
}

#' @rdname readSpatialData
#' @importFrom Rarr read_zarr_attributes
#' @importFrom duckspatial ddbs_open_dataset
#' @export
readShape <- function(x, ...) {
    md <- read_zarr_attributes(x)
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    df <- ddbs_open_dataset(pq, conn=.conn(), crs=NA_character_)
    attr(df, "source_path") <- pq
    SpatialDataShape(data=df, meta=SpatialDataAttrs(md))
}

#' @export
#' @rdname readSpatialData
#' @importFrom anndataR read_zarr
#' @importFrom S4Vectors metadata metadata<-
#' @importFrom SummarizedExperiment colData colData<-
#' @importFrom SingleCellExperiment int_colData int_colData<- int_metadata int_metadata<-
readTable <- function(x) {
    suppressWarnings({ # suppress warnings related to hidden files
        sce <- anndataR::read_zarr(x, as="SingleCellExperiment")
    })
    # move these to 'int_metadata'
    nm <- "spatialdata_attrs"
    md <- metadata(sce)[[nm]]
    int_metadata(sce)[[nm]] <- md
    int_metadata(sce)$source_path <- x
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
        f <- paste0("read", toupper(substr(l, 1, 1)), substr(l, 2, nchar(l)-1))
        lapply(j, f)
    }
    
    names(ls) <- ls <- .LAYERS[!skip]
    sd <- lapply(ls, .readLayer)
    do.call(SpatialData, sd)
}
