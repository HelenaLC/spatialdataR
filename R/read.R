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
    if (!any(startsWith(x, c("http://", "https://", "s3://")))) {
      # Until we have a complete store interface (https://github.com/Huber-group-EMBL/Rarr/pull/176),
      # only local objects can be fully validated.
      ds <- .validate_multiscales_paths(x, datasets(mdattr))
    } else {
      # For remote objects, we skip validation and assume that the datasets are in the expected location.
      ds <- datasets(mdattr)
    } 
    ds <- paste0(x, ds)
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
    pq <- paste0(x, file.path("points.parquet", "part.0.parquet"))
    md <- read_zarr_attributes(x)
    ax <- unlist(md$axes)
    df <- ddbs_open_dataset(pq, conn=.conn()) |>
        mutate(geometry=sql(sprintf("ST_Point(%s, %s)", ax[1], ax[2]))) |>
        as_duckspatial_df(crs=NA_character_) |>
        select(-all_of(ax))
    SpatialDataPoint(data=df, meta=SpatialDataAttrs(md))
}

#' @rdname readSpatialData
#' @importFrom Rarr read_zarr_attributes
#' @importFrom duckspatial ddbs_open_dataset
#' @export
readShape <- function(x, ...) {
    md <- read_zarr_attributes(x)
    # "shapes.parquet" currently hardcoded in SpatialData.io
    pq <- paste0(x, "shapes.parquet")
    df <- ddbs_open_dataset(pq, conn=.conn(), crs=NA_character_)
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
    
    x <- Rarr:::.normalize_array_path(x)
    store_meta <- Rarr:::.read_consolidated_metadata(x)$metadata

    # We have to treat v2 and v3 separately in the next 3 lines but we unify them again as `store_groups`.
    store_groups_v3 <- store_meta[vapply(store_meta, \(.) !is.null(.$node_type) && .$node_type == "group", logical(1))]
    store_groups_v2 <- store_meta[endsWith(names(store_meta), ".zgroup")]
    names(store_groups_v2) <- dirname(names(store_groups_v2))
    store_groups <- names(c(store_groups_v3, store_groups_v2))
    
    # helper for layer reading
    .readLayer <- \(l) {
        j <- store_groups[startsWith(store_groups, paste0(l, "/"))]
        j <- setNames(
            paste0(x, j, "/", recycle0 = TRUE),
            basename(j)
        )
        
        opt <- args[[l]]
        if (!isTRUE(opt)) {
            if (is.numeric(opt) && opt > (. <- length(j)))
                stop("'", l, "=", opt, "', but only ", ., " elements found")
            if (is.character(opt) && length(. <- setdiff(opt, basename(j))))
                stop("couldn't find ", l, " of name", .)
            j <- j[opt]
        }
        reader <- get(paste0("read", toupper(substr(l, 1, 1)), substr(l, 2, nchar(l)-1)))
        lapply(j, reader)
    }
    
    names(ls) <- ls <- .LAYERS[!skip]
    sd <- lapply(ls, .readLayer)
    do.call(SpatialData, sd)
}
