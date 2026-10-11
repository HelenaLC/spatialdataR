#' @name write_utils
#' @title Writing `SpatialData` components
#'
#' @aliases writeTable
#'
#' @param x
#'   For \code{readImage/Label/Point/Shape/Table},
#'   path to a \code{SpatialData} element.
#'   For \code{readSpatialData},
#'   path to a \code{SpatialData}-.zarr store.
#' @param ... option arguments passed to and from other methods.
#'
#' @return NULL
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

#' @export
#' @rdname write_utils
#' @importFrom anndataR read_zarr
#' @importFrom dplyr select
#' @importFrom S4Vectors metadata metadata<-
#' @importFrom SummarizedExperiment colData colData<-
#' @importFrom SingleCellExperiment int_colData int_colData<- int_metadata int_metadata<-
writeTable <- function(x, i = NULL) {
    sce <- table(x, i)
    suppressWarnings({ # suppress warnings related to hidden files
        anndataR::write_zarr(sce, path = path(sce), mode = "w")
    })
    NULL
}