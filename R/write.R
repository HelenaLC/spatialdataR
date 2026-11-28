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
#' @importFrom methods as is
#' @importFrom SummarizedExperiment assay assay<- assayNames colData colData<-
#' @importFrom SingleCellExperiment int_colData int_metadata
writeTable <- function(x, i = NULL) {
    sce <- table(x, i)
    pa <- path(sce)
    if (is.na(pa)) stop("table has no 'source_path' to write to")
    # move 'int_colData' columns back to 'colData' (reverse of 'readTable')
    nm <- "spatialdata_attrs"
    md <- int_metadata(sce)[[nm]]
    icd <- int_colData(sce)
    cd <- cbind(icd[names(icd) %in% unlist(md)], colData(sce))
    rownames(cd) <- colnames(sce)
    colData(sce) <- cd
    # 'anndataR' can't write 'DelayedArray's, so realize (backed) assays
    for (a in assayNames(sce)) {
        y <- assay(sce, a)
        if (!is(y, "DelayedArray")) next
        sparse <- DelayedArray::is_sparse(y)
        assay(sce, a) <- if (sparse) as(y, "dgCMatrix") else as.matrix(y)
    }
    # write to a sibling directory first, so that
    # the source is left untouched if writing fails
    tmp <- tempfile(paste0(".", basename(pa)), tmpdir=dirname(pa))
    fmt <- if (file.exists(file.path(pa, "zarr.json"))) 3L else 2L
    xm <- if ("X" %in% assayNames(sce)) "X"
    tryCatch({
        anndataR::write_zarr(sce, path=tmp, mode="w",
            x_mapping=xm, zarr_format=fmt)
        # write 'int_metadata' attributes back to the table's root
        # (reverse of 'readTable'); 'overwrite=FALSE' gives precedence
        # to encoding entries already written by 'anndataR'
        if (length(md)) Rarr::write_zarr_attributes(tmp, md, overwrite=FALSE)
    }, error=\(e) { unlink(tmp, recursive=TRUE); stop(e) })
    unlink(pa, recursive=TRUE)
    if (!file.rename(tmp, pa))
        stop("couldn't move written table from ", tmp, " to ", pa)
    NULL
}