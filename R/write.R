#' @name writeSpatialData
#' @title Writing `SpatialData` components
#'
#' @aliases writeTable
#'
#' @param x
#'   For \code{readImage/Label/Point/Shape/Table},
#'   path to a \code{SpatialData} element.
#'   For \code{readSpatialData},
#'   path to a \code{SpatialData}-.zarr store.
#' @param i table elements to write to  
#' @param ... option arguments passed to and from other methods.
#'
#' @return NULL
#'
#' @examples
#' # Write support is only for SpatialData stores associated with Zarr v3
#' zs <- file.path("extdata", "blobs_v3.zarr")
#' zs <- system.file(zs, package="spatialdataR")
#' 
#' # move store to temporary location
#' td <- tempfile()
#' dir.create(td)
#' file.copy(zs, td, recursive=TRUE)
#' zs <- file.path(td, basename(zs))
#' 
#' # read store & write (first) table back to disk
#' sd <- readSpatialData(zs)
#' writeTable(sd, i=1)
#' 
#' # re-read to check the round trip
#' (sd <- readSpatialData(zs))
#' 
#' # clean up
#' unlink(td, recursive=TRUE)
NULL

#' @export
#' @rdname writeSpatialData
#' @importFrom methods as is
#' @importFrom SummarizedExperiment assay assay<- assayNames colData colData<-
#' @importFrom SingleCellExperiment int_colData int_metadata
writeTable <- function(x, i = NULL) {
    # check version
    sce <- table(x, i)
    if(version(sce) !="0.2") 
        stop("Updates can only be performed tables with version 0.2!")
    
    # move 'int_colData' columns back to 'colData' (reverse of 'readTable')
    nm <- "spatialdata_attrs"
    md <- int_metadata(sce)[[nm]]
    icd <- int_colData(sce)
    cd <- cbind(icd[names(icd) %in% unlist(md)], colData(sce))
    rownames(cd) <- colnames(sce)
    colData(sce) <- cd
    
    # TODO: 'anndataR' can't write 'DelayedArray's, so realize (backed) assays
    # until https://github.com/scverse/anndataR/pull/601 is merged.
    sce <- .realize_assays(sce)
    
    # write to a temporary directory first, so that
    # the source is left untouched if writing fails
    pa <- path(sce)
    tmp <- tempfile(fileext = ".zarr")
    xm <- if ("X" %in% assayNames(sce)) "X"
    anndataR::write_zarr(sce, path=tmp, mode="w",
                         x_mapping=xm, zarr_format=3)
    if (length(md)) 
        Rarr::write_zarr_attributes(tmp, md, overwrite=FALSE)
    
    # write to table location
    unlink(pa, recursive=TRUE)
    if (!file.rename(tmp, pa))
        stop("couldn't move written table from ", tmp, " to ", pa)
    invisible(x)
}

#' @noRd
.realize_assays <- function(sce){
    for (a in assayNames(sce)) {
        y <- assay(sce, a)
        if (!is(y, "DelayedArray")) next
        sparse <- DelayedArray::is_sparse(y)
        assay(sce, a) <- if (sparse) as(y, "dgCMatrix") else as.matrix(y)
    }
    sce
}