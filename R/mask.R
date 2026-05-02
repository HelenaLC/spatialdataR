#' @name mask
#' @title Aggregate data across layers
#'
#' @description
#' Masking operations serve to aggregate data across layers, e.g.,
#' counting points in shapes, averaging image channels by labels, etc.
#' For added flexibility, these may be carried out directly between elements,
#' or using an input \code{SpatialData} object and specifying element names.
#'
#' @param x \code{\link{SpatialData}} object.
#' @param i,j character string; names of elements to mask,
#'   specifically, \code{i} will be masked by \code{j},
#'   adding a \code{table} for \code{j} in \code{x}.
#' @param k string or scalar integer; specifies target coordinate space
#'   (defaults to first common coordinate space between \code{i} and \code{j})
#' @param how character string; statistic to use for masking.
#' @param name function use to generate the new \code{table}'s name.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return Input \code{SpatialData} object \code{x} with an additional table.
#'
#' @examples
#' library(SingleCellExperiment)
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#'
#' # count points in shapes
#' y <- mask(x, "blobs_points", "blobs_circles")
#' tail(tables(y), 1)
#'
#' # average image channels by labels
#' y <- mask(x, "blobs_image", "blobs_labels")
#' tail(tables(y), 1)
#'
#' library(SpatialData.data)
#' x <- get_demo_SDdata("merfish")
#' x <- readSpatialData(x)
#'
#' # sum table counts by shapes
#' y <- mask(x, "cells", "anatomical")
#' tail(tables(y), 1)
NULL

.check_ij <- \(x, .) stopifnot(length(.) == 1, is.character(.), . %in% unlist(colnames(x)))

#' @rdname mask
#' @importFrom methods as
#' @importFrom SummarizedExperiment assay assay<-
#' @importFrom SingleCellExperiment int_colData int_colData<- int_metadata<-
#' @export
setMethod("mask", c("SpatialData", "ANY", "ANY"), \(x, i, j, k,
    how=NULL, name=\(i, j) sprintf("%s_by_%s", i, j), ...) {
    .check_ij(x, i); .check_ij(x, j)
    ok <- is.character(name) && length(name) == 1 && !name %in% tableNames(x)
    nm <- if (is.function(name)) name(i, j) else if (ok) name else stop(
        "Invalid 'name'; should be a function or a ",
        "character string not yet in 'tableNames(x)'")
    .i <- element(x, i)
    .j <- element(x, j)
    ct <- intersect(CTname(.i), CTname(.j))
    if (!length(ct)) stop(
        "can't mask; found no common ",
        "coordinates between 'i' and 'j'")
    # TODO: let user specify target coordinate system?
    if (missing(k)) {
        k <- 1
    } else {
        if (is.character(k)) {
            k <- match.arg(k, ct)
            k <- match(k, ct)
        } else if (is.numeric(k)) {
            stopifnot(k > 0, k <= length(ct))
        }
    }
    .i <- transform(.i, ct[k])
    .j <- transform(.j, ct[k])
    t <- tryCatch(error=\(.) NULL, getTable(x, i))
    se <- .mask(.i, .j, how=how, table=t, ...)
    ik <- if (is.null(t)) "instance" else instance_key(t)
    md <- list(region=j, region_key="region", instance_key=ik)
    int_metadata(se)$spatialdata_attrs <- md
    assay(se) <- as(assay(se), "dgCMatrix")
    cd <- int_colData(se)
    cd$region <- j
    cd[[ik]] <- colnames(se)
    int_colData(se) <- cd
    `table<-`(x, nm, value=se)
})

setGeneric(".mask", \(i, j, ...) standardGeneric(".mask"))

#' @noRd
#' @importFrom methods as
#' @importFrom Matrix sparseVector
#' @importFrom SummarizedExperiment assayNames<-
#' @importFrom SingleCellExperiment SingleCellExperiment
setMethod(".mask", c("ImageArray", "LabelArray"), \(i, j, how=NULL, ...) {
    if (is.null(how)) { how <- "mean"; message("Missing 'how'; defaulting to 'mean'") }
    stopifnot(dim(i)[-1] == dim(j))
    .j <- as(data(j), "sparseVector")
    .j <- as.vector(.j[ok <- .j > 0])
    mx <- apply(data(i), 1, \(.i) {
        .i <- as(.i, "sparseVector")
        .i <- as.vector(.i[ok])
        tapply(.i, .j, how)
    })
    colnames(mx) <- channels(i)
    se <- SingleCellExperiment(list(t(mx)))
    assayNames(se) <- how
    return(se)
})

#' @noRd
#' @importFrom Matrix sparseMatrix
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom duckspatial ddbs_intersects
#' @importFrom dplyr mutate inner_join join_by select count collect
#' @importFrom rlang .data
setMethod(".mask", c("PointFrame", "ShapeFrame"), \(i, j, how=NULL, ...) {
    if (!is.null(how)) warning("Can only count when masking points; ignoring 'how'")
    ST_Scale <- ST_Buffer <- geometry <- radius <- id_x <- id_y <- NULL # R CMD check
    jdata <- switch(
        geom_type(j), 
        "POINT"=mutate(j@data, geometry=ST_Buffer(geometry, radius)), 
        j@data)
    res <- ddbs_intersects(jdata, i@data) |>
        inner_join(mutate(i@data, id_y=row_number()), by=join_by(id_y)) |>
        select(all_of(c("id_x", feature_key(i)))) |>
        count(id_x, .data[[feature_key(i)]]) |>
        collect() |>
        mutate(key=factor(.data[[feature_key(i)]]))
    nms <- list(
        levels(res$key),
        seq_along(unique(res$id_x))-1)
    ns <- sparseMatrix(
        i=res$key, j=res$id_x,
        x=res$n, dimnames=nms)
    SingleCellExperiment(list(counts=ns))
})

#' @noRd
#' @importFrom methods as
#' @importFrom Matrix sparseMatrix
#' @importFrom SparseArray colSums
#' @importFrom SummarizedExperiment assay
#' @importFrom duckspatial ddbs_intersects
#' @importFrom SingleCellExperiment SingleCellExperiment
setMethod(".mask", c("ShapeFrame", "ShapeFrame"), \(i, j, how=NULL, table=NULL, value=NULL, assay=1, ...) {
    # validity
    if (is.null(table)) stop("Missing 'table'; can't mask shapes without")
    ok <- is.null(value) || (is.character(value) && all(value %in% rownames(table)))
    if (!ok) stop("Invalid 'value'; should be in 'rownames(table(x, i))'")
    if (is.null(how)) { how <- "sum"; message("Missing 'how'; defaulting to 'sum'") }
    if (is.character(how)) how <- match.arg(how, c("sum", "mean", "detected", "prop.detected"))
    # mapping of 'i' to 'j'
    ij <- ddbs_intersects(data(i), data(j), sparse=TRUE)
    is <- pull(ij, id_x)
    js <- pull(ij, id_y)
    na <- setdiff(seq_len(nrow(i)), is)
    # aggregation
    mx <- assay(table, assay)
    if (endsWith(how, "detected")) mx <- mx > 0
    # auxilary matrix to aggregate 'i's by 'j's; 
    # add dummy 'j' for 'i's without any'j's
    my <- sparseMatrix(
        x=1, 
        i=c(na, is), 
        j=c(rep(1, length(na)), 1+js),
        dims=c(ncol(table), 1+nrow(j)))
    mx <- mx %*% my
    ns <- colSums(my > 0) # number of 'i's per 'j'
    if (grepl("mean|prop", how)) mx <- t(t(mx)/ns)
    # wrangling
    mx <- as(mx, "dgCMatrix")
    colnames(mx) <- c("0", instances(j))
    mx <- list(mx); names(mx) <- how
    se <- SingleCellExperiment(mx)
    se$n_instances <- ns
    return(se)
})

#' @noRd
setMethod(".mask", c("ANY", "ANY"), \(i, j, ...)
    stop("'mask'ing between these element types not yet supported"))
