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
#' x <- system.file(x, package="spatialdataR")
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
#' # TODO: shape,shape example
NULL

.check_ij <- \(x, .) stopifnot(length(.) == 1, is.character(.), . %in% unlist(colnames(x)))

#' @export
#' @rdname mask
#' @importFrom methods as
#' @importFrom SummarizedExperiment assay assay<-
#' @importFrom SingleCellExperiment int_colData int_colData<- int_metadata<-
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
    se <- mask_i_by_j(.i, .j, how=how, table=t, ...)
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

# internal use only!
#' @noRd
setGeneric("mask_i_by_j", \(i, j, ...) standardGeneric("mask_i_by_j"))

#' @noRd
#' @importFrom methods as
#' @importFrom Matrix sparseVector
#' @importFrom SummarizedExperiment assayNames<-
#' @importFrom SingleCellExperiment SingleCellExperiment
setMethod("mask_i_by_j", 
    c("SpatialDataImage", "SpatialDataLabel"), 
    \(i, j, how=NULL, ...) {
    .wh <- \(.) {
        ds <- dim(.); if (length(ds) == 3) ds <- ds[-1]
        metadata(.)$wh %||% list(c(0, ds[2]), c(0, ds[1]))
    }
    stopifnot(
        "image/label width mismatch"=.wh(i)[[1]] == .wh(j)[[1]],
        "image/label height mismatch"=.wh(i)[[2]] == .wh(j)[[2]])
    if (is.null(how)) { 
        message("Missing 'how'; defaulting to 'mean'") 
        how <- "mean"
    }
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

.mask_map <- \(i, j) {
    ST_Buffer <- geometry <- radius <- NULL # R CMD check
    df_j <- switch(
        geom_type(j), 
        "POINT"=mutate(data(j), geometry=ST_Buffer(geometry, radius)), 
        data(j))
    ddbs_intersects(df_j, data(i), sparse=TRUE)
        
}

#' @noRd
#' @importFrom rlang .data
#' @importFrom Matrix sparseMatrix
#' @importFrom SparseArray colSums
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom dplyr mutate left_join coalesce join_by select count collect row_number
setMethod("mask_i_by_j", 
    c("SpatialDataPoint", "SpatialDataShape"), 
    \(i, j, how=NULL, ...) {
    if (!is.null(how)) message("Can only count when masking points; ignoring 'how'")
    id_x <- id_y <- n <- NULL # R CMD check
    ij <- .mask_map(i, j)
    fk <- feature_key(i)
    res <- data(i) |>
        mutate(id_y=row_number()) |>
        left_join(ij, by=join_by(id_y)) |>
        mutate(id_x=coalesce(id_x, 0L)) |>
        select(all_of(c("id_x", fk))) |>
        count(id_x, .data[[fk]]) |>
        collect() |>
        mutate(key=factor(.data[[fk]]))
    ks <- levels(res$key)
    ns <- sparseMatrix(
        x=res$n, 
        i=as.integer(res$key), 
        j=res$id_x + 1,
        dims=c(length(ks), 1 + nrow(j)), 
        dimnames=list(ks, c("0", instances(j))))
    se <- SingleCellExperiment(list(counts=ns))
    se$n_instances <- colSums(ns)
    return(se)
})

#' @noRd
#' @importFrom methods as
#' @importFrom S4Vectors DataFrame
#' @importFrom SparseArray colSums
#' @importFrom Matrix t sparseMatrix
#' @importFrom SummarizedExperiment assay
#' @importFrom duckspatial ddbs_intersects
#' @importFrom SingleCellExperiment SingleCellExperiment
setMethod("mask_i_by_j", 
    c("SpatialDataShape", "SpatialDataShape"), 
    \(i, j, how=NULL, table=NULL, assay=1, ...) {
    # validity
    if (is.null(table)) stop("Missing 'table'; can't mask shapes without")
    if (is.null(how)) { how <- "sum"; message("Missing 'how'; defaulting to 'sum'") }
    if (is.character(how)) how <- match.arg(how, c("sum", "mean", "detected", "prop.detected"))
    # mapping of 'i' to 'j'
    ij <- .mask_map(i, j)
    if (nrow(collect(head(ij, 1))) == 0)
        stop("found no intersections",
            " between shapes 'i' and 'j'")
    is <- pull(ij, .data$id_y) # elements in i
    js <- pull(ij, .data$id_x) # masks in j
    na <- setdiff(seq_along(i), is)
    # aggregation
    mx <- assay(table, assay)
    if (endsWith(how, "detected")) mx <- mx > 0
    # auxiliary matrix to aggregate 'i's by 'j's; 
    # add dummy 'j' for 'i's without any 'j's
    my <- sparseMatrix(
        x=1, 
        i=c(na, is), 
        j=c(rep(1, length(na)), 1+js),
        dims=c(nrow(i), 1+nrow(j)))
    mx <- mx %*% my
    ns <- colSums(my > 0) # number of 'i's per 'j'
    if (grepl("mean|prop", how)) mx <- t(t(mx)/ns)
    # wrangling
    mx <- as(mx, "CsparseMatrix")
    colnames(mx) <- c("0", instances(j))
    mx <- list(mx); names(mx) <- how
    ci <- seq_len(ncol(my))
    ci <- factor(rep(ci, diff(my@p)), levels=ci)
    ri <- split(my@i+1, ci)
    cd <- DataFrame(i_instances=I(ri), n_instances=ns)
    SingleCellExperiment(mx, colData=cd)
})

#' @noRd
setMethod("mask_i_by_j", c("ANY", "ANY"), \(i, j, ...)
    stop("'mask'ing between these element types not supported"))
