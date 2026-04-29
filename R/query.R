#' @name query
#' @title queries
#'
#' @description \code{query} provides a interface for table-based 
#' subsetting of \code{SpatialData} objects. It filters a specified 
#' table using \code{dplyr::filter} logic and propagates the result 
#' to all associated spatial elements (i.e., only instances 
#' present in the filtered table are kept).
#' 
#' For spatial cropping, see \code{\link{crop}}.
#'
#' @param x \code{SpatialData} object.
#' @param i index or name of table to query.
#' @param ... logic passed to \code{dplyr::filter}.
#'
#' @return \code{SpatialData} object
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' sd <- readSpatialData(zs, anndataR=TRUE)
#' 
#' # filter by 'region' and propagate to shapes/points
#' t <- table(sd)
#' query(sd, i=1, region == region(t))
NULL

#' @export
#' @rdname query
#' @importFrom dplyr filter pull
#' @importFrom SummarizedExperiment colData
#' @importFrom SingleCellExperiment int_colData
setMethod("query", "SpatialData", \(x, i, ...) {
    if (!length(tables(x)))
        stop("There aren't any tables")
    if (is.numeric(i)) {
        i <- tableNames(x)[i]
    } else if (is.character(i)) {
        i <- match.arg(i, tableNames(x))
    }
    t <- x$tables[[i]]
    df <- data.frame(.i=seq_len(ncol(t)), colData(t), int_colData(t))
    df <- filter(df, ...)
    if (!nrow(df)) stop("Nothing left after query")
    t <- t[, df$.i]
    colData(t) <- droplevels(colData(t))
    int_colData(t) <- droplevels(int_colData(t))
    region(t) <- levels(regions(t))
    for (l in setdiff(.LAYERS, "tables")) {
        j <- !names(x[[l]]) %in% region(t)
        if (sum(j)) x[[l]] <- x[[l]][-which(j)]
    }
    for (r in region(t)) {
        l <- layer(x, r)
        if (l == "labels") next
        e <- x[[l]][[r]]
        ik <- instance_key(t)
        j <- pull(data(e), ik)
        j <- j %in% instances(t)
        x[[l]][[r]] <- e[which(j), ]
    }
    table(x, i) <- t
    return(x)
})
