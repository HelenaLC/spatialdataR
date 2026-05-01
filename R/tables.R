#' @name table-utils
#' @title \code{SpatialData} annotations
#' @aliases hasTable getTable setTable
#'
#' @param x \code{\link{SpatialData}} object.
#' @param i character string; name of the
#'   element for which to get/set a \code{table}.
#' @param j character string; \code{colData} column,
#'   or row name to retrieve \code{assay} data.
#' @param drop logical; should observations (columns)
#'   that don't belong to \code{i} be filtered out?
#' @param name logical; should the \code{table}
#'   name be returned instead of TRUE/FALSE?
#' @param assay character string or scalar integer;
#'   specifies which \code{assay} to use when \code{j} is a row name.
#' @param rk,ik character string; region and instance key (the latter will be
#'   ignored if an instance key is already specified within element \code{i}).
#' @param y \code{SingleCellExperiment} containing annotations for \code{i}.
#'
#' @returns
#' \itemize{
#' \item \code{hasTable}:
#'   logical scalar (or character string, if \code{name=TRUE});
#'   whether or not a \code{table} annotating \code{i} exists in \code{x}
#' \item \code{getTable}:
#'   \code{SingleCellExperiment}; the \code{table} annotating
#'   \code{i} with optional filtering of matching observations
#' \item \code{valTable}:
#'   vector of values (according to \code{j})
#'   from the \code{table} annotating \code{i}
#' }
#'
#' @examples
#' library(SingleCellExperiment)
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x)
#'
#' # check if element has a 'table'
#' hasTable(x, "blobs_points")
#' hasTable(x, "blobs_labels")
#'
#' # retrieve 'table' for element 'i'
#' sce <- getTable(x, i="blobs_labels")
#' head(colData(sce))
#' meta(sce)
#'
#' # get values from 'table'
#' getTable(x,
#'   i="blobs_labels",
#'   j="channel_0_sum")
#'
#' # add 'table' annotating an element 'i'
#'
#' # labels
#' y <- x; tables(y) <- list()
#' mtx <- matrix(0, 1, length(instances(label(y))))
#' sce <- SingleCellExperiment(list(counts=mtx))
#' y <- setTable(y, i <- "blobs_labels", sce)
#' getTable(y, i)
#'
#' # shapes
#' i <- "blobs_circles"
#' mtx <- matrix(0, 1, nrow(shape(x, i)))
#' sce <- SingleCellExperiment(list(counts=mtx))
#' y <- setTable(x, i, sce)
#' getTable(y, i)
NULL

#' @rdname table-utils
#' @export
setMethod("meta", c("SingleCellExperiment"),
    \(x) int_metadata(x)$spatialdata_attrs)

.invalid_i <- \() stop(
    "invalid 'i'; should be a character ",
    "string specifying an element in 'x'")

# has ----

#' @rdname table-utils
#' @export
setMethod("hasTable", c("SpatialData", "ANY"), \(x, i) .invalid_i())

#' @rdname table-utils
#' @export
setMethod("hasTable", c("SpatialData", "character"), \(x, i, name=FALSE) {
    stopifnot(
        isTRUE(name) || isFALSE(name),
        length(i) == 1, is.character(i))
    # check that 'i' is a non-'table' element name
    nms <- colnames(x)
    idx <- setdiff(names(nms), "tables")
    match.arg(i, unlist(nms[idx]))
    # count occurrences
    t <- lapply(tables(x), \(t) meta(t)$region)
    ok <- vapply(t, \(.) i %in% ., logical(1))
    # failure when no/many matches
    if (!name) return(any(ok))
    if (!any(ok)) stop("no 'table' found for 'i'")
    if (sum(ok) > 1) stop("multiple 'table's found for 'i'")
    return(names(t)[ok])
})

# get ----

#' @rdname table-utils
#' @export
setMethod("getTable", c("SpatialData", "ANY"), \(x, i, j, assay=1, drop=TRUE) .invalid_i())

#' @export
#' @rdname table-utils
#' @importFrom dplyr pull
#' @importFrom SummarizedExperiment assay
#' @importFrom SingleCellExperiment int_colData
setMethod("getTable", c("SpatialData", "character"), \(x, i, j, assay=1, drop=TRUE) {
    stopifnot(isTRUE(drop) || isFALSE(drop))
    # get 'table' annotating 'i', if any
    nm <- hasTable(x, i, name=TRUE) 
    t <- SpatialData::table(x, nm)
    # only keep observations belonging to 'i' (optional)
    if (drop) {
        rk <- region_key(t)
        ik <- instance_key(t)
        cd <- int_colData(t)
        cd <- if (rk %in% names(cd)) cd[[rk]] else t[[rk]]
        t <- t[, cd == i]
        l <- names(which(vapply(colnames(x), \(.) i %in% ., logical(1))))
        y <- x[[l]][[i]]
        i <- if (is(y, "LabelArray")) {
            instances(y)
        } else if (is(y, "ShapeFrame")) {
            if (ik %in% names(y)) pull(y, !!ik) else seq(0, length(y)-1)
        } else stop ("Only labels and shapes can have tables.")
        t <- t[, instances(t) %in% i]
    }
    if (missing(j)) return(t)
    rs <- j %in% rownames(t)
    cd <- j %in% names(colData(t))
    if (!(rs || cd)) stop("invalid 'j'")
    if (cd) return(t[[j]])
    assay(t, assay)[j, ]
})

# set ----

#' @rdname table-utils
#' @export
setMethod("setTable", c("SpatialData", "ANY"), \(x, i, ..., name=NULL, rk="rk", ik="ik") .invalid_i())

# it seems pull below dispatches to arrow, and a warning on as_vector was being produced
#' @rdname table-utils
#' @importFrom methods as
#' @importFrom dplyr pull
#' @importFrom sf st_as_sf
#' @importFrom S4Vectors make_zero_col_DFrame
#' @importFrom SingleCellExperiment SingleCellExperiment
#'   int_colData int_colData<- int_metadata<-
#' @export
setMethod("setTable", c("SpatialData", "character"), \(x, i, y,
    name=NULL, rk="region", ik="instance_id") {
    
    # validity
    stopifnot(
        is(y, "SingleCellExperiment"),
        length(i) == 1, is.character(i),
        length(rk) == 1, is.character(rk),
        length(ik) == 1, is.character(ik))
    if (!i %in% unlist(colnames(x)))
        stop(dQuote(i), " is not an element of 'x'")
    if (is.null(name)) {
        # make up 'name' if not provided
        name <- paste0(i, "_table")
    } else {
        stopifnot(is.character(name), length(name) == 1)
        if (name %in% tableNames(x))
            stop("'table' with name ", dQuote(name),
                " exists; use 'table<-' to replace it.")
    }
    . <- layer(x, i)
    if (!. %in% c("labels", "shapes"))
        stop("can't add 'table' for", .)
    
    if (is.null(region_key(y))) region_key(y) <- rk
    if (is.null(instance_key(y))) instance_key(y) <- ik
    
    if (is.null(region(y))) {
        regions(y) <- i
    } else {
        stopifnot(region(y) == i)
    }
    
    e <- element(x, i)
    n <- length(instances(e))
    if (ncol(y) != n) stop(
        "'instances<-' have not been set on 'y'; ",
        "'ncol(y)' must match 'nrow(element(x, i))'")
    instances(y) <- instances(e)
    SpatialData::`table<-`(x, i=name, value=y)
})
