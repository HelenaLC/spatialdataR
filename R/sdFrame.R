#' @name sdFrame
#' @title The `sdFrame` class
#'
#' @description
#' The \code{PointFrame} class stores \code{SpatialData} elements from its
#' \code{"points"} layers. These are represented as \code{\link[arrow]{Table}}
#' (\code{data} slot) associated with .zattrs stored as \code{\link{Zattrs}}
#' (\code{meta} slot); a list of \code{metadata} stores other arbitrary info.
#'
#' Currently defined methods (here, \code{x} is a \code{PointFrame}):
#' \itemize{
#' \item \code{data/meta(x)} to access underlying \code{Table/Zattrs}
#' \item \code{names(x)} returns the underlying table's column names
#' \item \code{dim(x)} returns the dimensions of \code{data(x)}
#' \item \code{`$`,`[[`} directly access columns of \code{data(x)}
#' \item \code{filter,select} to subset rows/columns à la \code{dplyr}
#' \item \code{as.data.frame} to coerce \code{x} to a \code{data.frame}
#' }
#'
#' @param x \code{PointFrame}
#' @param data \code{arrow}-derived table for on-disk,
#'   \code{data.frame} for in-memory representation.
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary
#'   content describing the overall object.
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param i,j indices for subsetting (see \code{?base::Extract}).
#' @param drop ignored.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return \code{PointFrame}
#'
#' @examples
#' library(SpatialData.data)
#' zs <- get_demo_SDdata("merfish")
#' x <- file.path(zs, "points", "single_molecule")
#' (p <- readPoint(x))
#'
#' head(as.data.frame(data(p)))
#' (q <- dplyr::filter(p, cell_type == "VISp_wm"))
#'
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
#' @export
PointFrame <- \(data=data.frame(), meta=Zattrs(), metadata=list(), ...) {
    x <- .PointFrame(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @export
#' @rdname sdFrame
#' @importFrom dplyr tally pull
setMethod("length", "sdFrame", \(x) { 
    n <- NULL # R CMD check
    suppressWarnings(pull(tally(data(x)), n))
})

#' @export
#' @rdname sdFrame
setMethod("dim", "sdFrame", \(x) c(length(x), ncol(data(x))))

#' @export
#' @rdname sdFrame
setMethod("names", "sdFrame", \(x) colnames(data(x)))

# get ----

#' @exportMethod [[
#' @rdname sdFrame
#' @importFrom dplyr pull
setMethod("[[", "sdFrame", \(x, i, ...) pull(data(x), i))

#' @export
#' @importFrom utils .DollarNames
.DollarNames.PointFrame <- \(x, pattern="") grepv(pattern, names(x))

#' @exportMethod $
#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
setMethod("$", "PointFrame", \(x, name) do.call(`[[`, list(x, name)))

#' @export
#' @rdname sdFrame
#' @importFrom utils .DollarNames
.DollarNames.ShapeFrame <- \(x, pattern="") grepv(pattern, names(x))

#' @exportMethod $
#' @rdname sdFrame
setMethod("$", "ShapeFrame", \(x, name) do.call(`[[`, list(x, name)))

# uts ----

#' @export
#' @rdname ShapeFrame
#' @importFrom sf st_as_sf st_geometry_type
#' @importFrom dplyr slice
setMethod("geom_type", "ShapeFrame", \(x) {
    y <- st_as_sf(data(x) |> head(1))
    z <- st_geometry_type(y)
    return(as.character(z))
})

#' @export
#' @rdname sdFrame
#' @importFrom BiocGenerics as.data.frame
setMethod("as.data.frame", "sdFrame", \(x) as.data.frame(data(x)))
setAs(from="sdFrame", to="data.frame", \(from) as.data.frame(from))

#' @export
#' @rdname sdFrame
#' @importFrom dplyr pull
pull.sdFrame <- \(.data, ...) pull(data(.data), ...)

#' @export
#' @rdname sdFrame
#' @importFrom dplyr select
select.sdFrame <- \(.data, ...) { .data@data <- select(data(.data), ...); .data }

#' @export
#' @rdname sdFrame
#' @importFrom dplyr mutate
mutate.sdFrame <- \(.data, ...) { .data@data <- mutate(data(.data), ...); .data }

#' @export
#' @rdname sdFrame
#' @importFrom dplyr filter
filter.sdFrame <- \(.data, ...) { .data@data <- filter(data(.data), ...); .data }

# sub ----

#' @export
#' @rdname ShapeFrame
setMethod("[", c("sdFrame", "missing", "missing"),
    \(x, i, j, ...) x[TRUE, TRUE])

#' @rdname ShapeFrame    
#' @export
setMethod("[", c("sdFrame", "missing", "ANY"),
    \(x, i, j, ...) x[seq_len(nrow(x)), j])

#' @rdname ShapeFrame
#' @export
setMethod("[", c("sdFrame", "ANY", "missing"),
    \(x, i, j, ...) x[i, seq_len(ncol(x))])

#' @export
#' @rdname ShapeFrame
setMethod("[", c("sdFrame", "logical", "ANY"), \(x, i, j, ...) {
    if (isTRUE(i)) return(x[, j])
    if (isFALSE(i)) return(x[0, j])
    stopifnot(length(i) != nrow(x))
    x[seq_len(nrow(x))[i], j]
})

#' @export
#' @rdname ShapeFrame
setMethod("[", c("sdFrame", "ANY", "logical"), \(x, i, j, ...) {
    if (isTRUE(j)) return(x[i, ])
    if (isFALSE(j)) return(x[i, 0])
    stopifnot(length(j) != ncol(x))
    x[i, seq_len(nrow(x))[j]]
})

#' @rdname ShapeFrame
#' @export
setMethod("[", c("sdFrame", "ANY", "character"), \(x, i, j, ...) {
    stopifnot(all(j %in% names(x)))
    x[i, match(j, names(x))]
})

#' @rdname ShapeFrame
#' @importFrom dplyr row_number select all_of 
#' @export
setMethod("[", c("sdFrame", "numeric", "numeric"), \(x, i, j, ...) {
    if (any(i < 0)) stop("negative row-subsetting not supported")
    x@data <- x@data |> 
        filter(row_number() %in% i) |>
        select(all_of(j))
    return(x)
})
