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
select.sdFrame <- \(.data, ...) { data(.data) <- select(data(.data), ...); .data }

#' @export
#' @rdname sdFrame
#' @importFrom dplyr mutate
mutate.sdFrame <- \(.data, ...) { data(.data) <- mutate(data(.data), ...); .data }

#' @export
#' @rdname sdFrame
#' @importFrom dplyr filter
filter.sdFrame <- \(.data, ...) { data(.data) <- filter(data(.data), ...); .data }

# sub ----

.sub_sdFrame <- \(x, i, j) {
    if (missing(i) || isTRUE(i)) {
        if (missing(j) || isTRUE(j)) return(x)
        data(x) <- select(data(x), all_of(j))
    } else {
        if (is.numeric(i) && any(i < 0)) 
            stop("negative row-subsetting not supported")
        if (is.logical(i)) i <- seq_len(nrow(x))[i]
        if (is.character(j)) j <- match(j, names(x))
        if (missing(j) || isTRUE(j)) j <- seq_len(ncol(x))
        data(x) <- data(x) |> 
            filter(row_number() %in% i) |>
            select(all_of(j))
    }
    return(x)
}

#' @export
#' @rdname ShapeFrame
setMethod("[", c("sdFrame", "ANY", "ANY"), 
    \(x, i, j, ...) {
        if (missing(i)) i <- TRUE
        if (missing(j)) j <- TRUE
        .sub_sdFrame(x, i, j)
    })
