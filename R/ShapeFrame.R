#' @name ShapeFrame
#' @title The `ShapeFrame` class
#' @aliases geom_type
#'
#' @param x \code{ShapeFrame}
#' @param data \code{duckspatial_df} for on-disk representation,
#'   a 3-column \code{data.frame} (with columns \code{x}, \code{y} and
#'   \code{id}) with vertices of polygons, or any object that can be passed
#'   to \code{\link[duckspatial]{as_duckspatial_df}}.
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary
#'   content describing the overall object.
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param i,j indices specifying elements to extract.
#' @param drop,pattern ignored.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return \code{ShapeFrame}
#'
#' @examples
#' library(SpatialData.data)
#' zs <- get_demo_SDdata("merfish")
#'
#' y <- file.path(zs, "shapes", "cells")
#' (s <- readShape(y))
#' plot(sf::st_as_sf(data(s)), cex=0.2)
#'
#' y <- file.path(zs, "shapes", "anatomical")
#' (s <- readShape(y))
#' plot(sf::st_as_sf(data(s)), cex=0.2)
#'
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
#' @importFrom duckspatial as_duckspatial_df
#' @export
ShapeFrame <- function(data=data.frame(), meta=Zattrs(), metadata=list(), ...) {
    if (is.data.frame(data)) {
        if (ncol(data) == 3L &&
            all(c("x", "y", "id") %in% colnames(data))) {
            # create sf polygons from vertices
            mxL <- lapply(split(data, data$id), function(df) {
                as.matrix(df[, c("x", "y")]) + 0.0
            })
            data <- st_sf(geometry = st_sfc(lapply(mxL, function(x) st_polygon(list(x)))))
            rownames(data) <- names(mxL)
            data <- as_duckspatial_df(data)
        } else if (nrow(data) > 0L) {
            data <- as_duckspatial_df(data)
        }
    } else {
        data <- as_duckspatial_df(data)
    }
    x <- .ShapeFrame(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

# TODO: it's really annoying that this doesn't just inherit
# data.frame() operations, cuz data are in an extra slot...
# but else not sure how to assure validity, stash .zattrs etc.

#' @rdname ShapeFrame
#' @export
setMethod("dim", "ShapeFrame", \(x) c(length(x),
                                      ncol(data(x))))

#' @rdname ShapeFrame
#' @export
#' @importFrom dplyr tally pull
setMethod("length", "ShapeFrame", \(x) {
    # suppress warning caused by the 'geometry' column being dropped
    # duckspatial::ddbs_drop_geometry() is an alternative, but fails if
    # 'geometry' is the only column
    suppressWarnings({
        data(x) |> tally() |> pull(n)
    })
})

#' @rdname ShapeFrame
#' @export
setMethod("names", "ShapeFrame", \(x) colnames(data(x)))

#' @rdname ShapeFrame
#' @importFrom dplyr collect select
#' @exportMethod [[
setMethod("[[", "ShapeFrame", \(x, i, ...) {
    collect(select(data(x), all_of(i)))[[1]]
})

#' @export
#' @rdname ShapeFrame
#' @importFrom utils .DollarNames
.DollarNames.ShapeFrame <- \(x, pattern="")
    grep(pattern, names(x), value=TRUE)

#' @rdname ShapeFrame
#' @exportMethod $
setMethod("$", "ShapeFrame", \(x, name) do.call(`[[`, list(x, name)))

#' @export
#' @rdname ShapeFrame
#' @importFrom sf st_as_sf st_geometry_type
#' @importFrom dplyr slice
setMethod("geom_type", "ShapeFrame", \(x) {
    y <- st_as_sf(data(x) |> head(1))
    z <- st_geometry_type(y)
    return(as.character(z))
})

# sub ----

#' @rdname ShapeFrame
#' @export
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

