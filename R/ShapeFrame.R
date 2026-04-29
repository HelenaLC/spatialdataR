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
#' @importFrom sf st_sf st_sfc st_polygon
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
#' @importFrom duckspatial as_duckspatial_df
#' @export
ShapeFrame <- function(data=data.frame(), meta=Zattrs(type="frame"), metadata=list(), ...) {
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
