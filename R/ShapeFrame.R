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
#' @importFrom duckspatial as_duckspatial_df
#' @importFrom sf st_sf st_sfc st_polygon
#' @importFrom S4Vectors metadata<-
#' @importFrom dplyr filter select
#' @export
ShapeFrame <- \(
    data=NULL, 
    meta=Zattrs(type="frame"), 
    metadata=list(), ...) {
    if (is.null(data)) {
        # mock geometry for empty data
        geom <- st_sfc(st_polygon())
        data <- st_sf(data.frame(x=1), geometry=geom)
        data <- as_duckspatial_df(data, crs=NA)
        data <- data |> filter(x == 0) |> select(-x)
    } else if (is.data.frame(data)) {
        if (ncol(data) == 3L && 
            all(c("x", "y", "id") %in% names(data))) {
            # create polygons from vertices
            fn <- \(df) 0.0+as.matrix(df[, c("x", "y")])
            mx <- lapply(split(data, data$id), fn)
            data <- lapply(mx, \(x) st_polygon(list(x)))
            data <- st_sf(geometry=st_sfc(data))
            rownames(data) <- names(mx)
            data <- as_duckspatial_df(data, crs=NA)
        } else if (nrow(data) > 0L) {
            data <- as_duckspatial_df(data, crs=NA)
        }
    } else {
        data <- as_duckspatial_df(data, crs=NA)
    }
    x <- .ShapeFrame(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}
