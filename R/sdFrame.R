#' @name sdFrame
#' @title The `sdFrame` class
#'
#' @description
#' The \code{PointFrame} and \code{ShapeFrame} classes store 
#' \code{SpatialData} elements from its \code{"points"} and 
#' \code{"shapes"} layers, respectively. These are represented 
#' as \code{duckspatial_df} (\code{data} slot) associated with 
#' .zattrs stored as \code{\link{Zattrs}} (\code{meta} slot); 
#' a list of \code{metadata} stores other arbitrary info.
#'
#' Currently defined methods (here, \code{x} is an \code{sdFrame}):
#' \itemize{
#' \item \code{data/meta(x)} to access underlying \code{Table/Zattrs}
#' \item \code{names(x)} returns the underlying table's column names
#' \item \code{dim(x)} returns the dimensions of \code{data(x)}
#' \item \code{`$`,`[[`} directly access columns of \code{data(x)}
#' \item \code{filter,select} to subset rows/columns à la \code{dplyr}
#' \item \code{as.data.frame} to coerce \code{x} to a \code{data.frame}
#' }
#'
#' @param x an \code{sdFrame}
#' @param data \code{duckspatial_df} for on-disk representation,
#'   or a \code{data.frame} to be converted.
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary
#'   content describing the overall object.
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param i,j indices for subsetting (see \code{?base::Extract}).
#' @param drop ignored.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return an \code{sdFrame}
#'
#' @examples
#' # PointFrame ----
#' library(SpatialData.data)
#' zs <- get_demo_SDdata("merfish")
#' x <- file.path(zs, "points", "single_molecule")
#' (p <- readPoint(x))
#'
#' head(as.data.frame(data(p)))
#' (q <- dplyr::filter(p, cell_type == "VISp_wm"))
#' 
#' # ShapeFrame ----
#' zs <- get_demo_SDdata("merfish")
#'
#' y <- file.path(zs, "shapes", "cells")
#' (s <- readShape(y))
#' plot(sf::st_as_sf(data(s)), cex=0.2)
#'
#' y <- file.path(zs, "shapes", "anatomical")
#' (s <- readShape(y))
#' plot(sf::st_as_sf(data(s)), cex=0.2)
NULL

#' @rdname sdFrame
#' @export
PointFrame <- \(data=NULL, meta=Zattrs(type="frame"), metadata=list(), ik=NULL, fk=NULL, ...) {
    data <- .df_to_sf(data, "POINT")
    # validate geometry type (must be points)
    if (isTRUE(nrow(data) > 0L)) {
        gt <- tryCatch(unique(st_geometry_type(data)), error=\(.) "n/a")
        if (!all(gt == "POINT")) stop(
            "only 'POINT' geometries supported; ",
            "found: ", paste(gt, collapse=", "))
        # always ensure internal data is duckspatial_df
        if (!is(data, "duckspatial_df"))
            data <- as_duckspatial_df(data, crs=NA)
    }
    # update 'spatialdata_attrs' if keys are provided
    za <- as.list(meta)
    if (is.null(za$spatialdata_attrs))
        za$spatialdata_attrs <- list()
    if (!is.null(ik)) {
        stopifnot(ik %in% colnames(data))
        za$spatialdata_attrs$instance_key <- ik
    }
    if (!is.null(fk)) {
        stopifnot(fk %in% colnames(data))
        za$spatialdata_attrs$feature_key <- fk
    }
    # construct S4 object
    x <- .PointFrame(data=data, meta=Zattrs(za), ...)
    metadata(x) <- metadata
    return(x)
}

#' @rdname ShapeFrame
#' @importFrom sf st_geometry_type
#' @importFrom duckspatial as_duckspatial_df
#' @importFrom S4Vectors metadata<-
#' @export
ShapeFrame <- \(data=NULL, meta=Zattrs(type="frame"), metadata=list(), ...) {
    data <- .df_to_sf(data, "POLYGON")
    # always ensure internal data is duckspatial_df
    if (isTRUE(nrow(data) > 0L) && !is(data, "duckspatial_df"))
        data <- as_duckspatial_df(data, crs=NA)
    x <- .ShapeFrame(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}
