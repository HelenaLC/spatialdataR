#' @name sdFrame
#' @title The `sdFrame` class
#' @aliases PointFrame ShapeFrame
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
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' 
#' # points
#' pa <- list.dirs(
#'   file.path(zs, "points"), 
#'   recursive=FALSE, full.names=TRUE)
#' (x <- readPoint(pa))
#' 
#' y <- filter(x,
#'   genes == "gene_b",
#'   instance_id == 7) 
#' head(as.data.frame(y))
#' 
#' # shapes
#' pa <- list.dirs(
#'   file.path(zs, "shapes"), 
#'   recursive=FALSE, full.names=TRUE)
#' 
#' # circles
#' (x <- readShape(pa[1]))
#' length(x)
#' x$radius
#' 
#' # polygons
#' (y <- readShape(pa[2]))
#' df <- as.data.frame(y)
#' plot(df, col=seq(nrow(df)))
#' 
#' # multi-polygons
#' (z <- readShape(pa[3]))
#' df <- as.data.frame(z)
#' plot(df, col=seq(nrow(df)))
NULL

# get ----

#' @exportMethod [[
#' @rdname sdFrame
#' @importFrom dplyr pull
setMethod("[[", "sdFrame", \(x, i, ...) pull(data(x), i))

#' @export
#' @importFrom utils .DollarNames
.DollarNames.PointFrame <- \(x, pattern="") grepv(pattern, names(x))

#' @exportMethod $
#' @rdname sdFrame
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
#' @rdname sdFrame
#' @importFrom dplyr tally pull
setMethod("length", "sdFrame", \(x) { 
    n <- NULL # R CMD check
    suppressWarnings(dplyr::pull(dplyr::tally(data(x)), n))
})

#' @export
#' @rdname sdFrame
setMethod("dim", "sdFrame", \(x) c(length(x), ncol(data(x))))

#' @export
#' @rdname sdFrame
setMethod("names", "sdFrame", \(x) colnames(data(x)))

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
select.sdFrame <- \(.data, ...) `data<-`(.data, value=select(data(.data), ...))

#' @export
#' @rdname sdFrame
#' @importFrom dplyr mutate
mutate.sdFrame <- \(.data, ...) `data<-`(.data, value=mutate(data(.data), ...))

#' @export
#' @rdname sdFrame
#' @importFrom dplyr filter
filter.sdFrame <- \(.data, ...) `data<-`(.data, value=filter(data(.data), ...))

#' @export
#' @rdname sdFrame
#' @importFrom sf st_as_sf st_geometry_type
#' @importFrom dplyr slice
setMethod("geom_type", "ShapeFrame", \(x) {
    y <- st_as_sf(head(data(x), 1))
    z <- st_geometry_type(y)
    return(as.character(z))
})

# sub ----

.sub_sdFrame <- \(x, i, j) {
    if (missing(i) || isTRUE(i)) {
        if (missing(j) || isTRUE(j)) return(x)
        data(x) <- dplyr::select(data(x), dplyr::all_of(j))
    } else {
        if (is.numeric(i) && any(i < 0)) 
            stop("negative row-subsetting not supported")
        if (is.logical(i)) i <- seq_len(nrow(x))[i]
        if (is.character(j)) j <- match(j, names(x))
        if (missing(j) || isTRUE(j)) j <- seq_len(ncol(x))
        data(x) <- data(x) |> 
            dplyr::filter(dplyr::row_number() %in% i) |>
            dplyr::select(dplyr::all_of(j))
    }
    return(x)
}

#' @export
#' @rdname sdFrame
setMethod("[", c("sdFrame", "ANY", "ANY"), 
    \(x, i, j, ...) {
        if (missing(i)) i <- TRUE
        if (missing(j)) j <- TRUE
        .sub_sdFrame(x, i, j)
    })

# new ----

#' @importFrom sf st_sf st_sfc st_as_sf st_polygon
.df_to_sf <- \(data, type=c("POINT", "POLYGON")) {
    type <- match.arg(type)
    if (is.null(data) || isTRUE(nrow(data) == 0)) {
        # return empty data.frame with geometry column
        return(data.frame(geometry=I(list())))
    }
    if (is.data.frame(data) && !is(data, "sf")) {
        nms <- names(data)
        if (type == "POLYGON" && all(c("x", "y", "i") %in% nms)) {
            # create polygons from vertices
            fn <- \(df) 0.0 + as.matrix(df[, c("x", "y")])
            mx <- lapply(split(data, data$i), fn)
            data <- lapply(mx, \(x) st_polygon(list(x)))
            data <- st_sf(geometry=st_sfc(data))
            rownames(data) <- names(mx)
        } else if (all(c("x", "y") %in% nms)) {
            # create points from coordinates
            data <- st_as_sf(data, coords=c("x", "y"), crs=NA)
        }
    }
    return(data)
}

#' @export
#' @rdname sdFrame
#' @importFrom sf st_geometry_type
#' @importFrom S4Vectors metadata<-
#' @importFrom duckspatial as_duckspatial_df
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

#' @export
#' @rdname sdFrame
#' @importFrom S4Vectors metadata<-
#' @importFrom duckspatial as_duckspatial_df
ShapeFrame <- \(data=NULL, meta=Zattrs(type="frame"), metadata=list(), ...) {
    data <- .df_to_sf(data, "POLYGON")
    # always ensure internal data is duckspatial_df
    if (isTRUE(nrow(data) > 0L) && !is(data, "duckspatial_df"))
        data <- as_duckspatial_df(data, crs=NA)
    x <- .ShapeFrame(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}
