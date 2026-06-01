#' @name SpatialDataFrame
#' @title \code{SpatialDataFrame}
#' @aliases SpatialDataPoint SpatialDataShape geom_type
#'
#' @description 
#' The \code{SpatialDataPoint} and \code{-Shape} classes represent 
#' elements from a \code{SpatialData}'s \code{points/} and \code{shapes/} 
#' layers, respectively. In both cases, these  are represented as a 
#' \code{duckspatial_df} (\code{data} slot), and associated with .zattrs 
#' represented as \code{\link{SpatialDataAttrs}} (\code{meta} slot); 
#' a list of \code{metadata} stores other arbitrary info.
#'
#' Currently defined methods (here, \code{x} is an \code{SpatialDataFrame}):
#' \itemize{
#' \item \code{data/meta(x)} access underlying data/.zattrs
#' \item \code{geom_type(x)} get the shape's type (e.g., POLYGON)
#' \item \code{names(x)} returns the underlying table's column names
#' \item \code{dim(x)} returns the dimensions of \code{data(x)}
#' \item \code{`$`,`[[`} directly access columns of \code{data(x)}
#' \item \code{filter,select} to subset rows/columns à la \code{dplyr}
#' \item \code{as.data.frame} to coerce \code{x} to a \code{data.frame}
#' }
#'
#' @param x,.data \code{SpatialDataFrame}
#' @param data \code{duckspatial_df} for on-disk representation,
#'   or a \code{data.frame} to be converted.
#' @param meta \code{\link{SpatialDataAttrs}}
#' @param metadata optional list of arbitrary
#'   content describing the overall object.
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param i,j indices for subsetting (see \code{?base::Extract}).
#' @param drop,pattern ignored.
#' @param ... optional arguments passed to and from other methods.
#' @param ik,fk character string specifying "instance_/feature_key" 
#'   of the spatialdata_attrs; used to match observations/features. 
#'
#' @return \code{SpatialDataFrame}
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="spatialdataR")
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

# new ----

#' @importFrom sf st_sf st_sfc st_as_sf st_point st_polygon
.df_to_sf <- \(data, type=c("POINT", "POLYGON")) {
    type <- match.arg(type)
    if (is.null(data) || isTRUE(nrow(data) == 0)) {
        # return empty data.frame with geometry column
        fn <- switch(type, POINT=st_point, st_polygon)
        return(st_sf(geometry=st_sfc(fn())[0], crs=NA))
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

#' @importFrom duckspatial ddbs_write_table
#' @importFrom duckspatial as_duckspatial_df
.duck <- \(data, name) {
    # silent complaint re: missing CRS
    suppressMessages( 
        ddbs_write_table(
            conn=.conn(),
            data=data,
            name=name,
            overwrite=TRUE,
            temp_view=FALSE))
    as_duckspatial_df(
        x=name, 
        conn=.conn(), 
        crs=NA_character_,
        geom_col=attr(data, "sf_column"))
}

#' @export
#' @rdname SpatialDataFrame
#' @importFrom methods is
#' @importFrom sf st_geometry_type
#' @importFrom S4Vectors metadata<-
#' @importFrom duckspatial as_duckspatial_df
SpatialDataPoint <- \(data=NULL, meta=SpatialDataAttrs(type="frame"), metadata=list(), ik=NULL, fk=NULL, ...) {
    data <- .df_to_sf(data, "POINT")
    if (isTRUE(nrow(data) > 0L)) {
        gt <- tryCatch(unique(st_geometry_type(data)), error=\(.) "n/a")
        if (!all(gt == "POINT")) stop(
            "only 'POINT' geometries supported; ",
            "found: ", paste(gt, collapse=", "))
    }
    if (!is(data, "duckspatial_df")) 
        data <- .duck(data, "sdPoint")
    za <- as.list(meta)
    if (is.null(za$spatialdata_attrs))
        za$spatialdata_attrs <- list()
    if (!is.null(ik)) {
        stopifnot(ik %in% colnames(data))
        instance_key(za) <- ik
    }
    if (!is.null(fk)) {
        stopifnot(fk %in% colnames(data))
        feature_key(za) <- fk
    }
    x <- .SpatialDataPoint(data=data, meta=SpatialDataAttrs(za), ...)
    metadata(x) <- metadata
    return(x)
}

#' @export
#' @rdname SpatialDataFrame
#' @importFrom methods is
#' @importFrom S4Vectors metadata<-
SpatialDataShape <- \(data=NULL, meta=SpatialDataAttrs(type="frame"), metadata=list(), ...) {
    data <- .df_to_sf(data, "POLYGON")
    if (!is(data, "duckspatial_df")) 
        data <- .duck(data, "sdShape")
    x <- .SpatialDataShape(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

# utils ----

#' @export
#' @rdname SpatialDataFrame
#' @importFrom dplyr tally pull
setMethod("length", "SpatialDataFrame", \(x) { 
    suppressWarnings(dplyr::pull(dplyr::tally(data(x)), .data$n))
})

#' @export
#' @rdname SpatialDataFrame
setMethod("dim", "SpatialDataFrame", \(x) c(length(x), ncol(data(x))))

#' @export
#' @rdname SpatialDataFrame
setMethod("names", "SpatialDataFrame", \(x) colnames(data(x)))

#' @export
#' @rdname SpatialDataFrame
#' @importFrom BiocGenerics as.data.frame
setMethod("as.data.frame", "SpatialDataFrame", \(x) as.data.frame(data(x)))
setAs(from="SpatialDataFrame", to="data.frame", \(from) as.data.frame(from))

#' @export
#' @rdname SpatialDataFrame
#' @importFrom dplyr slice
#' @importFrom sf st_as_sf st_geometry_type
setMethod("geom_type", "SpatialDataShape", \(x) {
    y <- st_as_sf(head(data(x), 1))
    z <- st_geometry_type(y)
    return(as.character(z))
})

# dplyr ----

#' @export
dplyr::pull
#' @export
#' @rdname SpatialDataFrame
#' @importFrom dplyr pull
pull.SpatialDataFrame <- \(.data, ...) pull(data(.data), ...)

#' @export
dplyr::select
#' @export
#' @rdname SpatialDataFrame
#' @importFrom dplyr select
select.SpatialDataFrame <- \(.data, ...) `data<-`(.data, value=select(data(.data), ...))

#' @export
dplyr::mutate
#' @export
#' @rdname SpatialDataFrame
#' @importFrom dplyr mutate
mutate.SpatialDataFrame <- \(.data, ...) `data<-`(.data, value=mutate(data(.data), ...))

#' @export
dplyr::filter
#' @export
#' @rdname SpatialDataFrame
#' @importFrom dplyr filter
filter.SpatialDataFrame <- \(.data, ...) `data<-`(.data, value=filter(data(.data), ...))

# get ----

#' @exportMethod [[
#' @rdname SpatialDataFrame
#' @importFrom dplyr pull
setMethod("[[", "SpatialDataFrame", \(x, i, ...) pull(data(x), i))

#' @export
#' @importFrom utils .DollarNames
.DollarNames.SpatialDataPoint <- \(x, pattern="") grepv(pattern, names(x))

#' @exportMethod $
#' @rdname SpatialDataFrame
#' @importFrom dplyr select all_of collect
setMethod("$", "SpatialDataPoint", \(x, name) do.call(`[[`, list(x, name)))

#' @export
#' @rdname SpatialDataFrame
#' @importFrom utils .DollarNames
.DollarNames.SpatialDataShape <- \(x, pattern="") grepv(pattern, names(x))

#' @exportMethod $
#' @rdname SpatialDataFrame
setMethod("$", "SpatialDataShape", \(x, name) do.call(`[[`, list(x, name)))

# sub ----

#' @export
#' @rdname SpatialDataFrame
#' @importFrom dplyr filter select all_of row_number 
setMethod("[", c("SpatialDataFrame", "ANY", "ANY"), \(x, i, j, ...) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE
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
})
