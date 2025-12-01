#' @name SpatialData
#' @title The `SpatialData` class
#' 
#' @aliases images labels points shapes tables
#' ImageArray LabelArray PointFrame ShapeFrame
#' 
#' @param images list of \code{\link{ImageArray}}s
#' @param labels list of \code{\link{LabelArray}}s
#' @param points list of \code{\link{PointFrame}}s
#' @param shapes list of \code{\link{ShapeFrame}}s
#' @param tables list of \code{SingleCellExperiment}s
#' @param data 
#'   For \code{Image/LabelArray}s, a list of \code{ZarrArray}s;
#'   For \code{Shape/PointFrame}s, an \code{arrow} Table or derivative.
#' @param zattrs \code{Zattrs} objects; used to represent .zattrs.
#' @param metadata (option) list of free-form extra data.
#' @param x \code{SpatialData} object.
#' @param ... ignored.
#' 
#' @return \code{SpatialData}
#' 
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' (sd <- readSpatialData(pa))
#' 
#' names(sd)
#' sd@images[[1]]
#' sd[-4, ][, 1]
#' sd[1:2, c(1,1)]
#' sd["shapes", c(1,3)]
#' region(sd, "table")
#' 
#' @importFrom S7 new_class new_generic method class_list check_is_S7 
#' @importFrom SingleCellExperiment int_metadata
#' @importFrom SummarizedExperiment colData
#' @importFrom methods is initialize
#' @export
SpatialData <- new_class("SpatialData",
    properties=list(
        images=class_list,
        labels=class_list,
        points=class_list,
        shapes=class_list,
        tables=class_list
    ),
    validator=\(self) {
        ok <- character()
        slot <- c("images", "labels", "points", "shapes", "tables")
        type <- list(ImageArray, LabelArray, PointFrame, ShapeFrame, "SingleCellExperiment")
        .all <- \(x, y) {
            f <- if (is.character(y)) {
                \(.) is(., y)
            } else {
                \(.) is.null(check_is_S7(., y))
            }
            all(vapply(x, f, logical(1)))
        }
        ok <- c(ok, mapply(x=slot, y=type, \(x, y) {
            if (length(z <- attr(self, x)) && !.all(z, y))
                sprintf("'@%s' should be a list of '%s's", x, y)
            l <- slot(self, x)
            if (length(l)) {
                nms <- names(l)
                len <- vapply(nms, nchar, integer(1))
                if (is.null(nms) || any(len == 0))
                    sprintf("'@%s' should be a fully named list", x)
            }
        }, SIMPLIFY=FALSE) |> unlist())
        ts <- self@tables
        for (t in ts) {
            za <- int_metadata(t)$zattrs
            # 'table' annotates an existing region
            if (!za$region %in% unlist(names(self))) 
                ok <- c(ok, "'region' invalid")
            # specified 'instance_key' is present
            if (!za$instance_key %in% names(colData(t))) 
                ok <- c(ok, "'instance_key' invalid")
        }
        return(ok)
    }
)
names(.LAYERS) <- .LAYERS <- names(SpatialData@properties)

method(`[[`, SpatialData) <- \(x, i) attr(x, .LAYERS[i])

#' @importFrom S7 prop_names
#' @importFrom methods slot
method(`[`, SpatialData) <- \(x, i, j) {
    ps <- prop_names(x)
    if (missing(i)) {
        n <- vapply(ps, \(.) length(slot(x, .)), integer(1))
        i <- ps[n > 0]
    } else {
        if (is.numeric(i)) {
            stopifnot(abs(i) <= length(ps), i == round(i))
            i <- ps[i]
        } else if (is.character(i)) {
            i <- match.arg(i, ps, TRUE)
        }
    }
    if (missing(j)) {
        j <- !logical(length(i))
    } else {
        if (length(i) == 1) {
            j <- list(j)
        } else {
            if (length(j) == 1)
                j <- replicate(length(i), j, FALSE)
            j <- as.list(j)
        }
        stopifnot(length(j) == length(i))
        for (. in seq_along(i)) {
            .j <- j[[.]]
            l <- slot(x, i[.])
            if (is.character(.j)) {
                match.arg(.j, names(l), TRUE)
            } else {
                n <- length(l)
                stopifnot(abs(.j) <= n, .j == round(.j))
            }
        }
    }
    l <- mapply(i=i, j=j, \(i, j) {
        slot(x, i)[j]
    }, SIMPLIFY=FALSE)
    do.call(SpatialData, l)
}

method(names, SpatialData) <- \(x) lapply(.LAYERS, \(.) names(slot(x, .)))

#' @name SpatialData
#' @export
region <- new_generic("region", "x")
method(region, SpatialData) <- \(x, i) {
    ok <- vapply(names(x), \(.) i %in% ., logical(1))
    if (!any(ok)) stop("'i' invalid")
    slot(x, names(which(ok)))[[i]]
}

# NOTE: not needed? x@slot & names(x@slot) work just fine
# .get <- \(x, i, slot) {
#     data <- attr(x, slot)
#     if (missing(i)) return(data)
#     if (length(i) == 1) data[[i]] else data[i]
# }
# 
# for (f in .LAYERS) {
#     g <- sprintf("\\(x, i) .get(x, i, '%s')", f)
#     assign(f, new_generic(f, "x"))
#     `method<-`(
#         signature=SpatialData, 
#         value=eval(parse(text=g)),
#         generic=eval(parse(text=f)))
# }
# 
# for (. in .LAYERS) {
#     f <- paste0(gsub("s$", "", .), "Names")
#     g <- sprintf("\\(x) names(slot(x, '%s'))", .)
#     assign(f, new_generic(f, "x"))
#     `method<-`(
#         signature=SpatialData, 
#         value=eval(parse(text=g)),
#         generic=eval(parse(text=f)))
# }

# Zattrs ----

#' @importFrom S7 new_class
Zattrs <- new_class("Zattrs", properties=list(data=class_list))

#' @importFrom S7 method
#' @importFrom utils .DollarNames
method(.DollarNames, Zattrs) <- \(x, pattern="") names(x@data)
method(`$`, Zattrs) <- \(x, name) x@data[[name]]

# sdArray ----

#' @importFrom S7 new_class class_list
#' @importFrom methods is
sdArray <- new_class(
    name="sdArray", 
    properties=list(
        data=class_list,
        zattrs=Zattrs,
        metadata=class_list),
    validator=\(self) {
        x <- self@data
        y <- "ZarrArray"
        if (length(x) && !all(vapply(x, \(.) is(., y), logical(1)))) 
            sprintf("'@data' should be a list of '%s's", y)
    }
); S7::S4_register(sdArray)

#' @name SpatialData
#' @export
ImageArray <- new_class("ImageArray", parent=sdArray)

channels <- new_generic("channels", "x")
method(channels, ImageArray) <- \(x) {
    x <- zattrs(x)$omero$channels
    unlist(lapply(x, \(.) .$label))
}

#' @rdname SpatialData
#' @export
LabelArray <- new_class("LabelArray", parent=sdArray)

# sdFrame ----

#' @importFrom S7 new_union class_data.frame new_S3_class
.sdFrame <- new_union(
    class_data.frame, 
    new_class("Table"),
    new_class("arrow_dplyr_query"),
    new_S3_class("FileSystemDataset"))

#' @importFrom S7 class_any
sdFrame <- new_class(
    name="sdFrame", 
    properties=list(
        data=class_any,
        zattrs=Zattrs,
        metadata=class_list),
    validator=\(self) {
        ok <- c("Table", "arrow_dplyr_query", "FileSystemDataset")
        ok <- vapply(ok, \(.) is(self@data, .), logical(1))
        ok <- c(ok, is.data.frame(self@data))
        if (!any(ok)) "invalid 'data'"
    }
); S7::S4_register(sdFrame)

#' @rdname SpatialData
#' @export
PointFrame <- new_class("PointFrame", parent=sdFrame,
    validator=\(self) {
        # if (is.null(feature_key(self)) ||
        #     !feature_key(self) %in% names(self))
        #     "invalid 'feature_key'"
    })

#' @rdname SpatialData
#' @export
feature_key <- new_generic("feature_key", "x")
method(feature_key, PointFrame) <- \(x)
    x@zattrs@data$spatialdata_attrs$feature_key

#' @rdname SpatialData
#' @export
instance_key <- new_generic("instance_key", "x")
method(instance_key, PointFrame) <- \(x)
    x@zattrs@data$spatialdata_attrs$instance_key

#' @rdname SpatialData
#' @export
ShapeFrame <- new_class("ShapeFrame", parent=sdFrame)

#' @rdname SpatialData
#' @export
zattrs <- new_generic("zattrs", "x")
method(zattrs, sdArray) <- \(x) x@zattrs
method(zattrs, sdFrame) <- \(x) x@zattrs

#' @importFrom S4Vectors metadata
method(metadata, sdArray) <- \(x) x@metadata
method(metadata, sdFrame) <- \(x) x@metadata

#' @rdname SpatialData
#' @export
data <- new_generic("data", "x")
method(data, sdFrame) <- \(x) x@data
method(data, sdArray) <- \(x, k) {
    if (missing(k)) return(x@data)
    stopifnot(length(k) == 1, is.numeric(k), k > 0)
    n <- length(x@data) # get number of available scales
    if (is.infinite(k)) k <- n # input of Inf uses lowest
    if (k <= n) return(x@data[[k]]) # return specified scale
    stop("'k=", k, "' but only ", n, " resolution(s) available")
}

method(dim, sdArray) <- \(x) dim(data(x, 1))
method(dim, sdFrame) <- \(x) dim(data(x))

method(length, sdArray) <- \(x) length(data(x))
method(length, sdFrame) <- \(x) dim(data(x))[1]

method(names, sdFrame) <- \(x) setdiff(names(data(x)), "__null_dask_index__")

#' @importFrom utils .DollarNames
method(.DollarNames, sdFrame) <- \(x, pattern="") names(x)
method(`$`, sdFrame) <- \(x, name) data(x)[, name]

# subsetting ----
method(`[`, sdFrame) <- \(x, i) {
    n <- dim(x)[1]
    .i <- seq_len(n)
    if (missing(i)) {
        i <- .i
    } else if (is.numeric(i)) {
        stopifnot(i == round(i), i %in% .i)
    } else if (is.logical(i)) {
        stopifnot(length(i) %in% c(1, n))
        i <- .i[i]
    }
    x@data <- x@data[i, ]
    return(x)
}
