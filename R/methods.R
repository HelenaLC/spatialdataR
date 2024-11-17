#' @importFrom utils .DollarNames
#' @export
.DollarNames.SpatialData <- \(x, pattern="") grep(pattern, .LAYERS, value=TRUE)

#' @rdname SpatialData
#' @exportMethod $
setMethod("$", "SpatialData", \(x, name) attr(x, name))

#' @export
setMethod("[[", c("SpatialData", "numeric"), \(x, i, ...) {
    i <- .LAYERS[i]
    callNextMethod(x, i)
})

#' @export
setMethod("[[", c("SpatialData", "character"), \(x, i, ...) {
    attr(x, grep(i, names(attributes(x)), value=TRUE))
})

#' @importFrom utils getFromNamespace
.check_i <- \(x, i, e) {
    e <- match.arg(e, .LAYERS)
    stopifnot(length(i) == 1)
    if (!length(x[[e]]))
        stop("'SpatialData' object does not contain any '", e, "'")
    if (is.character(i)) {
        fun <- paste0(gsub("s$", "", e), "Names")
        fun <- get(fun)
        #fun <- getFromNamespace(fun, "SpatialData")
        stopifnot(
            i %in% fun(x),
            sum(!is.na(match(fun(x), i))) == 1)
    } else {
        fun <- get(e)
        #fun <- getFromNamespace(e, "SpatialData")
        stopifnot(
            round(i) == i,
            i %in% seq_along(fun(x)))
    }
    return(i)
}

# sub ----

setMethod("[", "SpatialData", \(x, i, j, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE
    i <- if (isFALSE(i)) {
        numeric()
    } else if (isTRUE(i)) {
        seq_along(.LAYERS)
    } else if (is.numeric(i) | is.logical(i)) {
        seq_along(.LAYERS)[i]
    } else if (is.character(i)) {
        i <- match.arg(i, .LAYERS, TRUE)
        which(.LAYERS %in% i)
    }
    if (any(is.na(i))) stop("out of bounds 'i'")
    # TODO: validity
    if (isTRUE(j)) {
        j <- replicate(length(i), TRUE, FALSE)
    } else {
        if (length(i) == 1) {
            if (!is.list(j)) j <- list(j)
        } else {
            if (length(i) != length(j)) stop()
        }
    }
    for (. in setdiff(seq_along(.LAYERS), i)) x[[.]] <- list()
    .e <- \(.) stop("out of bounds 'j' for layer ", ., " (", .LAYERS[.], ")")
    for (. in seq_along(i)) {
        j[[.]] <- if (isFALSE(j[[.]])) {
            numeric()
        } else if (isTRUE(j[[.]])) {
            seq_along(x[[i[.]]])
        } else if (is.numeric(j[[.]]) | is.logical(j[[.]])) {
            seq_along(x[[i[.]]])[j[[.]]]
        } else if (is.character(j[[.]])) {
            js <- names(x[[i[.]]])
            if (any(!j[[.]] %in% js)) .e(i[.])
            j[[.]] <- which(js %in% j[[.]])
        }
        js <- seq_along(x[[i[.]]])
        if (!isTRUE(j[[.]])) {
            if (any(!j[[.]] %in% js)) .e(i[.])
            x[[i[.]]] <- x[[i[.]]][j[[.]]]
        }
    }
    return(x)
})

# any ----

#' @rdname SpatialData
#' @export
setMethod("data", "SpatialDataElement", \(x) x@data)

#' @rdname SpatialData
#' @export
setMethod("meta", "SpatialDataElement", \(x) x@meta)

# get all ----

#' @rdname SpatialData
#' @export
setMethod("images", "SpatialData", \(x) x$images)

#' @rdname SpatialData
#' @export
setMethod("labels", "SpatialData", \(x) x$labels)

#' @rdname SpatialData
#' @export
setMethod("shapes", "SpatialData", \(x) x$shapes)

#' @rdname SpatialData
#' @export
setMethod("points", "SpatialData", \(x) x$points)

#' @rdname SpatialData
#' @export
setMethod("tables", "SpatialData", \(x) x$tables)

# nms ----

#' @rdname SpatialData
#' @export
setMethod("imageNames", "SpatialData", \(x) names(x$images))

#' @rdname SpatialData
#' @export
setMethod("labelNames", "SpatialData", \(x) names(x$labels))

#' @rdname SpatialData
#' @export
setMethod("shapeNames", "SpatialData", \(x) names(x$shapes))

#' @rdname SpatialData
#' @export
setMethod("pointNames", "SpatialData", \(x) names(x$points))

#' @rdname SpatialData
#' @export
setMethod("tableNames", "SpatialData", \(x) names(x$tables))

# get one ----

#' @rdname SpatialData
#' @export
setMethod("image", "SpatialData", \(x, i=1) {
    if (length(y <- images(x)))
        y[[.check_i(x, i, "images")]] else y })

#' @rdname SpatialData
#' @export
setMethod("label", "SpatialData", \(x, i=1) {
    if (length(y <- labels(x)))
        y[[.check_i(x, i, "labels")]] else y })

#' @rdname SpatialData
#' @export
setMethod("shape", "SpatialData", \(x, i=1) {
    if (length(y <- shapes(x)))
        y[[.check_i(x, i, "shapes")]] else y })

#' @rdname SpatialData
#' @export
setMethod("point", "SpatialData", \(x, i=1) {
    if (length(y <- points(x)))
        y[[.check_i(x, i, "points")]] else y })

#' @rdname SpatialData
#' @export
setMethod("table", "SpatialData", \(x, i=1) {
    if (length(y <- tables(x)))
        y[[.check_i(x, i, "tables")]] else y })

# set all ----

#' @rdname SpatialData
#' @export
setReplaceMethod("[[", c("SpatialData", "numeric"), 
    \(x, i, value) { attr(x, .LAYERS[i]) <- value; return(x) })

#' @rdname SpatialData
#' @export
setReplaceMethod("[[", c("SpatialData", "character"), 
    \(x, i, value) {attr(x, i) <- value; return(x) })

# |_value=list ----

#' @rdname SpatialData
#' @export
setReplaceMethod("images",
    c("SpatialData", "list"),
    \(x, value) {x@images <- value; x})

#' @rdname SpatialData
#' @export
setReplaceMethod("labels",
    c("SpatialData", "list"),
    \(x, value) {x@labels <- value; x})

#' @rdname SpatialData
#' @export
setReplaceMethod("shapes",
    c("SpatialData", "list"),
    \(x, value) {x@shapes <- value; x})

#' @rdname SpatialData
#' @export
setReplaceMethod("points",
    c("SpatialData", "list"),
    \(x, value) {x@points <- value; x})

#' @rdname SpatialData
#' @export
setReplaceMethod("tables", 
    c("SpatialData", "list"),
    \(x, value) {x@tables <- value; x})

# set one ----

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "character", "ImageArray"),
    \(x, i, value) { x@images[[i]] <- value; x })

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "character", "LabelArray"),
    \(x, i, value) { x@labels[[i]] <- value; x })

#' @rdname SpatialData
#' @export
setReplaceMethod("point",
    c("SpatialData", "character", "PointFrame"),
    \(x, i, value) { x@points[[i]] <- value; x })

#' @rdname SpatialData
#' @export
setReplaceMethod("shape",
    c("SpatialData", "character", "ShapeFrame"),
    \(x, i, value) { x@shapes[[i]] <- value; x })

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "character", "SingleCellExperiment"),
    \(x, i, value) { x@tables[[i]] <- value; x })

# TODO: something like table(x)$cluster_id <- doesn't work atm... 
# not sure how to get around without defining all the possible 
# SCE replacement methods :/ 

# _i=numeric ----

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "numeric", "ImageArray"), 
    \(x, i=1, value) { 
        i <- ifelse(
            i > (n <- length(points(x))), 
            paste0("image", n+1), 
            pointNames(x)[i])
        `image<-`(x=x, i=i, value=value)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "numeric", "LabelArray"), 
    \(x, i=1, value) { 
        i <- ifelse(
            i > (n <- length(points(x))), 
            paste0("label", n+1), 
            pointNames(x)[i])
        `label<-`(x=x, i=i, value=value)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("point",
    c("SpatialData", "numeric", "PointFrame"), 
    \(x, i=1, value) { 
        i <- ifelse(
            i > (n <- length(points(x))), 
            paste0("point", n+1), 
            pointNames(x)[i])
        `point<-`(x=x, i=i, value=value)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("shape",
    c("SpatialData", "numeric", "ShapeFrame"), 
    \(x, i=1, value) { 
        i <- ifelse(
            i > (n <- length(points(x))), 
            paste0("shape", n+1), 
            pointNames(x)[i])
        `shape<-`(x=x, i=i, value=value)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "numeric", "SingleCellExperiment"), 
    \(x, i=1, value) { 
        i <- ifelse(
            i > (n <- length(points(x))), 
            paste0("table", n+1), 
            pointNames(x)[i])
        `table<-`(x=x, i=i, value=value)
    })

# _i=missing ----

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "missing", "ImageArray"),
    \(x, i, value) `image<-`(x=x, i=1, value=value))

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "missing", "LabelArray"),
    \(x, i, value) `label<-`(x=x, i=1, value=value))

#' @rdname SpatialData
#' @export
setReplaceMethod("point",
    c("SpatialData", "missing", "PointFrame"),
    \(x, i, value) `point<-`(x=x, i=1, value=value))

#' @rdname SpatialData
#' @export
setReplaceMethod("shape",
    c("SpatialData", "missing", "ShapeFrame"),
    \(x, i, value) `shape<-`(x=x, i=1, value=value))

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "missing", "SingleCellExperiment"),
    \(x, i, value) `table<-`(x=x, i=1, value=value))

# _v=NULL ----

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "ANY", "NULL"), \(x, i, value) 
    {if (missing(i)) i <- 1; images(x)[[i]] <- value; x})

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "ANY", "NULL"), \(x, i, value) 
    {if (missing(i)) i <- 1; labels(x)[[i]] <- value; x})

#' @rdname SpatialData
#' @export
setReplaceMethod("shape",
    c("SpatialData", "ANY", "NULL"), \(x, i, value) 
    {if (missing(i)) i <- 1; shapes(x)[[i]] <- value; x})

#' @rdname SpatialData
#' @export
setReplaceMethod("point",
    c("SpatialData", "ANY", "NULL"), \(x, i, value) 
    {if (missing(i)) i <- 1; points(x)[[i]] <- value; x})

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "ANY", "NULL"), \(x, i, value) 
    {if (missing(i)) i <- 1; tables(x)[[i]] <- value; x})

# _v=ANY ----

.msg <- \(.) sprintf("replacement value should be a '%s'", .)

#' @rdname SpatialData
#' @export
setReplaceMethod("image", 
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) stop(.msg("ImageArray")))

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) stop(.msg("LabelArray")))

#' @rdname SpatialData
#' @export
setReplaceMethod("shape", 
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) stop(.msg("ShapeFrame")))

#' @rdname SpatialData
#' @export
setReplaceMethod("point", 
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) stop(.msg("PointFrame")))

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) stop(.msg("SingleCellExperiment")))