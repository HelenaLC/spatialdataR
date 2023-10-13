# utils ------------------------------------------------------------------------

#' @importFrom utils .DollarNames
#' @export
.DollarNames.SpatialData <- function(x, pattern="") {
    grep(pattern, LAYERS, value=TRUE)
}

#' @rdname SpatialData
#' @exportMethod $
setMethod("$", "SpatialData", function(x, name) {
    attr(x, name)
})

#' @rdname SpatialData
#' @exportMethod [[
setMethod("[[", "SpatialData", function(x, i, ...) {
    j <- grep(i, names(attributes(x)), value=TRUE)
    if (length(j)) return(attr(x, j))
    stop("'SpatialData' has no element '", i, "'")
})

#' @importFrom utils getFromNamespace
.check_i <- \(x, i, e) {
    e <- match.arg(e, LAYERS)
    stopifnot(length(i) == 1)
    if (!length(x[[e]]))
        stop("'SpatialData' object does not contain any '", e, "'")
    if (is.character(i)) {
        fun <- paste0(gsub("s$", "", e), "Names")
        fun <- getFromNamespace(fun, "SpatialData")
        stopifnot(
            i %in% fun(x),
            sum(!is.na(match(fun(x), i))) == 1)
    } else {
        fun <- getFromNamespace(e, "SpatialData")
        stopifnot(
            round(i) == i,
            i %in% seq_along(fun(x)))
    }
    return(i)
}

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

#' @rdname SpatialData
#' @export
setMethod("elements", "SpatialData", \(x) {
    y <- attributes(x)[setdiff(LAYERS, "tables")]
    y[vapply(y, length, integer(1)) > 0]
})

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

#' @rdname SpatialData
#' @export
setMethod("element", "SpatialData", \(x, e=1, i=1) {
    stopifnot(length(e) == 1, length(i) == 1)
    if (is.character(e)) {
        match.arg(e, LAYERS)
    } else if (is.integer(e)) {
        stopifnot(e <= length(elementNames(x)))
    } else stop("'e' should be a string or scalar integer")
    x[[e]][[.check_i(x, i, e)]]
})

# set all ----

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
    \(x, i, value) {
        images(x)[[i]] <- value
        if (is.null(image(x)))
            images(x) <- images(x)[-1]
        return(x)
})

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "character", "LabelArray"),
    \(x, i, value) {
        labels(x)[[i]] <- value
        if (is.null(label(x)))
            labels(x) <- labels(x)[-1]
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("shape",
    c("SpatialData", "character", "ShapeFrame"),
    \(x, i, value) {
        shapes(x)[[i]] <- value
        if (is.null(shape(x)))
            shapes(x) <- shapes(x)[-1]
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("point",
    c("SpatialData", "character", "PointFrame"),
    \(x, i, value) {
        points(x)[[i]] <- value
        if (is.null(point(x)))
            points(x) <- points(x)[-1]
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "character", "SingleCellExperiment"),
    \(x, i, value) {
        tables(x)[[i]] <- value
        if (is.null(table(x)))
            tables(x) <- tables(x)[-1]
        return(x)
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
setReplaceMethod("shape",
    c("SpatialData", "missing", "ShapeFrame"),
    \(x, i, value) `shape<-`(x=x, i=1, value=value))

#' @rdname SpatialData
#' @export
setReplaceMethod("point",
    c("SpatialData", "missing", "PointFrame"),
    \(x, i, value) `point<-`(x=x, i=1, value=value))

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "missing", "SingleCellExperiment"),
    \(x, i, value) `table<-`(x=x, i=1, value=value))

# _i=numeric ----

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "numeric", "ImageArray"), \(x, i=1, value) 
    { stopifnot(i <= length(images(x))+1); images(x)[[i]] <- value; x })

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "numeric", "LabelArray"), \(x, i=1, value) 
    { stopifnot(i <= length(labels(x))+1); labels(x)[[i]] <- value; x })

#' @rdname SpatialData
#' @export
setReplaceMethod("shape",
    c("SpatialData", "numeric", "ShapeFrame"), \(x, i=1, value) 
    { stopifnot(i <= length(shapes(x))+1); shapes(x)[[i]] <- value; x })

#' @rdname SpatialData
#' @export
setReplaceMethod("point",
    c("SpatialData", "numeric", "PointFrame"), \(x, i=1, value) 
    { stopifnot(i <= length(points(x))+1); points(x)[[i]] <- value; x })

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "numeric", "SingleCellExperiment"), \(x, i=1, value) 
    { stopifnot(i <= length(tables(x))+1); tables(x)[[i]] <- value; x })

# _value=NULL ----

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

# _value=ANY ----

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) { if (missing(i)) i <- NULL
    stop("replacement value should be a 'ImageArray'") })

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) { if (missing(i)) i <- NULL
    stop("replacement value should be a 'LabelArray'") })

#' @rdname SpatialData
#' @export
setReplaceMethod("shape",
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) { if (missing(i)) i <- NULL
    stop("replacement value should be a 'ShapeFrame'") })

#' @rdname SpatialData
#' @export
setReplaceMethod("point",
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) { if (missing(i)) i <- NULL
    stop("replacement value should be a 'PointFrame'") })

#' @rdname SpatialData
#' @export
setReplaceMethod("table", 
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) { if (missing(i)) i <- NULL
    stop("replacement value should be a 'SingleCellExperiment'") })

# get nms ----

#' @rdname SpatialData
#' @export
setMethod("imageNames", "SpatialData", \(x) names(images(x)))

#' @rdname SpatialData
#' @export
setMethod("labelNames", "SpatialData", \(x) names(labels(x)))

#' @rdname SpatialData
#' @export
setMethod("shapeNames", "SpatialData", \(x) names(shapes(x)))

#' @rdname SpatialData
#' @export
setMethod("pointNames", "SpatialData", \(x) names(points(x)))

#' @rdname SpatialData
#' @export
setMethod("tableNames", "SpatialData", \(x) names(tables(x)))

#' @rdname SpatialData
#' @export
setMethod("elementNames", "SpatialData", \(x) {
    y <- lapply(attributes(x)[LAYERS], names)
    y[vapply(y, length, integer(1)) > 0]
})

# set nms ----

#' @rdname SpatialData
#' @export
setReplaceMethod("imageNames",
    c("SpatialData", "character"),
    \(x, value) `images<-`(x, `names<-`(images(x), value)))

#' @rdname SpatialData
#' @export
setReplaceMethod("labelNames",
    c("SpatialData", "character"),
    \(x, value) `labels<-`(x, `names<-`(labels(x), value)))

#' @rdname SpatialData
#' @export
setReplaceMethod("shapeNames",
    c("SpatialData", "character"),
    \(x, value) `shapes<-`(x, `names<-`(shapes(x), value)))

#' @rdname SpatialData
#' @export
setReplaceMethod("pointNames",
    c("SpatialData", "character"),
    \(x, value) `points<-`(x, `names<-`(points(x), value)))

#' @rdname SpatialData
#' @export
setReplaceMethod("tableNames",
    c("SpatialData", "character"),
    \(x, value) `tables<-`(x, `names<-`(tables(x), value)))
