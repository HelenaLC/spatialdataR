# utils ------------------------------------------------------------------------

#' @export
#' @importFrom utils .DollarNames
.DollarNames.SpatialData <- function(x, pattern="") {
    grep(pattern, LAYERS, value=TRUE)
}

#' @rdname SpatialData
#' @aliases $,SpatialData-method
#' @exportMethod $
setMethod("$", "SpatialData", function(x, name) {
    attr(x, name)
})

#' @rdname SpatialData
#' @aliases [[,SpatialData-method
#' @exportMethod [[
setMethod("[[", "SpatialData", function(x, i, ...) {
    j <- grep(i, names(attributes(x)), value=TRUE)
    if (length(j)) return(attr(x, j))
    stop("'SpatialData' has no element '", i, "'")
})

#' @importFrom utils getFromNamespace
.check_i <- function(x, ele, i) {
    ele <- match.arg(ele, LAYERS)
    stopifnot(length(i) == 1)
    if (!length(x[[ele]]))
        stop("'SpatialData' object does not contain any '", ele, "'")
    if (is.character(i)) {
        fun <- paste0(gsub("s$", "", ele), "Names")
        fun <- getFromNamespace(fun, "SpatialData")
        stopifnot(
            i %in% fun(x),
            sum(grepl(i, fun(x))) == 1)
    } else {
        fun <- getFromNamespace(ele, "SpatialData")
        stopifnot(
            round(i) == i,
        i %in% seq_along(fun(x)))
    }
}

# images -----------------------------------------------------------------------

#' @rdname SpatialData
#' @export
setMethod("images", "SpatialData", function(x) x$images)

#' @rdname SpatialData
#' @export
setMethod("image", "SpatialData", function(x, i=1) {
    .check_i(x, "images", i)
    images(x)[[i]]
})

#' @rdname SpatialData
#' @export
setMethod("imageNames", "SpatialData", function(x) names(images(x)))

#' @rdname SpatialData
#' @export
setReplaceMethod("images",
    c("SpatialData", "list"),
    function(x, value) {
        x@images <- value
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "numeric"),
    function(x, i, value) {
        stopifnot(i <= length(images(x))+1)
        images(x)[[i]] <- value
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "character"),
    function(x, i, value) {
        images(x)[[i]] <- value
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("image",
    c("SpatialData", "missing"),
    function(x, i, value) {
        `image<-`(x=x, i=1, value=value)
    })

# labels -----------------------------------------------------------------------

#' @rdname SpatialData
#' @export
setMethod("labels", "SpatialData", function(x) x$labels)

#' @rdname SpatialData
#' @export
setMethod("label", "SpatialData", function(x, i=1) {
    .check_i(x, "labels", i)
    labels(x)[[i]]
})

#' @rdname SpatialData
#' @export
setMethod("labelNames", "SpatialData", function(x) names(labels(x)))

#' @rdname SpatialData
#' @export
setReplaceMethod("labels",
    c("SpatialData", "list"),
    function(x, value) {
        x@labels <- value
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "numeric"),
    function(x, i, value) {
        stopifnot(i <= length(images(x))+1)
        labels(x)[[i]] <- value
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "character"),
    function(x, i, value) {
        labels(x)[[i]] <- value
        return(x)
    })

#' @rdname SpatialData
#' @export
setReplaceMethod("label",
    c("SpatialData", "missing"),
    function(x, i, value) {
        `label<-`(x=x, i=1, value=value)
    })

# shapes -----------------------------------------------------------------------

#' @rdname SpatialData
#' @export
setMethod("shapes", "SpatialData", function(x) x$shapes)

#' @rdname SpatialData
#' @export
setMethod("shape", "SpatialData", function(x, i=1) {
    .check_i(x, "shapes", i)
    shapes(x)[[i]]
})

#' @rdname SpatialData
#' @export
setMethod("shapeNames", "SpatialData", function(x) names(shapes(x)))

# points -----------------------------------------------------------------------

#' @rdname SpatialData
#' @export
setMethod("points", "SpatialData", function(x) x$points)

#' @rdname SpatialData
#' @export
setMethod("point", "SpatialData", function(x, i=1) {
    .check_i(x, "points", i)
    points(x)[[i]]
})

#' @rdname SpatialData
#' @export
setMethod("pointNames", "SpatialData", function(x) names(points(x)))

# table ------------------------------------------------------------------------

#' @rdname SpatialData
#' @export
setMethod("table", "SpatialData", function(x) x$table)

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "SingleCellExperiment_OR_NULL"),
    function(x, value) {
        x@table <- NULL
        return(x)
    }
)

#' @rdname SpatialData
#' @export
setReplaceMethod("table",
    c("SpatialData", "ANY"),
    function(x, value) {
        stop("replacement value should be a",
            " 'SingleCellExperiment' or NULL")
    }
)

#' @rdname SpatialData
#' @export
setMethod("elementNames", "SpatialData", function(x) {
    layers <- attributes(x)[LAYERS]
    names(layers)[!vapply(layers, \(.)
        length(.) == 0 || is(., "name"),
        logical(1))]
})

#' @rdname SpatialData
#' @importFrom utils getFromNamespace
#' @export
setMethod("element", "SpatialData",
    function(x, elementName=elementNames(x)[1], i=1, ...) {
        .check_i(x, elementName, i)
        fun <- getFromNamespace(elementName, "SpatialData")
        fun(x)[[i]]
})
