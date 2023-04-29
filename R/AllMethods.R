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
  stop(sprintf("'SpatialData' has no element '%s'", i))

})

#' @importFrom S4Vectors metadata
#' @rdname ImageArray
#' @export
setMethod("metadata", "ImageArray", function(x) {
  x@metadata
})

#' @rdname ImageArray
#' @export
setMethod("dim", "ImageArray", function(x) {
  dim(x@data)
})

#' @rdname ImageArray
#' @export
setMethod("dimnames", "ImageArray", function(x) {
  dimnames(x@data)
})

# TODO: not sure if/why we need this?
#' #' @rdname ImageArray
#' #' @export
#' setMethod("extract_array", "ImageArray", function(x, index) {
#'   extract_array(x@data, index)
#' })

#' @rdname ImageArray
#' @export
setMethod("[", "ImageArray", function(x, i, j, ...) {
  x@data <- x@data[i, j, ..., drop=FALSE]
  x
})

getArrayElement <- S4Arrays:::getArrayElement
#' @rdname ImageArray
#' @export
setMethod("getArrayElement", "ImageArray", function(x, subscripts) {
  if (is(x@data, "Array")) {
    getArrayElement(x@data, subscripts)
  } else {
    do.call(`[`, c(list(x=x@data), as.list(subscripts)))
  }
})

#' @rdname ImageArray
#' @export
setMethod("as.array", "ImageArray", function(x) {
  as.array(x@data)
})

#' @rdname ImageArray
#' @importFrom BiocGenerics aperm
#' @export
setMethod("aperm", "ImageArray", function(a, perm) {
  if (missing(perm)) perm <- NULL
  a@data <- aperm(a@data, perm)
  a
})

.check_i <- function(x, ele, i) {
  ele <- match.arg(ele, LAYERS)
  stopifnot(length(i) == 1)
  if (!length(x[[ele]]))
    stop(sprintf("'SpatialData' object does not contain any '%s'", ele))
  if (is.character(i)) {
    fun <- paste0(ele, "Names")
    fun <- getFromNamespace(fun, "SpatialData")
    stopifnot(
      i %in% fun(x),
      sum(grepl(i, fun(x))) == 1)
  } else {
    fun <- paste0(ele, "s")
    fun <- getFromNamespace(fun, "SpatialData")
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
  .check_i(x, "image", i)
  images(x)[[i]]
})

#' @rdname SpatialData
#' @export
setMethod("imageNames", "SpatialData", function(x) names(images(x)))

# labels -----------------------------------------------------------------------

#' @rdname SpatialData
#' @export
setMethod("labels", "SpatialData", function(x) x$labels)

#' @rdname SpatialData
#' @export
setMethod("label", "SpatialData", function(x, i=1) {
  .check_i(x, "label", i)
  labels(x)[[i]]
})

#' @rdname SpatialData
#' @export
setMethod("labelNames", "SpatialData", function(x) names(labels(x)))

# shapes -----------------------------------------------------------------------

#' @rdname SpatialData
#' @export
setMethod("shapes", "SpatialData", function(x) x$shapes)

#' @rdname SpatialData
#' @export
setMethod("shape", "SpatialData", function(x, i=1) {
  .check_i(x, "shape", i)
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
  .check_i(x, "point", i)
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
    stop(
      "replacement value should be a ",
      "'SingleCellExperiment' or NULL")
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
#' @export
setMethod("element", "SpatialData",
  function(x, elementName=elementNames(x)[1], which=1, ...) {
    stopifnot(
      length(which) == 1,
      is.character(which) || which == round(which))
    ele <- utils::getFromNamespace(elementName, "SpatialData")(x)
    if (length(ele) == 0)
      stop(sprintf("'SpatialData' object does not contain any '%s'", elementName))
    if (is.character(which)) {
      stopifnot(which %in% names(ele))
    } else {
      stopifnot(which %in% seq_along(ele))
    }
    ele[[which]]
})
