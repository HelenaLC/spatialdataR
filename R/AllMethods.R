

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
  a@data <- aperm(data(a), perm)
  a
})

# ------------------------------------------------------------------------------

#' @rdname SpatialData
#' @export
setMethod("images", "SpatialData", function(x) x@images)

#' @rdname SpatialData
#' @export
setMethod("labels", "SpatialData", function(x) x@labels)

#' @rdname SpatialData
#' @export
setMethod("shapes", "SpatialData", function(x) x@shapes)

#' @rdname SpatialData
#' @export
setMethod("points", "SpatialData", function(x) x@points)

#' @rdname SpatialData
#' @export
setMethod("image", "SpatialData", function(x, i = 1) x@images[[i]])

#' @rdname SpatialData
#' @export
setMethod("label", "SpatialData", function(x, i = 1) x@labels[[i]])

#' @rdname SpatialData
#' @export
setMethod("shape", "SpatialData", function(x, i = 1) x@shapes[[i]])

#' @rdname SpatialData
#' @export
setMethod("point", "SpatialData", function(x, i = 1) x@points[[i]])

#' @rdname SpatialData
#' @export
setMethod("imageNames", "SpatialData", function(x) names(images(x)))

#' @rdname SpatialData
#' @export
setMethod("labelNames", "SpatialData", function(x) names(labels(x)))

#' @rdname SpatialData
#' @export
setMethod("shapeNames", "SpatialData", function(x) names(shapes(x)))

#' @rdname SpatialData
#' @export
setMethod("pointNames", "SpatialData", function(x) names(points(x)))

#' @rdname SpatialData
#' @export
setMethod("elementNames", "SpatialData", function(x) {
  layers <- attributes(x)
  layers <- layers[setdiff(names(layers), c("metadata", "class"))]
  names(layers)[vapply(layers, \(.) length(.) > 0, logical(1))]
})

#' @rdname SpatialData
#' @export
setMethod("element", "SpatialData",
  function(x, elementName=elementNames(x)[1], which=1, ...) {
  getFromNamespace(elementName, "SpatialData")(x)[[which]]
})


