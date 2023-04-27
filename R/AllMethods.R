#' @rdname ImageArray
#' @export
setMethod("data", "ImageArray", function(x) {
  x@data
})

#' @rdname ImageArray
#' @importFrom S4Vectors metadata
#' @export
setMethod("metadata", "ImageArray", function(x) {
  x@metadata
})

#' @rdname ImageArray
#' @export
setMethod("dim", "ImageArray", function(x) {
  dim(data(x))
})

#' @rdname ImageArray
#' @export
setMethod("dimnames", "ImageArray", function(x) {
  dimnames(data(x))
})

#' #' @rdname ImageArray
#' #' @export
#' setMethod("extract_array", "ImageArray", function(x, index) {
#'   extract_array(data(x), index)
#' })

#' @rdname ImageArray
#' @export
setMethod("[", "ImageArray", function(x, i, j, ...) {
  x@data <- data(x)[i, j, ..., drop=FALSE]
  x
})
  
getArrayElement <- S4Arrays:::getArrayElement
#' @rdname ImageArray
#' @export
setMethod("getArrayElement", "ImageArray", function(x, subscripts) {
  if (is(data(x), "Array")) {
    getArrayElement(data(x), subscripts)
  } else {
    do.call(`[`, c(list(x=data(x)), as.list(subscripts)))
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