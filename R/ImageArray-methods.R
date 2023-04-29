#' @rdname ImageArray
#' @importFrom S4Vectors metadata
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
