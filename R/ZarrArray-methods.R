#' @rdname ZarrArray
#' @export
setMethod("channels", "ImageArray", function(x) {
    # TODO: Gio said this'll move elsewhere in corrected version
    as.character(x@zattrs$channels_metadata$channels$label)
})

#' @rdname ZarrArray
#' @export
setReplaceMethod("channels",
    c("ImageArray", "character"),
    function(x, value) {
        new <- length(value)
        old <- length(channels(x))
        if (new != old)
            stop("new channel names (", new, ") must be of the same",
                " length as the number of channels (", old, ")")
        x@zattrs$channels_metadata$channels$label <- value
        return(x)
    }
)
#' @rdname ZarrArray
#' @export
setReplaceMethod("channels",
    c("ImageArray", "numeric"),
    function(x, value) {
        value <- as.character(value)
        `channels<-`(x=x, value=value)
    }
)

#' @rdname ZarrArray
#' @importFrom S4Vectors metadata
#' @export
setMethod("metadata", "ZarrArray", function(x) {
    x@metadata
})

#' @rdname ZarrArray
#' @export
setMethod("dim", "ZarrArray", function(x) {
    dim(x@data)
})

#' @rdname ZarrArray
#' @export
setMethod("dimnames", "ZarrArray", function(x) {
    dimnames(x@data)
})

# TODO: not sure if/why we need this?
#' #' @rdname ZarrArray
#' #' @export
#' setMethod("extract_array", "ZarrArray", function(x, index) {
#'   extract_array(x@data, index)
#' })

#' @rdname ZarrArray
#' @export
setMethod("[", "ZarrArray", function(x, i, j, ...) {
    x@data <- x@data[i, j, ..., drop=FALSE]
    x
})

getArrayElement <- S4Arrays:::getArrayElement
#' @rdname ZarrArray
#' @export
setMethod("getArrayElement", "ZarrArray", function(x, subscripts) {
    if (is(x@data, "Array")) {
        getArrayElement(x@data, subscripts)
    } else {
        do.call(`[`, c(list(x=x@data), as.list(subscripts)))
    }
})

#' @rdname ZarrArray
#' @export
setMethod("as.array", "ZarrArray", function(x) {
    as.array(x@data)
})

#' @rdname ZarrArray
#' @importFrom BiocGenerics aperm
#' @export
setMethod("aperm", "ZarrArray", function(a, perm) {
    if (missing(perm)) perm <- NULL
    a@data <- aperm(a@data, perm)
    a
})
