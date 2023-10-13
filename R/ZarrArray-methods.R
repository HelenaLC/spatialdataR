#' @rdname ZarrArray
#' @export
setMethod("channels", "ImageArray", function(x) {
    # TODO: Gio said this'll move elsewhere in corrected version
    as.character(zattrs(x)$channels_metadata$channels$label)
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
        zattrs(x)$channels_metadata$channels$label <- value
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
    if (is.data.frame(x@data)) {
        x@data$dim[[1]]
    } else {
        dim(x@data)
    }
})

#' @rdname ZarrArray
#' @export
setMethod("dimnames", "ZarrArray", function(x) {
    if (!is.data.frame(x@data))
        dimnames(x@data)
})

.load <- function(x, i) {
    var <- deparse(substitute(x))
    x@data <- as.array(x, i)
    assign(var, x, parent.frame())
}

#' @importFrom Rarr read_zarr_array
as.array.ZarrArray <- function(x, i) {
    if (is.data.frame(x@data)) {
        if (missing(i)) {
            d <- length(x@data$dim[[1]])
            i <- vector("list", d)
        }
        read_zarr_array(x@data$path, i)
    } else {
        as.array(x@data)
    }
}

#' @rdname ZarrArray
#' @export
setMethod("as.array", "ZarrArray", as.array.ZarrArray)

#' @rdname ZarrArray
#' @export
setMethod("[", "ZarrArray", function(x, i, j, ...) {
    if (is.data.frame(x@data)) {
        if (missing(i)) i <- NULL
        if (missing(j)) j <- NULL
        d <- length(dim(x))
        if (nargs()-1 != d)
            stop("incorrect number of dimensions")
        if (missing(..0)) {
            k <- vector("list", d-2)
        } else {
            k <- list(...)
        }
        .load(x, c(list(i, j), k))
    } else {
        x@data <- x@data[i, j, ..., drop=FALSE]
        x
    }
})

#' @rdname ZarrArray
#' @importFrom BiocGenerics aperm
#' @export
setMethod("aperm", "ZarrArray", function(a, perm) {
    if (is.data.frame(a@data)) .load(a)
    if (missing(perm)) perm <- NULL
    a@data <- aperm(a@data, perm)
    a
})

getArrayElement <- S4Arrays:::getArrayElement
#' @rdname ZarrArray
#' @export
setMethod("getArrayElement", "ZarrArray", function(x, subscripts) {
    if (is.data.frame(x@data)) .load(x)
    if (is(x@data, "Array")) {
        getArrayElement(x@data, subscripts)
    } else {
        do.call(`[`, c(list(x=x@data), as.list(subscripts)))
    }
})

# TODO: not sure if/why we need this?
#' #' @rdname ZarrArray
#' #' @export
#' setMethod("extract_array", "ZarrArray", function(x, index) {
#'   extract_array(x@data, index)
#' })
