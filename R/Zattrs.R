# ==============================================================================
# All of the below is designated to .zattrs handling,
# in order to guarantee that we can eventually produce
# a valid OME-Zarr store with the same specs as pySpatialData.
# Ideally, if OME specs change, only this part of the code will
# have to be adapted (with potentially version-specific methods).
# ==============================================================================

# this class is defined solely for the purpose
# of validity checks & method dispatching...
Zattrs <- function(x=list()) {
    y <- .Zattrs(x)
    if (length(x))
        attr(y, "names") <- names(x)
    return(y)
}

.showZattrs <- function(object) {
    show(unclass(object))
}
setMethod("show", "Zattrs", .showZattrs)

.validityZattrs <- function(object) {
    msg <- NULL
    # TODO: write specs checker so that we don't accidentally do
    # anything stupid & can eventually write out a valid file...
    if (is.null(msg)) TRUE else msg
}

setValidity("Zattrs", .validityZattrs)

# getters & setters of 'zattrs' slot for all elements --------------------------

setMethod("zattrs", "SpatialDataElement", function(x) x@zattrs)

setReplaceMethod("zattrs",
    c("ZarrArray_OR_ShapeFrame", "Zattrs"),
    function(x, value) { x@zattrs <- value; x })

setReplaceMethod("zattrs",
    c("ZarrArray_OR_ShapeFrame", "list"),
    function(x, value) `zattrs<-`(x, Zattrs(value)))

# getters & setters for coordinate transformations -----------------------------

setMethod("getCoordTrans", "Zattrs", function(x, name=NULL) {
    ms <- x$multiscales
    if (!is.null(ms)) x <- ms
    ct <- x$coordinateTransformations
    ct <- if (is.data.frame(ct)) ct else ct[[1]]
    if (is.null(name))
        return(ct)
    if (is.numeric(name)) {
        if (max(name) > nrow(ct))
            stop("index exceeds number of",
                " available coords (", nrow(ct), ")")
        return(ct[name, ])
    }
    idx <- match(name, ct$output$name)
    if (is.na(idx))
        stop("couldn't find coords '", name, "'")
    return(ct[idx, ])
})

setMethod("getCoordTrans", "SpatialDataElement",
    function(x, name=NULL) getCoordTrans(zattrs(x), name))

setMethod("getCoordTrans", "SpatialData",
    function(x, i=1, name=NULL) {
        for (e in elementNames(x)) {
            .check_i(x, e, i)
            y <- tryCatch(x[[e]][[i]], error=function(e) NULL)
            if (!inherits(y, "error")) break
        }
        if (is.null(y)) stop("couldn't find coords '", name, "'")
        getCoordTrans(zattrs(y), name)
    }
)

setMethod("setCoordTrans", "Zattrs", function(x, value) {
    ms <- x$multiscales
    if (is.null(ms)) {
        ct <- x$coordinateTransformations
        if (is.data.frame(ct)) {
            x$coordinateTransformations <- value
        } else {
            x$coordinateTransformations[[1]] <- value
        }
    } else {
        x$multiscales$coordinateTransformations[[1]] <- value
    }
    return(x)
})

setMethod("setCoordTrans", "ANY", function(x, value)
    `zattrs<-`(x, setCoordTrans(zattrs(x), value)))

.newCoordTrans <- function(x, name, type="identity", data=NULL) {
    type <- match.arg(type, c("identity", "scale")) # TODO: more choices
    df <- getCoordTrans(x)[1, ]
    df$output$name <- name
    df$type <- type
    if (type != "identity") {
        if (!is.list(data))
            data <- list(data)
        df[[type]] <- data
    }
    return(df)
}

setMethod("addCoordTrans", "Zattrs",
    function(x, name, type="identity", data=NULL) {
        old <- getCoordTrans(x)
        df <- .newCoordTrans(x, name, type, data)
        # there has to be an easier way than this...
        # but 'data.frame' keeps flattening stuff
        l <- vector("list", nrow(old)+1)
        new <- data.frame(
            input=I(l), output=I(l),
            type=character(1),
            data=character(1))
        new$input <- rbind(old$input, df$input)
        new$output <- rbind(old$output, df$output)
        new$type <- c(old$type, df$type)
        if (is.null(old$data))
            old$data <- list(old$data)
        new$data <- c(old$data, df[[type]])
        # need an example with different transformations
        # to see what specs actually look like in the wild...
        nan <- vapply(new$data, is.null, logical(1))
        if (all(nan)) new$data <- NULL else {
            typ <- names(old)[ncol(old)]
            names(new)[ncol(new)] <- type
        }
        setCoordTrans(x, new)
    }
)

setMethod("addCoordTrans",
    "ZarrArray_OR_ShapeFrame",
    function(x, name, type, data) {
        l <- addCoordTrans(zattrs(x), name, type, data)
        zattrs(x) <- l
        return(x)
    }
)

setMethod("rmvCoordTrans", "Zattrs", function(x, name) {
    old <- getCoordTrans(x)
    idx <- match(name, old$output$name)
    new <- old[-idx[!is.na(idx)], ]
    setCoordTrans(x, new)
})

setMethod("rmvCoordTrans",
    "ZarrArray_OR_ShapeFrame",
    function(x, name) {
        l <- rmvCoordTrans(zattrs(x), name)
        zattrs(x) <- l
        return(x)
    }
)
