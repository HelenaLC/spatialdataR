#' @importFrom methods is setMethod callNextMethod setReplaceMethod

#' @export
#' @importFrom utils .DollarNames
.DollarNames.SpatialData <- \(x, pattern="") grep(pattern, .LAYERS, value=TRUE)

#' @exportMethod $
#' @rdname SpatialData
setMethod("$", "SpatialData", \(x, name) slot(x, name))

#' @exportMethod $<-
#' @rdname SpatialData
setReplaceMethod("$", "SpatialData", \(x, name, value) `[[<-`(x, i=name, value=value))

#' @export
#' @rdname SpatialData
setMethod("[[", c("SpatialData", "numeric"), \(x, i, ...) x[[.LAYERS[[i]]]])

#' @export
#' @rdname SpatialData
setMethod("[[", c("SpatialData", "character"), \(x, i, ...) slot(x, i))

# data/meta ----

#' @export
#' @rdname SpatialData
setMethod("data", "ANY", \(...) {
    l <- list(...)
    x <- l[[1]]
    if (!is(x, "SpatialDataElement")) 
        return(utils::data(...))
    if (!is(x, "SpatialDataArray")) 
        return(x@data)
    # return list of available scales
    k <- if (length(l) == 1) 1 else l[[2]]
    if (is.null(k)) return(x@data)
    # should be a scalar positive integer
    ok <- length(k) == 1 && is.numeric(k) && k > 0 && k == round(k)
    if (!ok) stop("invalid 'k'; should be ",
        "NULL or a scalar positive integer")
    # get number of available scales
    n <- length(x <- x@data)   
    # input of Inf uses lowest
    if (is.infinite(k)) k <- n 
    # return specified scale
    if (k <= n) return(x[[k]]) 
    stop("'k=", k, "' but only ", n, " resolution(s) available")
})

#' @export
#' @rdname SpatialData
setMethod("meta", "SpatialDataElement", \(x) x@meta)

# internal use only!
#' @noRd
setReplaceMethod("data", c("SpatialDataElement", "ANY"), 
    \(x, value) { x@data <- value; x })

#' @noRd
setReplaceMethod("meta", c("SpatialDataElement", "SpatialDataAttrs"), 
    \(x, value) { x@meta <- value; x })

#' @noRd
setReplaceMethod("meta", c("SpatialDataElement", "list"), 
    \(x, value) `meta<-`(x, value=SpatialDataAttrs(value)))
# TODO: validity check that .zattrs are valid for 'x'

# row/colnms ----

#' @rdname SpatialData
#' @importFrom BiocGenerics rownames
#' @export
setMethod("rownames", "SpatialData", \(x) .LAYERS)

#' @rdname SpatialData
#' @importFrom BiocGenerics colnames
#' @export
setMethod("colnames", "SpatialData", \(x) {
    names(.) <- . <- rownames(x)
    lapply(., \(.) names(slot(x, .)))
})

# sub ----

.sub_i <- \(x, i) {
    if (isTRUE(i)) return(x)
    if (is.numeric(i) || is.logical(i)) i <- rownames(x)[i]
    if (anyNA(i)) stop("invalid 'i'")
    for (. in setdiff(rownames(x), i)) {
        set <- get(paste0(., "<-"))
        x <- set(x, list())
    }
    x
}
.sub_j <- \(x, j) {
    if (isTRUE(j)) return(x)
    # count number of elements in each layer,
    # and number of layers with any elements
    nl <- sum((ne <- lengths(colnames(x))) > 0)
    if (!is.list(j)) {
        if (nl == 1) j <- list(j)
        if (length(j) == 1) j <- as.list(rep(j, nl))
    }
    if (!isFALSE(j)) stopifnot(length(j) == nl)
    names(j) <- rownames(x)[ne > 0]
    for (. in names(j)) {
        .j <- j[[.]]
        n <- length(x[[.]])
        if (is.character(.j)) {
            if (!all(.j %in% names(x[[.]])))
                stop("invalid 'j'")
        } else if (length(.j) == 1 && is.infinite(.j)) {
            .j <- n
        } else if (any(.j > n)) {
            stop("invalid 'j'")
        }
        x[[.]] <- x[[.]][.j]
    }
    x
}

#' @rdname SpatialData
#' @export
setMethod("[", "SpatialData", \(x, i, j, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE
    x <- .sub_j(.sub_i(x, i), j)
    x <- .sync_tables_on_drop(x)
    x
})

# layer ----

.invalid_i <- paste(
    "invalid 'i'; should be a string or scalar integer",
    "specifying the name or position of an element in 'x'")

#' @rdname SpatialData
#' @export
setMethod("layer", c("SpatialData", "character"), \(x, i) {
    match.arg(i, unlist(colnames(x)))
    names(Filter(\(.) i %in% ., colnames(x)))
})

#' @rdname SpatialData
#' @export
setMethod("layer", c("SpatialData", "ANY"), \(x, i) stop(.invalid_i))

# element ----

#' @export
#' @rdname SpatialData
setMethod("element", c("SpatialData", "character"), 
    \(x, i) slot(x, layer(x, i))[[i]])

#' @export
#' @rdname SpatialData
setMethod("element", c("SpatialData", "numeric"), 
    \(x, i) element(x, unlist(colnames(x))[i]))

#' @export
#' @rdname SpatialData
setMethod("element", c("SpatialData", "missing"), \(x, i) element(x, 1))

#' @export
#' @rdname SpatialData
setMethod("element", c("SpatialData", "ANY"), \(x, i) stop(.invalid_i))

#' @export
#' @rdname SpatialData
setReplaceMethod("element", 
    c("SpatialData", "character"), 
    \(x, i, value) {
        l <- layer(x, i)
        e <- gsub("s$", "", l)
        set <- get(paste0(e, "<-"))
        set(x, i, value=value)
    })

# get all ----

#' @export
#' @rdname SpatialData
setMethod("images", "SpatialData", \(x) x@images)

#' @export
#' @rdname SpatialData
setMethod("labels", "SpatialData", \(object) object@labels)

#' @export
#' @rdname SpatialData
setMethod("points", "SpatialData", \(x) x@points)

#' @export
#' @rdname SpatialData
setMethod("shapes", "SpatialData", \(x) x@shapes)

#' @export
#' @rdname SpatialData
setMethod("tables", "SpatialData", \(x) x@tables)

# get nms ----

all <- paste0(one <- c("image", "label", "point", "shape", "table"), "s")

#' @name SpatialData
#' @exportMethod imageNames labelNames pointNames shapeNames tableNames
NULL

f <- \(e) setMethod(
    paste0(e, "Names"), "SpatialData", 
    \(x) names(slot(x, paste0(e, "s"))))
for (e in one) eval(f(e), parent.env(environment()))

# set nms ----

#' @name SpatialData
#' @exportMethod imageNames<- labelNames<- pointNames<- shapeNames<- tableNames<-
NULL

f <- \(e) setReplaceMethod(
    paste0(e, "Names"),
    c("SpatialData", "character"),
    \(x, value) {
        stopifnot(!any(duplicated(value)), nchar(value) > 0)
        old <- names(x[[paste0(e, "s")]])
        new <- names(x[[paste0(e, "s")]]) <- value
        if (e == "table") return(x)
        .sync_tables_sdattrs(x, old, new)
    })
for (e in one) eval(f(e), parent.env(environment()))

# get one ----

#' @name SpatialData
#' @importFrom BiocGenerics table
#' @exportMethod image label point shape table
NULL

.get <- \(y, i) {
    if (is.numeric(i)) {
        if (i < 1 || !is.finite(i)) stop(
            "invalid 'i'; should be a ",
            "positive integer or string")
        if (i > length(y)) stop(
            "invalid 'i'; only ", length(y), 
            " ", ., " element(s) available")
        i <- names(y)[i]
    }
    if (!i %in% names(y)) stop(
        "invalid 'i'; should be one of: ",
        paste(names(y), collapse=", "))
    y[[i]]
}

.set <- \(e) setMethod(e, "SpatialData", \(x, i=1, ...) .get(slot(x, paste0(e, "s")), i))
for (e in one) eval(.set(e), parent.env(environment()))

# set all ----

#' @name SpatialData
#' @exportMethod images<- labels<- points<- shapes<- tables<-
NULL

# note: core replacement method 
# to be called by layer<- [[<- $<-
f <- \(l) setReplaceMethod(l, 
    c("SpatialData", getSlots("SpatialData")[[l]]), 
    \(x, value) {
        if (l != "tables") {
            old <- names(slot(x, l))
            new <- names(value)
            if (length(old) == length(new) && any(old != new))
                x <- .sync_tables_sdattrs(x, old, new)
        }
        slot(x, l) <- value
        if (l != "tables") {
            x <- .sync_tables_on_drop(x)
        } else {
            for (t in tableNames(x)) {
                x <- .sync_shapes_on_drop(x, t)
            }
        }
        x
    })
for (l in all) eval(f(l), parent.env(environment()))

f <- \(l) setReplaceMethod(l, 
    c("SpatialData", "list"), 
    \(x, value) {
        set <- get(paste0(l, "<-"))
        val <- get(getSlots("SpatialData")[[l]])(value)
        set(x, val)
    })
for (l in all) eval(f(l), parent.env(environment()))

f <- \(l) setReplaceMethod(l, 
    c("SpatialData", "NULL"), 
    \(x, value) {
        set <- get(paste0(l, "<-"))
        set(x, list())
    })
for (l in all) eval(f(l), parent.env(environment()))

# |_[[<- ----

#' @rdname SpatialData
#' @export
setReplaceMethod("[[", c("SpatialData", "numeric"), 
    \(x, i, value) { `[[<-`(x, .LAYERS[i], value) })

#' @rdname SpatialData
#' @export
setReplaceMethod("[[", c("SpatialData", "character"), 
    \(x, i, value) { 
        i <- match.arg(i, .LAYERS)
        set <- get(paste0(i, "<-"))
        set(x, value)
    })

# |_layer<- ----

#' @export
#' @rdname SpatialData
setReplaceMethod("layer", 
    c("SpatialData", "character", "ANY"), 
    \(x, i, value) {
        i <- match.arg(i, .LAYERS)
        set <- get(paste0(i, "<-"))
        set(x, value)
    })

# set one ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

typ <- c(
    image="SpatialDataImage", 
    label="SpatialDataLabel", 
    point="SpatialDataPoint", 
    shape="SpatialDataShape", 
    table="SingleCellExperiment")

f <- \(e) setReplaceMethod(e, 
    c("SpatialData", "character", typ[[e]]), 
    \(x, i, ..., value) {
        y <- slot(x, paste0(e, "s"))
        y[[i]] <- value
        set <- get(paste0(e, "s<-"))
        x <- set(x, y)
        if (e == "table") x <- .sync_shapes_on_drop(x, i)
        return(x)
    })
for (e in one) eval(f(e), parent.env(environment()))

# _i=numeric ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

f <- \(e) setReplaceMethod(e, 
    c("SpatialData", "numeric", typ[[e]]), 
    \(x, i, ..., value) { 
        nms <- get(paste0(e, "Names"))(x)
        n <- length(get(paste0(e, "s"))(x))
        i <- ifelse(i > n, paste0(e, n+1), nms[i])
        set <- get(paste0(e, "<-"))
        set(x, i, value=value)
    })
for (e in one) eval(f(e), parent.env(environment()))

# _i=missing ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

f <- \(e) setReplaceMethod(e, 
    c("SpatialData", "missing", typ[[e]]), 
    \(x, i, ..., value) { 
        set <- get(paste0(e, "<-"))
        set(x, 1, value=value)
    })
for (e in one) eval(f(e), parent.env(environment()))


# _v=NULL ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

f <- \(e) setReplaceMethod(e, 
    c("SpatialData", "ANY", "NULL"), 
    \(x, i, ..., value) {
        if (missing(i)) i <- 1
        l <- paste0(e, "s")
        y <- slot(x, l)
        if (is.numeric(i))
            i <- names(y)[i]
        y <- y[setdiff(names(y), i)]
        x[[l]] <- y
        x
    })
for (e in one) eval(f(e), parent.env(environment()))

# _v=ANY ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

g <- \(e) sprintf("replacement value should be a '%s'", typ[[e]])
f <- \(e) setReplaceMethod(e, 
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, ..., value) stop(g(e)))
for (e in one) eval(f(e), parent.env(environment()))

