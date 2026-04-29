#' @importFrom utils .DollarNames
#' @export
.DollarNames.SpatialData <- \(x, pattern="") grep(pattern, .LAYERS, value=TRUE)

#' @rdname SpatialData
#' @exportMethod $
setMethod("$", "SpatialData", \(x, name) attr(x, name))

#' @rdname SpatialData
#' @importFrom methods callNextMethod
#' @export
setMethod("[[", c("SpatialData", "numeric"), \(x, i, ...) {
    i <- .LAYERS[i]
    callNextMethod(x, i)
})

#' @rdname SpatialData
#' @export
setMethod("[[", c("SpatialData", "character"), \(x, i, ...) {
    attr(x, grep(i, names(attributes(x)), value=TRUE))
})

# sub ----

.sub_i <- \(x, i) {
    if (isTRUE(i)) return(x)
    if (is.numeric(i) || is.logical(i)) i <- rownames(x)[i]
    if (anyNA(i)) stop("invalid 'i'")
    for (. in setdiff(rownames(x), i)) attr(x, .) <- list()
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
        n <- length(attr(x, .))
        if (is.character(.j)) {
            if (!all(.j %in% names(attr(x, .))))
                stop("invalid 'j'")
        } else if (length(.j) == 1 && is.infinite(.j)) {
            .j <- n
        } else if (any(.j > n)) {
            stop("invalid 'j'")
        }
        attr(x, .) <- attr(x, .)[.j]
    }
    x
}

#' @rdname SpatialData
#' @export
setMethod("[", "SpatialData", \(x, i, j, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE
    .sub_j(.sub_i(x, i), j)
})

# data/meta ----

#' @rdname SpatialData
#' @export
setMethod("data", "SpatialDataElement", \(x) x@data)

#' @rdname SpatialData
#' @export
setMethod("meta", "SpatialDataElement", \(x) x@meta)

# row/colnms ----

#' @rdname SpatialData
#' @importFrom BiocGenerics rownames
#' @export
setMethod("rownames", "SpatialData", \(x) {
    intersect(names(attributes(x)), .LAYERS)
})

#' @rdname SpatialData
#' @importFrom BiocGenerics colnames
#' @export
setMethod("colnames", "SpatialData", \(x) {
    names(.) <- . <- rownames(x)
    lapply(., \(.) names(x[[.]]))
})

# layer ----

#' @rdname SpatialData
#' @export
setMethod("layer", c("SpatialData", "character"), \(x, i) {
    names(Filter(\(.) i %in% ., colnames(x)))
})

#' @rdname SpatialData
#' @export
setMethod("layer", c("SpatialData", "ANY"), \(x, i) 
    stop("invalid 'i'; should be a string specifying an element in 'x'"))

# element ----

#' @rdname SpatialData
#' @export
setMethod("element", c("SpatialData", "character"), 
    \(x, i) x[[layer(x, i)]][[i]])

#' @rdname SpatialData
#' @export
setMethod("element", c("SpatialData", "numeric"), 
    \(x, i) element(x, unlist(colnames(x))[i]))

#' @rdname SpatialData
#' @export
setMethod("element", c("SpatialData", "missing"), \(x, i) element(x, 1))

#' @rdname SpatialData
#' @export
setMethod("element", c("SpatialData", "ANY"), \(x, i) 
    stop("invalid 'i'; should be a string specifying an element in 'x'"))

# get all ----

all <- paste0(one <- c("image", "label", "point", "shape", "table"), "s")

#' @name SpatialData
#' @exportMethod images labels points shapes tables
NULL

f <- \(.) setMethod(., "SpatialData", \(x) x[[.]])
for (. in all) eval(f(.), parent.env(environment()))

# get nms ----

#' @name SpatialData
#' @exportMethod imageNames labelNames pointNames shapeNames tableNames
NULL

f <- \(.) setMethod(paste0(., "Names"), "SpatialData", \(x) names(x[[.]]))
for (. in one) eval(f(.), parent.env(environment()))

# set nms ----

#' @name SpatialData
#' @exportMethod imageNames<- labelNames<- pointNames<- shapeNames<- tableNames<-
NULL

f <- \(.) setReplaceMethod(
    paste0(., "Names"),
    c("SpatialData", "character"),
    \(x, value) {
        stopifnot(!any(duplicated(value)), nchar(value) > 0)
        old <- names(x[[paste0(., "s")]])
        new <- names(x[[paste0(., "s")]]) <- value
        if (. == "table" || !length(tables(x))) return(x)
        for (i in seq_along(tables(x))) {
            j <- match(region(table(x, i)), old)
            region(table(x, i)) <- new[j]
        }
        return(x)
    })
for (. in one) eval(f(.), parent.env(environment()))

# get one ----

#' @name SpatialData
#' @exportMethod image label point shape table
NULL

f <- \(.) setMethod(., "SpatialData", \(x, i=1) {
    y <- x[[paste0(., "s")]]
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
})
for (. in one) eval(f(.), parent.env(environment()))

# set all ----

# |_[[<- ----

#' @rdname SpatialData
#' @importFrom methods setReplaceMethod
#' @export
setReplaceMethod("[[", c("SpatialData", "numeric"), 
    \(x, i, value) { attr(x, .LAYERS[i]) <- value; return(x) })

#' @rdname SpatialData
#' @export
setReplaceMethod("[[", c("SpatialData", "character"), 
    \(x, i, value) { attr(x, match.arg(i, .LAYERS)) <- value; return(x) })

# |_value=list ----

#' @name SpatialData
#' @exportMethod images<- labels<- points<- shapes<- tables<-
NULL

f <- \(.) setReplaceMethod(., 
    c("SpatialData", "list"), 
    \(x, value) { attr(x, .) <- value; x })
for (. in all) eval(f(.), parent.env(environment()))

# set one ----

typ <- c(
    image="ImageArray", label="LabelArray", 
    point="PointFrame", shape="ShapeFrame", 
    table="SingleCellExperiment")

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

f <- \(.) setReplaceMethod(., 
    c("SpatialData", "character", typ[[.]]), 
    \(x, i, value) {
        y <- attr(x, paste0(., "s"))
        y[[i]] <- value
        attr(x, paste0(., "s")) <- y
        return(x)
    })
for (. in one) eval(f(.), parent.env(environment()))

# TODO: something like table(x)$cluster_id <- doesn't work atm... 
# not sure how to get around without defining all the possible 
# SCE replacement methods :/ 

# _i=numeric ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

f <- \(.) setReplaceMethod(., 
    c("SpatialData", "numeric", typ[[.]]), 
    \(x, i, value) { 
        nms <- get(paste0(., "Names"))(x)
        n <- length(get(paste0(., "s"))(x))
        i <- ifelse(i > n, paste0(., n+1), nms[i])
        get(paste0(., "<-"))(x=x, i=i, value=value)
    })
for (. in one) eval(f(.), parent.env(environment()))

# _i=missing ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

f <- \(.) setReplaceMethod(., 
    c("SpatialData", "missing", typ[[.]]), 
    \(x, i, value) { 
        f <- get(paste0(., "<-"))
        f(x=x, i=1, value=value)
})
for (. in one) eval(f(.), parent.env(environment()))

# _v=NULL ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

f <- \(.) setReplaceMethod(., 
    c("SpatialData", "ANY", "NULL"), 
    \(x, i, value) {
        if (missing(i)) i <- 1
        y <- attr(x, .)
        if (is.numeric(i))
            i <- names(y)[i]
        y <- y[setdiff(names(y), i)]
        x[[.]] <- y
        x
    })
for (. in one) eval(f(.), parent.env(environment()))

# _v=ANY ----

#' @name SpatialData
#' @exportMethod image<- label<- point<- shape<- table<-
NULL

g <- \(.) sprintf("replacement value should be a '%s'", .)
f <- \(.) setReplaceMethod(., 
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) stop(g(typ[[.]])))
for (. in one) eval(f(.), parent.env(environment()))
