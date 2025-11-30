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
    if (any(is.na(i))) stop("invalid 'i'")
    for (. in setdiff(rownames(x), i)) attr(x, .) <- list()
    x
}
.sub_j <- \(x, j) {
    if (isTRUE(j)) return(x)
    # count number of elements in each layer,
    # and number of layers with any elements
    nl <- sum((ne <- vapply(colnames(x), length, numeric(1))) > 0)
    if (!is.list(j)) {
        if (nl == 1) j <- list(j)
        if (length(j) == 1) j <- as.list(rep(j, nl))
    }
    if (!isFALSE(j)) stopifnot(length(j) == nl)
    names(j) <- rownames(x)[ne > 0]
    for (. in names(j)) {
        .j <- j[[.]]
        n <- length(attr(x, .))
        if (length(.j) == 1 && is.infinite(.j)) {
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

.err_i <- c(
    "invalid 'i'; should be an integer in [1, 5], or a ",
    "string in ", dQuote(paste(.LAYERS, collapse="/"))) 

#' @rdname SpatialData
#' @export
setMethod("layer", c("SpatialData", "character"), 
    \(x, i) attr(x, match.arg(i, .LAYERS, TRUE)))

#' @rdname SpatialData
#' @export
setMethod("layer", c("SpatialData", "numeric"), \(x, i) {
    ok <- length(i) == 1 && (i > 0 & i < 6 & i == round(i))
    if (!ok) stop(.err_i)
    attr(x, .LAYERS[i])
})

#' @rdname SpatialData
#' @export
setMethod("layer", c("SpatialData", "missing"), \(x, i) layer(x, 1))

#' @rdname SpatialData
#' @export
setMethod("layer", c("SpatialData", "ANY"), \(x, i) stop(.err_i))

# element ----

.err_j <- c(
    "invalid 'j'; should be a scalar integer or ",
    "a string specifying an element in layer 'i'") 

#' @rdname SpatialData
#' @export
setMethod("element", c("SpatialData", "ANY", "character"), \(x, i, j) {
    y <- layer(x, i)
    j <- match.arg(j, names(y))
    y[[j]]
})

#' @rdname SpatialData
#' @export
setMethod("element", c("SpatialData", "ANY", "numeric"), \(x, i, j) {
    n <- length(y <- layer(x, i))
    if (n == 0) stop("there aren't any ", dQuote(i))
    if (is.infinite(j)) j <- n
    ok <- length(j) == 1 && (j > 0 & j <= n & j == round(j))
    if (!ok) stop(.err_j)
    j <- names(y)[j]
    element(x, i, j)
})

#' @rdname SpatialData
#' @export
setMethod("element", c("SpatialData", "ANY", "missing"), \(x, i, j) element(x, i, 1))

#' @rdname SpatialData
#' @export
setMethod("element", c("SpatialData", "ANY", "ANY"), \(x, i, j) stop(.err_j))

# get all ----

all <- paste0(one <- c("image", "label", "point", "shape", "table"), "s")

#' @name SpatialData
#' @exportMethod images labels points shapes tables
NULL

f <- \(.) setMethod(., "SpatialData", \(x) x[[.]])
for (. in all) eval(f(.), parent.env(environment()))

# nms ----

#' @name SpatialData
#' @exportMethod imageNames labelNames pointNames shapeNames tableNames
NULL

f <- \(.) setMethod(paste0(., "Names"), "SpatialData", \(x) names(x[[.]]))
for (. in one) eval(f(.), parent.env(environment()))

# get one ----

#' @name SpatialData
#' @exportMethod image label point shape table
NULL

f <- \(.) setMethod(., "SpatialData", \(x, i=1) element(x, paste0(., "s"), i))
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
