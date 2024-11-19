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

#' @rdname SpatialData
#' @export
setMethod("[", "SpatialData", \(x, i, j, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE
    i <- if (isFALSE(i)) {
        numeric()
    } else if (isTRUE(i)) {
        seq_along(.LAYERS)
    } else if (is.numeric(i) | is.logical(i)) {
        seq_along(.LAYERS)[i]
    } else if (is.character(i)) {
        i <- match.arg(i, .LAYERS, TRUE)
        which(.LAYERS %in% i)
    }
    if (any(is.na(i))) stop("out of bounds 'i'")
    # TODO: validity
    if (isTRUE(j)) {
        j <- replicate(length(i), TRUE, FALSE)
    } else {
        n <- length(i)
        m <- length(j)
        if (n == 1) {
            if (!is.list(j)) j <- list(j)
        } else {
            if (m > 1 && n == 1) {
                i <- rep(i, m) # recycle 'i'
            } else if (n > 1 && m == 1) {
                j <- rep(j, n) # recycle 'j'
            } else if (n != m) stop("invalid combination of 'i' and 'j'")
        }
    }
    for (. in setdiff(seq_along(.LAYERS), i)) x[[.]] <- list()
    .e <- \(.) stop("out of bounds 'j' for layer ", ., " (", .LAYERS[.], ")")
    for (. in seq_along(i)) {
        j[[.]] <- if (isFALSE(j[[.]])) {
            numeric()
        } else if (isTRUE(j[[.]])) {
            seq_along(x[[i[.]]])
        } else if (is.numeric(j[[.]]) | is.logical(j[[.]])) {
            seq_along(x[[i[.]]])[j[[.]]]
        } else if (is.character(j[[.]])) {
            js <- names(x[[i[.]]])
            if (any(!j[[.]] %in% js)) .e(i[.])
            j[[.]] <- which(js %in% j[[.]])
        }
        js <- seq_along(x[[i[.]]])
        if (!isTRUE(j[[.]])) {
            if (any(!j[[.]] %in% js)) .e(i[.])
            x[[i[.]]] <- x[[i[.]]][j[[.]]]
        }
    }
    return(x)
})

# any ----

#' @rdname SpatialData
#' @export
setMethod("data", "SpatialDataElement", \(x) x@data)

#' @rdname SpatialData
#' @export
setMethod("meta", "SpatialDataElement", \(x) x@meta)

# get all ----

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

all <- paste0(one <- c("image", "label", "point", "shape", "table"), "s")

#' @name SpatialData
#' @exportMethod images labels points shapes tables
f <- \(.) setMethod(., "SpatialData", \(x) x[[.]])
for (. in all) eval(f(.), parent.env(environment()))

# nms ----

#' @name SpatialData
#' @exportMethod imageNames labelNames pointNames shapeNames tableNames
f <- \(.) setMethod(paste0(., "Names"), "SpatialData", \(x) names(x[[.]]))
for (. in one) eval(f(.), parent.env(environment()))

# get one ----

#' @importFrom utils getFromNamespace
.get_ele <- \(x, e, l) {
    l <- match.arg(l, .LAYERS)
    stopifnot(length(e) == 1)
    es <- x[[l]]
    if (!length(es)) 
        return(es)
    if (is.character(e)) {
        e <- match.arg(e, names(es))
    } else stopifnot(
        round(e) == e, 
        e %in% seq_along(es))
    return(x[[l]][[e]])
}

#' @name SpatialData
#' @exportMethod image label point shape table
f <- \(.) setMethod(., "SpatialData", \(x, i=1) .get_ele(x, i, paste0(., "s")))
for (. in one) eval(f(.), parent.env(environment()))

# set all ----

# |_[[<- ----

#' @rdname SpatialData
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
g <- \(.) sprintf("replacement value should be a '%s'", .)
f <- \(.) setReplaceMethod(., 
    c("SpatialData", "ANY", "ANY"), 
    \(x, i, value) stop(g(typ[[.]])))
for (. in one) eval(f(.), parent.env(environment()))
