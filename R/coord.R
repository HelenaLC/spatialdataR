# TODO: currently applying transformations only on 'data.frame's for plotting,
# not the actual data (e.g., image)... but this might be necessary for queries?

# TODO: for all layers, implement all transformations 
# (translate, scale, rotate, affine, and sequential)

#' @name .coord2graph
#' @rdname coord2graph
#' @title CS graph representation
#' 
#' @param x \code{SpatialData} object
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' # element-wise
#' g <- SpatialData:::.coord2graph(image(x))
#' SpatialData:::.get_path(g, "self", "global")
#' 
#' # object-wide
#' g <- SpatialData:::.coord2graph(x)
#' graphics.off(); graph::plot(g)
#' # retrieve transformation from element to target space
#' graph::edgeData(g, "blobs_labels", "translation", "data")
#' 
#' @importFrom graph graphAM nodes
#'   addNode nodeData<- nodeDataDefaults<-
#'   addEdge edgeData<- edgeDataDefaults<-
.coord2graph <- \(x) {
    # initialize empty directed graph with node & edge attributes
    g <- graphAM(edgemode="directed")
    edgeDataDefaults(g, "data") <- list()
    edgeDataDefaults(g, "type") <- character()
    nodeDataDefaults(g, "type") <- character()
    names(ls) <- ls <- setdiff(.LAYERS, "tables")
    .md <- if (!is(x, "SpatialData")) {
        list("mock"=list("self"=meta(x)))
    } else lapply(ls, \(l) {
        names(es) <- es <- names(x[[l]])
        lapply(es, \(e) meta(x[[l]][[e]]))
    })
    for (l in names(.md)) for (e in names(.md[[l]])) {
        md <- .md[[l]][[e]]
        ms <- md$multiscales
        if (!is.null(ms)) md <- ms
        ct <- md$coordinateTransformations
        ct <- if (length(ct) == 1) ct[[1]] else ct
        g <- addNode(e, g)
        nodeData(g, e, "type") <- "element"
        for (i in seq(nrow(ct))) {
            n <- ct$output$name[i]
            if (!n %in% nodes(g)) {
                g <- addNode(n, g)
                nodeData(g, n, "type") <- "space"
            }
            t <- ct$type[i]
            if (t == "sequence") {
                sq <- ct$transformations[i][[1]]
                . <- e
                for (j in seq(nrow(sq))) {
                    if (j == nrow(sq)) {
                        m <- n
                    } else {
                        m <- paste(e, n, j, sep="_")
                        g <- addNode(m, g)
                        nodeData(g, m, "type") <- "none"
                    }
                    t <- sq$type[j]
                    d <- sq[[t]][j]
                    g <- addEdge(., m, g)
                    edgeData(g, ., m, "type") <- t
                    edgeData(g, ., m, "data") <- d
                    . <- m
                }
            } else {
                g <- addEdge(e, n, g)
                d <- ct[[ct$type[i]]][i]
                edgeData(g, e, n, "type") <- t
                edgeData(g, e, n, "data") <- d
            }
        }
    }
    return(g)
}

#' @name .get_path
#' @rdname get_path
#' @title get transformations path
#' 
#' @param g \code{\link[graph]{graphAM}}
#' @param i,j source and target node label
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' g <- SpatialData:::.coord2graph(x)
#' SpatialData:::.get_path(g, "blobs_labels", "sequence")
#' 
#' @importFrom graph edgeData
#' @importFrom RBGL sp.between
.get_path <- \(g, i, j) {
    p <- sp.between(g, i, j)
    p <- p[[1]]$path_detail
    n <- length(p)-1
    lapply(seq_len(n), \(.)
        edgeData(g, p[.], p[.+1])[[1]])
}

#' @name coord
#' @title Coordinate transformations
#' @aliases rmvCT
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#'
#' # view available coordinate transformations
#' coordTransData(z <- meta(label(x)))
#'
#' # add
#' addCT(z, "scale", "scale", c(12, 34)) # can't overwrite
#' coordTransData(addCT(z, "new", "translation", c(12, 34)))
#' 
#' # rmv
#' coordTransData(rmvCT(z, 2)) # by index
#' coordTransData(rmvCT(z, "scale")) # by name
#' coordTransData(rmvCT(z, 1)) # identity is protected
NULL

# rmv ----

setGeneric("rmvCT", \(x, ...) standardGeneric("rmvCT"))

#' @rdname coord
#' @export
setMethod("rmvCT", "SpatialDataElement", 
    \(x, i) { x@meta <- rmvCT(meta(x), i); x })

#' @rdname coord
#' @export
setMethod("rmvCT", "Zattrs", \(x, i) {
    nms <- coordTransName(x)
    if (is.numeric(i)) i <- nms[i]
    nan <- setdiff(i, nms)
    if (length(nan)) stop(
        "couln't find 'coordTrans' of name(s) ", 
        paste(dQuote(nan), collapse=","))
    # prevent against dropping identity
    i <- match(i, nms, nomatch=0)
    i <- i[coordTransType(x)[i] != "identity"]
    ms <- "multiscales"
    ct <- "coordinateTransformations"
    if (length(i)) {
        # utility to drop empty columns
        j <- \(.) vapply(., \(.) !is.null(unlist(.)), logical(1))
        if (!is.null(x[[ms]])) {
            y <- x[[ms]][[ct]][[1]][-i, ]
            x[[ms]][[ct]][[1]] <- y[, j(y)]
        } else {
            y <- x[[ct]][-i, ]
            x[[ct]] <- y[, j(y)]
        }
    }
    return(x)
})

# add ----

setGeneric("addCT", \(x, ...) standardGeneric("addCT"))

#' @rdname coord
#' @export
setMethod("addCT", "SpatialDataElement", \(x, name, type, data) {
    x@meta <- addCT(meta(x), name, type, data); x })

#' @rdname coord
#' @export
setMethod("addCT", "Zattrs", \(x, name, type="identity", data=NULL) {
    stopifnot(
        is.character(name), length(name) == 1,
        is.character(type), length(type) == 1)
    type <- match.arg(type, c("scale", "rotate", "translation", "affine"))
    ms <- "multiscales"; ts <- "transformations"; ct <- "coordinateTransformations"
    if (!is.null(x[[ms]])) {
        # use existing as skeleton
        fd <- (df <- coordTransData(x))[1, ]
        fd <- fd[, c("input", "output", "type")]
        fd$type <- type
        fd$output$name <- name
        fd[[fd$type]] <- list(data)
        # append to existing if 'name' already present 
        idx <- match(name, coordTransName(x))
        typ <- coordTransType(x)[idx]
        if (!is.na(typ) && typ == "identity") {
            df <- df[0, ]
            app <- FALSE
        } else if (app <- !is.na(idx)) {
            if (seq <- (typ == "sequence")) {
                df <- df[idx, ][[ts]][[1]]
                fd$output$name <- df$output$name[1]
            } else {
                df <- df[idx, ]
                if (is.null(df[[ts]])) {
                    
                } else {
                    df[[ts]][[1]] <- df
                }
                # fd$type <- type
                # fd[[fd$type]] <- list(data)
            }
        } else {
            # fd$type <- type
            # fd[[fd$type]] <- list(data)
        }
        na <- setdiff(names(df), names(fd))
        for (. in na) fd[[.]] <- list(NULL)
        na <- setdiff(names(fd), names(df))
        for (. in na) df[[.]] <- if (nrow(df) > 0) list(NULL) else list()
        fd <- fd[, names(col) <- col <- names(df)]
        # combine
        rownames(fd$input) <- rownames(fd$output) <- nrow(df)+1
        new <- rbind(df, fd)
        if (app) {
            x[[ms]][[ct]][[1]]$type[idx] <- "sequence"
            x[[ms]][[ct]][[1]][idx, ]$transformations[[1]] <- new
        } else {
            x[[ms]][[ct]] <- new
        }
    }
    return(x)
})

setGeneric("setCS", \(x, ...) standardGeneric("setCS"))
setGeneric("setTS", \(x, ...) standardGeneric("setTS"))

# TODO: check the validity of this function
setMethod("setCS", "SpatialDataElement", \(x, c) {
    ms <- (md <- meta(x))$multiscales
    if (!is.null(ms)){
        cs <- md$coordinateTransformations
        if (length(cs) == 1) {
            md$multiscales$coordinateTransformations <- c
        } else{
            md$multiscales$coordinateTransformations[[1]] <- c
        }
    }
    md
})

# transformations
setMethod("setTS", "SpatialDataElement", \(x, i=1, t) {
    y <- getCS(x)
    if (is.character(i)) 
        i <- which(y$output$name == i)
    y[i, ]$transformations[[1]] <- t
    x@meta <- Zattrs(setCS(x,y))
    x
})

#' #' @importFrom EBImage resize
#' setMethod("scale", "ImageArray", \(x, t, ...) {
#'     a <- as.array(data(x)) 
#'     # TODO: this should be done w/o realizing 
#'     # into memory, but EBImage needs an array?
#'     d <- length(dim(a))
#'     if (missing(t)) 
#'         t <- rep(1, d)
#'     b <- resize(aperm(a),
#'         w=dim(a)[d]*t[d],
#'         h=dim(a)[d-1]*t[d-1])
#'     x@data <- aperm(b)
#'     x
#' })
#' 
#' #' @importFrom EBImage resize
#' setMethod("translation", "ImageArray", \(x, t, ...) {})
#' setMethod("transform", "ImageArray", \(x, t) get(t$type)(x, unlist(t[[t$type]])))
