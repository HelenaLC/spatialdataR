#' @name coord-utils
#' @title Coordinate transformations
#' @aliases axes CTname CTtype CTdata CTpath CTgraph addCT rmvCT
#' 
#' @param x \code{SpatialData}, an element, or \code{Zattrs}.
#' @param i for \code{CTpath}, source node label; else, string or 
#'   scalar integer giving the name or index of a coordinate space.
#' @param j character string; name of target coordinate space.
#' @param name character(1); name of coordinate space
#' @param type character(1); type of transformation
#' @param data transformation data; size and shape depend on transformation and
#'   element type (e.g., numeric(1) for rotation, numeric(2) for scaling in 2D)
#' @param ... option arguments passed to and from other methods.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' # element-wise
#' g <- CTgraph(y <- image(x))
#' graph::nodes(g)
#' CTpath(y, "global")
#' 
#' # object-wide
#' g <- CTgraph(x)
#' plotCoordGraph(g)
#' 
#' # retrieve transformation from element to target space
#' CTpath(x, "blobs_labels", "sequence")
#' 
#' # view available coordinate transformations
#' CTdata(z <- meta(label(x)))
#'
#' # add
#' addCT(z, "scale", "scale", c(12, 34)) # can't overwrite
#' CTdata(addCT(z, "new", "translation", c(12, 34)))
#' 
#' # rmv
#' CTdata(rmvCT(z, 2)) # by index
#' CTdata(rmvCT(z, "scale")) # by name
#' CTdata(rmvCT(z, 1)) # identity is protected
NULL

# TODO: currently applying transformations only on 'data.frame's for plotting,
# not the actual data (e.g., image)... but this might be necessary for queries?

# TODO: for all layers, implement all transformations 
# (translate, scale, rotate, affine, and sequential)

# axes() ----

#' @rdname coord-utils
#' @export
setMethod("axes", "Zattrs", \(x, ...) {
    if (!is.null(ms <- x$multiscales)) x <- ms
    if (is.null(x <- x$axes)) stop("couln't find 'axes'") 
    if (is.character(x)) x else x[[1]]
})

#' @rdname coord-utils
#' @export
setMethod("axes", "SpatialDataElement", \(x, ...) axes(meta(x)))

# CTdata/type/name() ----

#' @rdname coord-utils
#' @export
setMethod("CTdata", "Zattrs", \(x, ...) {
    ms <- x$multiscales
    if (!is.null(ms)) x <- ms
    x <- x$coordinateTransformations
    if (is.null(dim(x))) x[[1]] else x
})

#' @rdname coord-utils
#' @export
setMethod("CTdata", "SpatialDataElement", \(x, ...) CTdata(meta(x)))

#' @rdname coord-utils
#' @export
setMethod("CTtype", "Zattrs", \(x, ...) CTdata(x)$type)

#' @rdname coord-utils
#' @export
setMethod("CTtype", "SpatialDataElement", \(x, ...) CTtype(meta(x)))

#' @rdname coord-utils
#' @export
setMethod("CTname", "Zattrs", \(x, ...) CTdata(x)$output$name)

#' @rdname coord-utils
#' @export
setMethod("CTname", "SpatialDataElement", \(x, ...) CTname(meta(x)))

#' @rdname coord-utils
#' @export
setMethod("CTname", "SpatialData", \(x, ...) {
    g <- CTgraph(x)
    t <- nodeData(g, nodes(g), "type")
    names(t)[unlist(t) == "space"]
})

# CTgraph() ----

#' @rdname coord-utils
#' @export
setMethod("CTgraph", "SpatialData", \(x) {
    names(ls) <- ls <- setdiff(.LAYERS, "tables")
    md <- lapply(ls, \(l) {
        names(es) <- es <- names(x[[l]])
        lapply(es, \(e) meta(x[[l]][[e]]))
    })
    .make_g(md)
})

#' @rdname coord-utils
#' @export
setMethod("CTgraph", "SpatialDataElement", \(x) 
    .make_g(list("mock"=list("self"=meta(x)))))

#' @rdname coord-utils
#' @export
setMethod("CTgraph", "ANY", \(x) stop("'x' should be a", 
    " 'SpatialData' object, or a non-'table' element"))

#' @importFrom graph graphAM nodeDataDefaults<- edgeDataDefaults<-
.init_g <- \() {
    g <- graphAM(edgemode="directed")
    edgeDataDefaults(g, "data") <- list()
    edgeDataDefaults(g, "type") <- character()
    nodeDataDefaults(g, "type") <- character()
    return(g)
}

#' @importFrom graph nodes addNode addEdge nodeData<- edgeData<- 
.make_g <- \(md) {
    g <- .init_g()
    for (l in names(md)) for (e in names(md[[l]])) {
        .md <- md[[l]][[e]]
        ms <- .md$multiscales
        if (!is.null(ms)) .md <- ms
        ct <- .md$coordinateTransformations
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

# CTpath() ----

#' @rdname coord-utils
#' @export
setMethod("CTpath", "SpatialData", \(x, i, j) {
    g <- CTgraph(x)
    .path_ij(g, i, j)
})

#' @rdname coord-utils
#' @export
setMethod("CTpath", "SpatialDataElement", \(x, j) {
    g <- CTgraph(x)
    .path_ij(g, "self", j)
})

#' @importFrom graph edgeData
#' @importFrom RBGL sp.between
.path_ij <- \(g, i, j) {
    p <- sp.between(g, i, j)
    p <- p[[1]]$path_detail
    n <- length(p)-1
    lapply(seq_len(n), \(.)
        edgeData(g, p[.], p[.+1])[[1]])
}

# rmv ----

#' @rdname coord-utils
#' @export
setMethod("rmvCT", "SpatialDataElement", 
    \(x, i) { x@meta <- rmvCT(meta(x), i); x })

#' @rdname coord-utils
#' @export
setMethod("rmvCT", "Zattrs", \(x, i) {
    nms <- CTname(x)
    if (is.numeric(i)) {
        if (any(i > length(nms)))
            stop("invalid 'i'")
        i <- nms[i]
    }
    nan <- setdiff(i, nms)
    if (length(nan)) stop(
        "couln't find 'coordTrans' of name(s) ", 
        paste(dQuote(nan), collapse=","))
    i <- match(i, nms)
    # # prevent against dropping identity
    # i <- i[CTtype(x)[i] != "identity"]
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

#' @rdname coord-utils
#' @export
setMethod("addCT", "SpatialDataElement", \(x, name, type, data) {
    x@meta <- addCT(meta(x), name, type, data); x })

.check_ct <- \(x, type, data) {
    d <- ifelse(is.character(a <- axes(x)), length(a), nrow(a))
    f <- \(t) stop("invalid 'data' for transformation of 'type' ", dQuote(t))
    t <- match.arg(type, c("identity", "scale", "rotate", "translation", "affine"))
    . <- switch(t, 
        identity=is.null(data),
        translation=length(data) == d & is.numeric(data),
        rotate=length(data) == 1 & is.numeric(data) & data > 0,
        scale=length(data) == d & is.numeric(data) & all(data > 0),
        TRUE)
    if (!.) f(t)
}

#' @rdname coord-utils
#' @export
setMethod("addCT", "Zattrs", \(x, name, type="identity", data=NULL) {
    stopifnot(
        is.character(name), length(name) == 1,
        is.character(type), length(type) == 1)
    .check_ct(x, type, data)
    ms <- "multiscales"
    ts <- "transformations"
    ct <- "coordinateTransformations"
    # use existing as skeleton
    fd <- (df <- CTdata(x))[1, ]
    fd <- fd[, c("input", "output", "type")]
    fd$type <- type
    fd$output$name <- name
    fd[[fd$type]] <- list(data)
    # append to existing if 'name' already present 
    idx <- match(name, CTname(x))
    typ <- CTtype(x)[idx]
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
    if (app && !seq) {
        # append to other
        rownames(df) <- rownames(df$input) <- rownames(df$output) <- 1
        rownames(fd) <- rownames(fd$input) <- rownames(fd$output) <- 2
    } else {
        # append to table or sequence
        rownames(fd$input) <- rownames(fd$output) <- nrow(df)+1
    }
    new <- rbind(df, fd)
    if (is_ms <- !is.null(x[[ms]])) {
        .x <- x[[ms]][[ct]][[1]]
    } else .x <- x[[ct]]
    if (app) {
        .x$type[idx] <- "sequence"
        .x[idx, ]$transformations[[1]] <- new
    } else .x <- new
    if (is_ms) x[[ms]][[ct]][[1]] <- .x else x[[ct]] <- .x
    return(x)
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

# plotCoordGraph() ----

#' @name plotCoordGraph
#' @title CT graph viz.
#' @description
#' given a \code{graphNEL} instance, nodes with \code{nchar>max} are
#' split and hyphenated at character position \code{floor(nchar/fac)}.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' g <- CTgraph(x)
#' plotCoordGraph(g, cex=0.6)
#' 
#' @importFrom graph nodes nodes<- graph.par
#' @importFrom Rgraphviz layoutGraph renderGraph
#' @export
plotCoordGraph <- \(g, cex=0.6) {
    g <- CTgraph(x)
    g2view <- g # leave 'g' alone
    nodes(g2view) <- .nodefix(nodes(g2view))
    graph.par(list(nodes=list(shape="plaintext", cex=cex)))
    g2view <- layoutGraph(g2view)
    renderGraph(g2view)
}

.nodefix <- \(x, fac=2, max=10) {
    if (any((nc <- nchar(x)) > max))
        x[nc > max] <- .fixup(x[nc > max], fac)
    x
}

.fixup <- \(x, fac) {
    nc <- nchar(x)
    hm <- floor(nc/fac)
    xs <- strsplit(x, "")
    xu <- lapply(seq_along(xs), \(i) c(
        xs[[i]][seq_len(hm[i])], "-\n", 
        xs[[i]][-seq_len(hm[i])]))
    xu <- lapply(xu, \(z) paste(z, collapse=""))
    unlist(xu)
}
