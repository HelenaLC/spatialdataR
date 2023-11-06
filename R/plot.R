#' @name plotting
#' @title `SpatialData` plotting
#' @aliases plotImage plotLabel plotShape plotPoint pile
#'
#' @description ...
#'
#' @param x A \code{\link{SpatialData}} object.
#' @param i Scalar integer or character string 
#'   specifying which element to plot.
#' @param coord Scalar integer or character string
#'   specifying the target coordinate system.
#' @param pal Color palette to use.
#' @param fill,alpha Graphical parameters.
#' @param assay Scalar integer or character string
#'   specifying the corresponding `table` data to use.
#' @param width Scalar integer specifying the output's width (in pixel).
#' @param n Scalar integer; for \code{plotShapes} when rendering circles,
#'   specifies the number of vertices to compute for each. If \code{NULL},
#'   will automatically scale inversely with the number of instances.
#' @param ... Additional graphical parameters passed to different methods.
#'
#' @return \code{ggplot2}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' dir <- file.path("extdata", "blobs")
#' dir <- system.file(dir, package="SpatialData")
#' spd <- readSpatialData(dir)
#'
#' # element-wise
#' i <- plotImage(spd)
#' l <- plotLabel(spd)
#' s <- plotShape(spd)
#' p <- plotPoint(spd)
#'
#' # layered
#' pile(i, l, s, p) # or...
#' pile(list(i, l))
NULL

#' @import ggplot2
.plotElement <- function(x, w, h, ...) {
    thm <- list(
        coord_fixed(expand=FALSE),
        scale_y_reverse(limits=h),
        scale_x_continuous(limits=w),
        theme_linedraw(), theme(
            panel.grid=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank()))
    suppressMessages(ggplot() + x + thm)
}

# image ----

.geom <- \(r, wh) {
    annotation_raster(r, 
        wh[[1]][1], wh[[1]][2], 
        -wh[[2]][2], -wh[[2]][1])
}

#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 annotation_raster
#' @export
setMethod("plotImage", "SpatialData",
    function(x, ..., i=1, coord=1, width=400) {
        y <- image(x, i)
        # rescale & transform
        . <- .rescale(y, width, coord)
        z <- .$za; wh <- .$wh
        # render raster image
        a <- as.array(z)
        a <- aperm(a, c(2, 3, 1))
        if (dim(a)[3] == 1) a <- a[, , 1]
        r <- as.raster(a, max(a))
        #geom <- annotation_raster(r, -Inf, Inf, -Inf, Inf)
        .rast <- .geom(r, wh)
        .plotElement(.rast, wh[[1]], rev(wh[[2]]))
    })

# label ----

#' @rdname plotting
#' @importFrom ggplot2 annotation_raster
#' @importFrom SummarizedExperiment assay
#' @importFrom SingleCellExperiment int_metadata
#' @importFrom grDevices as.raster hcl.colors colorRampPalette adjustcolor
#' @export
setMethod("plotLabel", "SpatialData",
    function(x, ..., i=1, coord=1, fill=NULL, assay="X",
        alpha=0.5, pal=hcl.colors(9, "Spectral"), width=NULL) {
        if (!length(labels(x))) stop("no 'label's available")
        if (!is.character(i))
            i <- labelNames(x)[i]
        y <- label(x, i)
        # rescale & transform
        . <- .rescale(y, width, coord)
        z <- .$za; wh <- .$wh
        # render raster image
        l <- NULL
        a <- as.array(z)
        . <- as.list(match.call())
        if (is.null(.$fill)) {
            b <- as.integer(factor(a, exclude=0))
            n <- length(unique(b))
            c <- colorRampPalette(pal)(n)
            c <- matrix(c[b], nrow(a), ncol(a))
        } else if (.is_color(.$fill)) {
            c <- matrix(.$fill, nrow(a), ncol(a))
            c[a == 0] <- NA
        } else if (i %in% tableNames(x)) {
            za <- zattrs(sce <- SpatialData::table(x, i))
            stopifnot(.$fill %in% c(rownames(sce), names(colData(sce))))
            sce <- sce[, sce[[za$region_key]] == i]
            idx <- match(sce[[za$instance_key]], c(a))
            sce <- sce[, !is.na(idx)]
            if (.$fill %in% rownames(sce)) {
                mtx <- assay(sce, assay)
                sce[[.$fill]] <- mtx[.$fill, ]
            }
            val <- sce[[.$fill]]
            foo <- data.frame(foo=val); names(foo) <- .$fill
            foo <- geom_tile(data=foo, alpha=0, aes(Inf, Inf, fill=.data[[.$fill]]))
            if (!is.numeric(val)) {
                n <- nlevels(val <- factor(val))
                pal <- colorRampPalette(pal)(n)
                l <- list(foo, scale_fill_manual(.$fill, values=pal))
            } else {
                val <- cut(val, n <- 100)
                pal <- colorRampPalette(pal)(n)
                l <- list(foo, scale_fill_gradientn(.$fill, colors=pal))
            }
            b <- as.integer(factor(a))
            c <- c(NA, pal[as.integer(val)])
            c <- matrix(c[b], nrow(a), ncol(a))
        }
        # transparency
        r <- as.raster(c)
        r <- adjustcolor(r, alpha)
        r <- matrix(r, nrow(a), ncol(a), byrow=TRUE)
        #geom <- annotation_raster(r, -Inf, Inf, -Inf, Inf)
        geom <- .geom(r, wh)
        .plotElement(c(list(geom), l), wh[[1]], rev(wh[[2]]))
    })

# shape ----

#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 aes geom_polygon
#' @importFrom SummarizedExperiment colData
#' @export
setMethod("plotShape", "SpatialData", \(x, ..., i=1, coord=1, assay="X", n=NULL) {
    if (!is.character(i))
        i <- shapeNames(x)[i]
    y <- shape(x, i)
    y <- transformElement(y, coord)
    df <- switch(y$type[1],
        circle=.df_circle(y, n),
        polygon=.df_polygon(y),
        # TODO: are there more options?
        paste0("shapes of type '", y$type[1], "' not (yet) supported"))
    df$index <- factor(df$index, sort(unique(df$index)))
    # aesthetics
    aes <- aes(.data[["x"]], .data[["y"]], group=.data[["index"]])
    dot <- list(); . <- as.list(match.call())
    for (a in c("col", "fill", "size", "shape", "alpha")) {
        if (!is.null(.[[a]])) {
            stopifnot(length(.[[a]]) == 1)
            if (a %in% c("col", "fill") && is.character(.[[a]])
                && .is_color(.[[a]]) || is.numeric(.[[a]])) {
                dot <- c(dot, .[a])
            } else if (.[[a]] %in% names(df)) {
                aes[[a]] <- aes(.data[[.[[a]]]])[[1]]
            } else if (i %in% tableNames(x)) {
                za <- zattrs(sce <- SpatialData::table(x, i))
                idx <- sce[[za$region_key]] == i
                cd <- data.frame(colData(sce[, idx]))
                cd <- cd[match(df$index, cd$instance_id), ]
                val <- if (.[[a]] %in% names(cd)) {
                    cd[[.[[a]]]]
                } else if (.[[a]] %in% rownames(sce)) {
                    assay(sce, assay)[.[[a]], ]
                }
                if (!is.null(val)) {
                    df[[.[[a]]]] <- val
                    aes[[a]] <- aes(.data[[.[[a]]]])[[1]]
                }
            }
        }
    }
    aes$colour <- aes$col; aes$col <- NULL
    geom <- do.call(geom_polygon, c(list(aes, df), dot))
    .plotElement(geom, range(df$x), rev(range(df$y)))
})

# point ----

#' @rdname plotting
#' @importFrom dplyr filter
#' @import ggplot2
#' @export
setMethod("plotPoint", "SpatialData",
    function(x, ..., i=1, coord=1) {
        y <- point(x, i)
        y <- transformElement(y, coord)
        # filtering
        for (. in .fil(...)) {
            . <- sprintf("filter(y@data, %s)", .)
            fd <- try(eval(parse(text=.)), silent=TRUE)
            if (!inherits(fd, "try-error")) y@data <- fd
        }
        df <- as(y, "data.frame")
        # aesthetics
        aes <- aes(.data[["x"]], .data[["y"]])
        dot <- list(); . <- as.list(match.call())
        for (a in c("col", "size", "alpha", "shape")) {
            if (!is.null(.[[a]])) {
                stopifnot(length(.[[a]]) == 1)
                if (.[[a]] %in% names(df)) {
                    aes[[a]] <- aes(.data[[.[[a]]]])[[1]]
                } else {
                    dot <- c(dot, .[a])
                }
            }
        }
        aes$colour <- aes$col; aes$col <- NULL
        geom <- do.call(geom_point, c(list(aes, df), dot))
        .plotElement(geom, range(df$x), rev(range(df$y)))
    })

# pile ----

# extract axis limits from a 'ggplot'
.wh <- \(p, i) abs(p$scales$scales[[i]]$limits)
.w <- \(p) .wh(p, 2)
.h <- \(p) .wh(p, 1)

# adjust axis limits on arbitrary
# number of 'ggplot's to match 1st
#' @rdname plotting
#' @importFrom ggplot2
#'   scale_y_reverse 
#'   scale_x_continuous
#' @export
setMethod("pile", "list", \(...) { 
    ps <- ..1
    ps <- ps[!vapply(ps, is.null, logical(1))]
    # get shared axis limits
    ws <- vapply(ps, .w, numeric(2))
    hs <- vapply(ps, .h, numeric(2))
    w <- c(min(ws), max(ws))
    h <- c(max(hs), min(hs))
    # use 1st as base & stack others
    gg <- asS3(ps[[1]])
    ps <- ps[-1]
    while (length(ps)) {
        gg <- gg + ps[[1]]$layers
        ps <- ps[-1]
    }
    suppressMessages(gg <- gg +
            scale_y_reverse(limits=h) +
            scale_x_continuous(limits=w))
    return(gg)
})

#' @rdname plotting
#' @export
setMethod("pile", "ANY", \(...) pile(list(...)))

# utils ----

.rescale <- \(x, width, coord) {
    # adjust resolution according to 'width'
    if (is.null(width)) {
        t <- 1; y <- x
    } else {
        d <- dim(x)
        n <- length(d)
        t <- width/d[n]
        if (is(x, "ImageArray")) {
            .t <- c(1, t, t)
        } else {
            .t <- c(t, t)
        }
        y <- scaleElement(x, t=.t)
    }
    # transform & get axiis
    z <- transformElement(y, coord)
    wh <- .whZarrArray(z)
    list(za=z, wh=wh)
}

# check whether character string is a valid color
#' @importFrom grDevices col2rgb
.is_color <- \(.) { !inherits(try(col2rgb(.), silent=TRUE), "try-error") }

# construct table of circle coordinates
# (polygons) given xy-coordinates & radii
#' @importFrom S4Vectors DataFrame
.circles <- \(x, n=NULL) {
    df <- DataFrame(x)
    if (is.null(n)) n <- ifelse(
        (m <- nrow(df)) < 10, 360, 
        round(360 / log10( m )))
    angle <- seq(-pi, pi, length=n)
    .circ <- function(x, y, r, id)
        data.frame(index=id,
            x=x+r*cos(angle),
            y=y+r*sin(angle))
    mapply(.circ, id=df$index,
        x=df$x, y=df$y, r=df$radius,
        SIMPLIFY=FALSE) |> do.call(what=rbind)
}

# unpack 'data.frame' of circle shapes
.df_circle <- \(x, n) {
    a <- as.array(x)
    x$x <- a[, 1]
    x$y <- a[, 2]
    .circles(x, n)
}

# unpack 'data.frame' of polygon shapes
.df_polygon <- \(x) {
    .df <- \(.) data.frame(
        index=.$index[1],
        x=.$data[[1]][, 1], 
        y=.$data[[1]][, 2])
    do.call(rbind, by(x, x$index, .df))
}

.fil <- function(...) {
    fil <- deparse(substitute(c(...)))
    fil <- gsub("c\\((.*)\\)", "\\1", fil)
    fil <- gsub("\"", "'", fil)
    strsplit(fil, ",")[[1]]
}

# .in2or <- function(x) {
#     # 'arrow' filtering currently lacks support for '%in%'
#     # as a work around, convert to a series of '|' statements?
# }
