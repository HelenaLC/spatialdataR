#' @name sd_plot_image
#' @title Plot `ImageArray`
#' 
#' @param x \code{\link{SpatialData}} object.
#' @param i scalar integer or string; 
#'   specifies which \code{images} to plot.
#' @param c character vector of colors to use;
#'   if NULL (default), using RGB.
#' @param ch integer or character vector
#'   specifying which channels to render.
#' @param cl list of channel-wise contrast limits.
#' @param k resolution; if NULL (default), picking 
#'   best for given \code{w}idth and \code{h}eight.
#' @param w,h render width and height in pixel.
#' 
#' @return \code{ggplot}
#' 
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' sd <- readSpatialData(pa)
#' 
#' sd_plot() + sd_plot_image(sd)
#' 
#' # custom colors
#' pal <- c("cyan", "magenta", "gold")
#' sd_plot() + sd_plot_image(sd, c=pal)
#' 
#' @importFrom methods as
#' @importFrom grDevices rgb
#' @importFrom DelayedArray realize
#' @export
sd_plot_image <- \(x, i=1, c=NULL, ch=NULL, cl=NULL, k=NULL, w=800, h=800) {
    #x <- sd; i <- 1; w=h=800; ch <- cl <- c <- k <- NULL; ch <- 2
    ia <- x@images[[i]]
    ch <- .ch_idx(ia, ch)
    if (is.null(k)) 
        k <- .guess_scale(ia, w, h)
    a <- data(ia, k)
    if (!.is_rgb(ia))
        a <- a[ch, , , drop=FALSE]
    dt <- .get_dt(ia)
    a <- as(a, "DelayedArray")
    a <- .norm_ia(realize(a), dt)
    # enter when image isn't RGB already, either
    # custom colors or contrasts are specified
    if (!.is_rgb(ia) || !is.null(c) || !is.null(cl))
        a <- .chs2rgb(a, ch, c, cl)
    a <- apply(a, c(2, 3), \(.) do.call(rgb, as.list(.))) 
    w <- c(0, dim(ia)[3])
    h <- c(0, dim(ia)[2])
    pal <- if (!.is_rgb(ia) && dim(ia)[1] > 1) {
        nms <- channels(ia)[ch]
        pal <- if (is.null(c)) .DEFAULT_COLORS else c
        `names<-`(pal[seq_along(ch)], nms)
    }
    lgd <- if (!is.null(pal)) list(
        guides(col=guide_legend(override.aes=list(alpha=1, size=2))),
        scale_color_identity(NULL, guide="legend", labels=names(pal)),
        geom_point(aes(col=.data$foo), data.frame(foo=pal), x=0, y=0, alpha=0))
    list(lgd,
        scale_x_continuous(limits=w), scale_y_reverse(limits=rev(h)),
        annotation_raster(a, w[2],w[1], h[1],h[2], interpolate=FALSE))
}

# default colors (from ImageJ/Fiji)
.DEFAULT_COLORS <- c("red", "green", "blue", "gray", "cyan", "magenta", "yellow")

# image data type factors (max values)
# TODO: add more cases from other data types
# https://doc.embedded-wizard.de/uint-type
.DTYPE_MAX_VALUES <- c(
    "uint8"=2**8-1, "uint16"=2**16-1, 
    "uint32"=2**32-1, "uint64"=2**64-1)

.ch_idx <- \(x, ch) {
    if (is.null(ch))
        return(1)
    lbs <- channels(x)
    if (is.integer(ch)) {
        return(lbs[ch])
    } else if (all(ch %in% lbs)) {
        return(match(ch, lbs))
    } else if (!any(ch %in% lbs)) {
        warning("Couldn't find some channels; picking first one(s)!")
        return(1)
    } else {
        warning("Couldn't find channels; picking first one(s)!")
        return(1)
    }
    return(NULL)
}

.check_cl <- \(cl, d) {
    if (is.null(cl)) {
        # default to [0, 1] for all channels
        cl <- replicate(d, c(0, 1), FALSE)
    } else {
        # should be a list with as many elements as channels
        if (!is.list(cl)) stop("'cl' should be a list")
        if (length(cl) != d) stop("'cl' should be of length ", d)
        for (. in seq_len(d)) {
            # replace NULL by [0, 1] & n by [0, n]
            if (is.null(cl[[.]])) cl[[.]] <- c(0, 1)
            if (length(cl[[.]]) == 1) {
                if (cl[[.]] < 0) stop("scalar 'cl' can't be < 0")
                cl[[.]] <- c(0, cl[[.]])
            }
        }
        # elements should be length-2, numeric, non-negative, increasing
        .f <- \(.) length(.) == 2 && is.numeric(.) && all(. >= 0) && .[2] > .[1]
        if (!all(vapply(cl, .f, logical(1))))
            stop("elements of 'cl' should be length-2,",
                " non-negative, increasing numeric vectors")
    }
    return(cl)
}

# merge/manage image channels;
# if no colors and channels defined, 
# return the first channel
#' @importFrom grDevices col2rgb
.chs2rgb <- \(a, ch, c=NULL, cl=NULL) {
    cl <- .check_cl(cl, d <- dim(a)[1])
    if (length(ch) > (n <- length(.DEFAULT_COLORS)) && is.null(c))
        stop("Only ", n, " default colors available, but",
            length(ch), " are needed; please specify 'c'")
    if (!is.null(c) || (is.null(c) && length(ch) > 1)) {
        if (is.null(c)) c <- .DEFAULT_COLORS[seq_along(ch)] 
        c <- col2rgb(c)/255
        b <- array(0, dim=c(3, dim(a)[-1]))
        for (i in seq_len(d)) {
            for (j in seq_len(3)) {
                rgb <- a[i,,,drop=FALSE]*c[j,i]
                # apply upper contrast lim.
                rgb <- rgb*(1/cl[[i]][2]) 
                b[j,,] <- b[j,,,drop=FALSE] + rgb
                # apply lower contrast lim.
                b[j,,][b[j,,] < cl[[i]][1]] <- 0
            }
        }
        a <- pmin(b, 1)
    } else {
        a <- a[rep(1, 3), , ]
    }
    return(a)
}

.guess_scale <- \(x, w, h) {
    n <- length(dim(x))
    i <- ifelse(n == 3, -1, TRUE)
    d <- vapply(x@data, dim, numeric(n))
    d <- apply(d, 2, \(.) sum(abs(.[i]-c(h, w))))
    which.min(d)
}

# check if an image is rgb or not
#' @importFrom Rarr zarr_overview
#' @noRd
.get_dt <- \(x) {
    pa <- x@data[[1]]@seed@zarr_array_path
    df <- zarr_overview(pa, as_data_frame=TRUE)
    if (!is.null(dt <- df$data_type)) return(dt)
}

.norm_ia <- \(a, dt) {
    d <- dim(a)[1]
    if (dt %in% names(.DTYPE_MAX_VALUES)) {
        a <- a / .DTYPE_MAX_VALUES[dt]
    } else if (max(a) > 1) {
        for (i in seq_len(d))
            a[i,,] <- a[i,,] / max(a[i,,])
    }
    return(a)
}

# check if an image is RGB or not
# (NOTE: some RGB channels are named 0, 1, 2)
#' @importFrom S7 S7_inherits
.is_rgb <- \(x) {
    stopifnot("invalid 'x'"=S7_inherits(x, ImageArray) && !is.null(zattrs(x)))
    x <- channels(x)
    is_len <- length(x) == 3
    is_012 <- setequal(x, seq(0, 2))
    is_rgb <- setequal(x, c("r", "g", "b"))
    return(is_len && (is_012 || is_rgb))
}
