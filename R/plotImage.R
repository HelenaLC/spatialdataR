#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 annotation_raster
#' @export
setMethod("plotImage", "SpatialData",
    function(x, ..., i=1, coord=1, width=400) {
        y <- image(x, i)
        # down resolution according to 'width'
        if (is.null(width)) {
            t <- 1; z <- y
        } else{
            t <- width/dim(y)[2]
            z <- scaleElement(y, t=c(1, t, t))
        }
        z <- transformElement(z, coord)
        # # rescale axis limits accordingly
        # wh <- .whZarrArray(y)
        # wh_z <- .whZarrArray(z)
        # rng_y <- vapply(wh_y, diff, numeric(1))
        # rng_z <- vapply(wh_z, diff, numeric(1))
        # asp <- rng_y/rng_z/t
        # wh <- lapply(seq(2), \(.) wh_z[[.]]*asp[.])
        wh <- wh_y
        # render raster image
        a <- as.array(z)
        a <- aperm(a, c(2, 3, 1))
        if (dim(a)[3] == 1) a <- a[, , 1]
        r <- as.raster(a, max(a))
        geom <- annotation_raster(r, -Inf, Inf, -Inf, Inf)
        .plotElement(geom, wh[[1]], rev(wh[[2]]))
    })
