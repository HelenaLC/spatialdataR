#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 annotation_raster
#' @importFrom SingleCellExperiment int_metadata
setMethod("plotLabel", "SpatialData",
    function(x, ..., i=1, coord=1,
        fill=NULL, alpha=1/3, width=400) {
        y <- label(x, i)
        # down resolution according to 'width'
        if (is.null(width)) {
            t <- 1; z <- y
        } else {
            t <- width/ncol(y)
            z <- scaleElement(y, t=c(t, t))
        }
        z <- transformElement(z, coord)
        # rescale axis limits accordingly
        wh_y <- vapply(.whZarrArray(y), diff, numeric(1))
        wh_z <- vapply(.whZarrArray(z), diff, numeric(1))
        wh <- lapply(.whZarrArray(y), `*`, wh_z/wh_y/t)
        # render raster image
        # TODO: this'll definitely need fixing once I have a
        # suitable test case where coordinates are shifted...
        a <- as.array(y)
        #fill <- "Area"
        w <- dim(a)[2]; h <- dim(a)[1]
        n <- length(unique(c(a)))
        c <- rainbow(n, alpha=alpha)
        c <- matrix(c[a+1], h, w)
        #wh <- list(w, h)
        c[a == 0] <- NA
        r <- as.raster(c)
        # if (is.null(fill)) {
        #     r <- as.raster(a)
        #     r[a == 0] <- NA
        # } else {
        #     sce <- SpatialData::table(x)
        #     md <- int_metadata(sce)$zattrs
        #     cd <- colData(sce)
        #     id <- labelNames(x)[i]
        #     cd <- cd[cd[[md$region_key]] == id, ]
        #     cd <- cd[match(unique(c(a)), cd$cell_ID, nomatch=0), ]
        #     n <- length(unique(c(a)))
        #     val <- cut(cd[, fill], n)
        #     c <- hcl.colors(n, "Blues", alpha=alpha)
        #     names(c) <- levels(val)
        #     c <- matrix(c[val], nrow(a), ncol(a))
        #     c[a == 0] <- NA
        #     r <- as.raster(c)
            # sce <- table(x)
            # cd <- colData(sce)
            # id <- labelNames(x)[i]
            # val <- cd[cd[[md$region_key]] == id, "fill"]
            # val <- cd[[fill]]
            # val <- val/max(val)
            # r <- matrix(val, nrow(a), ncol(a))
            # r[a == 0] <- NA
        geom <- annotation_raster(r, -Inf, Inf, -Inf, Inf)
        .plotElement(geom, wh[[1]], rev(wh[[2]]))
    })
