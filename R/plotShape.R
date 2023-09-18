#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 aes geom_polygon
#' @export
setMethod("plotShape", "SpatialData",
    function(x, ..., i=1, coord=NULL) {
        y <- shape(x, i)
        y <- transformElement(y, coord)
        df <- switch(y$type[1],
            circle=.df_circle(y),
            polygon=.df_polygon(y),
            # TODO: are there more options?
            paste0("shapes of type '", y$type[1], "' not (yet) supported"))
        df$id <- factor(df$id, sort(unique(df$id)))
        geom <- geom_polygon(data=df,
            aes(x, y, group=id, ...),
            show.legend = FALSE)
        xlim <- range(df$x)
        ylim <- rev(range(df$y))
        .plotElement(geom, xlim, ylim)
    })

.df_circle <- \(x) {
    a <- as.array(x)
    x$x <- a[, 1]
    x$y <- a[, 2]
    df <- .circles(x)
    return(df)
}

.df_polygon <- \(x) {
    .df <- \(.)
    data.frame(
        id=.$index[1],
        x=.$data[[1]][, 1],
        y=.$data[[1]][, 2])
    lys <- by(x, x$index, .df)
    df <- do.call(rbind, lys)
    return(df)
}

# construct table of circle coordinates
# (polygons) given xy-coordinates & radii
.circles <- function(df, n=360){
    angle <- seq(-pi, pi, length=n)
    .circ <- function(x, y, r, id)
        data.frame(id,
            x=x+r*cos(angle),
            y=y+r*sin(angle))
    mapply(.circ,
        id=seq_len(nrow(df)),
        x=df$x, y=df$y, r=df$radius,
        SIMPLIFY=FALSE) |> do.call(what=rbind)
}
