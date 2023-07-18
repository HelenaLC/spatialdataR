#' @importFrom EBImage abind resize
.scaleZarrArray <- function(x, t) {
    d <- length(dim(x))
    if (length(t) != d)
        stop("'t' should be of length ", d)
    y <- as.array(x)
    y <- resize(aperm(y), 
        w=dim(y)[3]*t[d], 
        h=dim(y)[2]*t[d-1])
    y <- aperm(y)
    fun <- get(class(x))
    fun(y, zattrs(x))
}

.scaleShapeFrame <- function(x, t) {
    switch(x$type[1],
        circle={
            if (length(t) != 1) stop(
                "'t' should be scalar for scaling ",
                "for scaling 'circle' shapes")
            r <- t*x$radius
            x$radius <- r
        },
        polygon={
            if (length(t) != 2) stop(
                "'t' should be of length 2 ",
                "for scaling 'polygon' shapes")
            x$data <- lapply(x$data, \(df) sweep(df, 2, t, `*`))
        }
    )
    return(x)
}

.scalePointFrame <- function(x, t) {
    stopifnot("'t' should be of length 2 for scaling points"=length(t) == 2)
    x@data <- x@data %>% mutate(x=x*t[1], y=y*t[2])
    return(x)
}

.scale <- function(x, t) {
    # do nothing if all factors are 0
    if (all(t == 1)) return(x)
    # otherwise call corresponding method
    c <- ifelse(is(x, za <- "ZarrArray"), za, class(x))
    get(paste0(".scale", c))(x, t)
}

#' @rdname ZarrArray
#' @export
setMethod("scaleElement",
    "SpatialDataElement",
    function(x, t) {
        if (missing(t)) t <- rep(1, length(dim(x)))
        stopifnot(
            "'t' should be numeric"=is.numeric(t),
            "'t' should be greater than zero"=t > 0)
        .scale(x, t)
    }
)