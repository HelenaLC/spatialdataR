#' @importFrom EBImage translate
.translateZarrArray <- function(x, t) {
    d <- dim(x)
    h <- c(0, d[2])+t[2]
    w <- c(0, d[3])+t[3]
    metadata(x) <- c(
        metadata(x),
        list(h=h, w=w))
    return(x)
    # a <- as.array(x)
    # b <- aperm(translate(aperm(a), t[-1]))
    # get(class(x))(b, zattrs=zattrs(x))
}

.translateShapeFrame <- function(x, t) {
    a <- as.array(x)
    a[, 1] <- a[, 1]+t[2]
    a[, 2] <- a[, 2]+t[1]
    y <- switch(x$type[1],
        circle=asplit(a, 1),
        polygon={
            ns <- vapply(x$data, nrow, integer(1))
            i <- rep.int(x$index, ns)
            i <- split(seq(nrow(a)), i)
            lapply(i, \(.) a[., ])
        })
    x$data <- y
    return(x)
}

#' @importFrom dplyr mutate
.translatePointFrame <- function(x, t) {
    x@data <- x@data %>%
        mutate(x=x+t[1], y=y+t[2])
    return(x)
}

.translate <- function(x, t) {
    stopifnot(
        "'t' should be numeric"=is.numeric(t),
        "'t' should be whole numbers"=round(t) == t)
    # do nothing when all coordinate shifts are 0
    if (all(t == 0)) return(x)
    # otherwise call corresponding method
    c <- ifelse(is(x, za <- "ZarrArray"), za, class(x))
    get(paste0(".translate", c))(x, t)
}

#' @rdname ZarrArray
#' @export
setMethod("translateElement", "SpatialDataElement",
    function(x, t=numeric(2)) .translate(x, t))
