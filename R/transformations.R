#' @rdname ZarrArray
#' @importFrom S4Vectors DataFrame
#' @export
setMethod("coords", "ImageArray", function(x) {
    df <- metadata(x)$multiscales$coordinateTransformations[[1]]
    data <- lapply(seq(nrow(df)), \(.)
        ifelse(df$type[.] == "identity",
            list(NA), I(df[., df$type[.]])))
    DataFrame(
        input.name = df$input$name,
        output.name = df$output$name,
        input.axes = I(df$input$axes),
        output.axes = I(df$output$axes),
        type = df$type,
        data = I(unlist(data, recursive = FALSE)))
})

#' @rdname ZarrArray
#' @export
setMethod("coord", "ImageArray", function(x, name) {
    df <- coords(x)
    if (missing(name))
        name <- df$output.name[1]
    stopifnot(length(name) == 1,
        is.character(name))
    idx <- match(name, df$output.name)
    if (is.na(idx))
        stop("couldn't find coords '", name, "'")
    return(df[idx,])
})

#' @rdname ZarrArray
#' @importFrom EBImage abind resize
#' @export
setMethod("scaleImage", "ImageArray", function(x, t=rep(1, length(dim(x)))) {
    stopifnot(
        is.numeric(t),
        length(t) == length(dim(x)))
    a <- as.array(x)
    y <- apply(a, 1, \(.)
        resize(., nrow(.) * t[2], ncol(.) * t[3]),
        simplify = FALSE)
    y <- abind(y, along = 0)
    ImageArray(y, metadata(x))
})

#' @rdname ZarrArray
#' @importFrom EBImage abind rotate
#' @export
setMethod("rotateImage", "ImageArray", function(x, t=0) {
    stopifnot(
        is.numeric(t),
        length(t) == 1)
    a <- as.array(x)
    y <- apply(a, 1, rotate, t, simplify = FALSE)
    y <- abind(y, along = 0)
    ImageArray(y, metadata(x))
})

#' @rdname ZarrArray
#' @importFrom EBImage abind translate
#' @export
setMethod("translateImage", "ImageArray", function(x, t=c(0,0)) {
    stopifnot(
        is.numeric(t),
        length(t) == 2,
        round(t) == t)
    a <- as.array(x)
    y <- apply(a, 1, translate, t, simplify = FALSE)
    y <- abind(y, along = 0)
    ImageArray(y, metadata(x))
})

#' @rdname ZarrArray
#' @export
setMethod("transformImage", "ImageArray", function(x, coords) {
    df <- coord(x, coords)
    t <- df$data[[1]]
    switch(
        df$type,
        "identity" = x,
        "scale" = scaleImage(x, t),
        "rotate" = rotateImage(x, t),
        sprintf("transformation of type '%s' yet to be supported.", df$type))
})
