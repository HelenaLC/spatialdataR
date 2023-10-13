#' @name coords
#' @title coordinate systems
#' @aliases coord coord<- coords<- coordNames
#' 
#' @description ...
#' 
#' @param x A \code{\link{SpatialData}} element or \code{Zattrs} objects.
#' @param i Scalar integer or character string
#'   specifying which element to extract/replace.
#' @param value Replacement value.
#' 
#' @return
#' If \code{x} is a \code{Zattrs} object,
#' \code{CoordNames} returns a character vector,
#' \code{coord(s)} returns a \code{data.frame}.
#' If \code{x} is a \code{SpatialData} object
#' or a list of \code{SpatialData} elements,
#' only shared coordinate systems are returned.
#' 
#' @examples
#' dir <- file.path("extdata", "blobs")
#' dir <- system.file(dir, package="SpatialData")
#' spd <- readSpatialData(dir)
#' 
#' # all available
#' coordNames(image(spd)) 
#' coordNames(label(spd)) 
#' 
#' # shared only  
#' lys <- list(image(spd), label(spd))
#' coordNames(lys)
#' coords(lys)
#' 
#' @author Helena L. Crowell
NULL

#' @rdname coords
#' @export
setReplaceMethod("coords", c("SpatialDataElement", "data.frame"), \(x, value) {
    y <- zattrs(x)
    coords(y) <- value
    zattrs(x) <- y
    x
})
    
# get all ----

#' @rdname coords
#' @export
setMethod("coords", "Zattrs", \(x) {
    y <- x$multiscales
    . <- "coordinateTransformations"
    if (is.null(y)) x[[.]] else y[[.]][[1]]
})

#' @rdname coords
#' @export
setMethod("coords", "SpatialDataElement", \(x) coords(zattrs(x)))

#' @rdname coords
#' @export
setMethod("coords", "list", \(x) {
    lapply(
        coordNames(x),
        \(.) coord(x[[1]], .)) |>
        do.call(what=rbind)
})

#' @rdname coords
#' @export
setMethod("coords", "SpatialData", \(x) {
    . <- elementNames(x)
    y <- x[[names(.)[1]]][[1]]
    lapply(
        coordNames(x), 
        \(.) coord(y, .)) |>
        do.call(what=rbind)
})

# get one ----

#' @rdname coords
#' @export
setMethod("coord", "Zattrs", \(x, i=1) {
    stopifnot(length(i) == 1)
    y <- coords(x)
    if (is.numeric(i)) {
        stopifnot(i == round(i))
        if (i < 1 || i > (n <- nrow(y))) 
            stop("'i' should be between 1 and ", n)
    } else if (is.character(i)) {
        i <- match(i, y$output$name)
        if (is.na(i)) 
            stop("couldn't find coordinate system '", i, "'")
    }
    y[i, ]
})

#' @rdname coords
#' @export
setMethod("coord", "SpatialDataElement", \(x, i=1) coord(zattrs(x), i))

# get nms ----

#' @rdname coords
#' @export
setMethod("coordNames", "Zattrs", \(x) {
    y <- x$multiscales
    . <- "coordinateTransformations"
    z <- if (is.null(y)) x[[.]] else y[[.]][[1]]
    z$output$name
})

#' @rdname coords
#' @export
setMethod("coordNames", "SpatialDataElement", \(x) coordNames(zattrs(x)))

# retrieve shared coordinate systems
# across a list of arbitrary elements
#' @rdname coords
#' @export
setMethod("coordNames", "list", \(x) {
    . <- \(.) is(., "SpatialDataElement")
    if (!all(vapply(x, ., logical(1))))
        stop("list elements should be 'SpatialData'",
            " image/label/shape/point/table elements")
    y <- lapply(x, coordNames)
    Reduce(intersect, y)
})

# retrieve shared coordinate systems
# across an entire 'SpatialData' object
#' @rdname coords
#' @export
setMethod("coordNames", "SpatialData", \(x) {
    i <- setdiff(LAYERS, "tables")
    i <- lapply(i, \(.) get(.)(x))
    i <- i[vapply(i, length, integer(1)) > 0]
    Reduce(intersect, lapply(i, coordNames))
    # y <- rapply(i, coordNames, how="list")
    # Reduce(intersect, Reduce(c, y))
})

# set all ----

#' @rdname coords
#' @export
setReplaceMethod("coords", "Zattrs", \(x, value) {
    y <- x$multiscales
    . <- "coordinateTransformations"
    if (is.null(y)) {
        x[[.]] <- value
    } else {
        y[[.]][[1]] <- value
        x$multiscales <- y
    }
    x
})

# axiis ----

#' @rdname ZarrArray
#' @export
setMethod("axiis", "SpatialDataElement", \(x, 
    type=c("time", "space", "channel")) {
    stopifnot(is.character(type))
    type <- match.arg(type, several.ok=TRUE)
    if (length(zattrs(x))) {
        df <- coord(x)$input$axes[[1]]
        df$name[df$type %in% type]
    } else character(0)
})

# devel ----

setMethod("setCoord", "Zattrs", function(x, value) {
    ms <- x$multiscales
    if (is.null(ms)) {
        ct <- x$coordinateTransformations
        if (is.data.frame(ct)) {
            x$coordinateTransformations <- value
        } else {
            x$coordinateTransformations[[1]] <- value
        }
    } else {
        x$multiscales$coordinateTransformations[[1]] <- value
    }
    return(x)
})

setMethod("setCoord", "ANY", function(x, value)
    `zattrs<-`(x, setCoord(zattrs(x), value)))

# new ----
    
newCoord <- \(name, type="identity", data=NULL) {
    axes <- I(list(data.frame(name=c("x", "y"), type="space", unit="unit")))
    df <- data.frame(
        input=I(data.frame(axes, name="xy")),
        output=I(data.frame(axes, name)), type=type)
    df[[type]] <- data
    df
}
        
setMethod("addCoord", "SpatialDataElement", 
    \(x, name, type, data=NULL) {
        # construct new data
        df <- coords(x)
        fd <- df[1, ]
        fd$type <- type
        fd[[type]] <- data
        fd$output$name <- name
        # fix duplicated names
        n <- 1+nrow(df)
        rownames(fd) <- n
        rownames(fd$input) <- n
        rownames(fd$output) <- n
        `coords<-`(x, rbind(df, fd))
    })

.newCoord <- function(x, name, type="identity", data=NULL) {
    type <- match.arg(type, c("identity", "scale")) # TODO: more choices
    df <- getCoord(x)[1, ]
    df$output$name <- name
    df$type <- type
    if (type != "identity") {
        if (!is.list(data))
            data <- list(data)
        df[[type]] <- data
    }
    return(df)
}

setMethod("addCoord", "Zattrs",
    function(x, name, type="identity", data=NULL) {
        old <- getCoord(x)
        df <- .newCoord(x, name, type, data)
        # there has to be an easier way than this...
        # but 'data.frame' keeps flattening stuff
        l <- vector("list", nrow(old)+1)
        new <- data.frame(
            input=I(l), output=I(l),
            type=character(1),
            data=character(1))
        new$input <- rbind(old$input, df$input)
        new$output <- rbind(old$output, df$output)
        new$type <- c(old$type, df$type)
        if (is.null(old$data))
            old$data <- list(old$data)
        new$data <- c(old$data, df[[type]])
        # need an example with different transformations
        # to see what specs actually look like in the wild...
        nan <- vapply(new$data, is.null, logical(1))
        if (all(nan)) new$data <- NULL else {
            typ <- names(old)[ncol(old)]
            names(new)[ncol(new)] <- type
        }
        setCoord(x, new)
    }
)

setMethod("addCoord",
    "ZarrArray_OR_ShapeFrame",
    function(x, name, type, data) {
        l <- addCoord(zattrs(x), name, type, data)
        zattrs(x) <- l
        return(x)
    }
)

setMethod("rmvCoord", "Zattrs", function(x, name) {
    old <- getCoord(x)
    idx <- match(name, old$output$name)
    new <- old[-idx[!is.na(idx)], ]
    setCoord(x, new)
})

setMethod("rmvCoord",
    "ZarrArray_OR_ShapeFrame",
    function(x, name) {
        l <- rmvCoord(zattrs(x), name)
        zattrs(x) <- l
        return(x)
    }
)
