#' @name Zattrs
#' @title `SpatialData` .zattrs
#' 
#' @aliases ct_data ct_axes ct_type ct_name ct_args
#' 
#' @description Handling of coordinate transformations. 
#' 
#' @param x \code{Zattrs}
#' @param i scalar integer or string 
#'   specifying a coordinate transformation.
#' @param ... ignored.
#' 
#' @return
#' \item{ct_axes}{character vector of axes names and types.}
#' \item{ct_data}{full coordinate transformation (CT) data.}
#' \item{ct_args}{CT arguments (scalar, vector, or matrix).}
#' \item{ct_type/name}{character vector for CT types/names.}
#' 
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' sd <- readSpatialData(pa)
#' za <- zattrs(sd@labels[[1]])
#' 
#' ct_axes(za)
#' ct_name(za)
#' ct_args(za, "scale")
#' ct_args(za, "affine")
NULL

#' @rdname Zattrs
#' @export
ct_data <- new_generic("ct_data", "x")
method(ct_data, Zattrs) <- \(x) {
    ms <- x@data$multiscales[[1]]
    if (!is.null(ms)) x <- ms
    x$coordinateTransformations
}

#' @rdname Zattrs
#' @export
ct_type <- new_generic("ct_type", "x")
method(ct_type, Zattrs) <- \(x) {
    vapply(ct_data(x), \(.) .$type, character(1))
}

#' @rdname Zattrs
#' @export
ct_name <- new_generic("ct_name", "x")
method(ct_name, Zattrs) <- \(x) {
    vapply(ct_data(x), \(.) .$output$name, character(1))
}

#' @rdname Zattrs
#' @export
ct_axes <- new_generic("ct_axes", "x")
method(ct_axes, Zattrs) <- \(x) {
    ax <- ct_data(x)[[1]]$input$axes
    `names<-`(
        vapply(ax, \(.) .$type, character(1)),
        vapply(ax, \(.) .$name, character(1)))
    
}

#' @rdname Zattrs
#' @importFrom S7 class_any
#' @export
ct_args <- new_generic("ct_args", c("x", "i"))
method(ct_args, list(Zattrs, class_any)) <- \(x, i) {
    if (missing(i)) {
        i <- ct_name(x)[1]
    } else {
        stopifnot(length(i) == 1)
        if (is.numeric(i)) {
            stopifnot(i == round(i))
            i <- ct_name(x)[i]
        } else if (is.character(i)) {
            i <- match.arg(i, ct_name(x))
        }
    }
    i <- which(ct_name(x) == i)
    l <- ct_data(x)[[i]]
    t <- l[[ct_type(x)[[i]]]]
    # TODO: not sure how to best return yet;
    # all scalar or vector, but not for affine
    t <- lapply(t, as.list)
    lapply(t, unlist)
    # if (is.list(t[[1]])) {
    #     do.call(rbind, t)
    # } else {
    #     unlist(t)
    # }
}
