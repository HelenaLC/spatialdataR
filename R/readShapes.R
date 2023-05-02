#' @rdname readShapes
#' @title Read `shapes` element
#' @description ...
#'
#' @param path A character string specifying
#'   the path to a `shapes/` subdirectory.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{\link{DataFrame}}
#'
#' @examples
#' path <- file.path("extdata", "raccoon", "shapes", "circles")
#' path <- system.file(path, package="SpatialData")
#' (df <- readShapes(path))
#'
#' @author Tim Treis, Helena L. Crowell
#'
#' @importFrom jsonlite fromJSON
#' @importFrom S4Vectors DataFrame
#' @importFrom Rarr read_zarr_array
#' @export
readShapes <- function(path, ...) {
    # TODO: metadata are currently being ignored here...
    # might need another data structure to accommodate these.
    parts <- list.dirs(path, recursive=FALSE)
    names(ps) <- ps <- c("coords", "Index", "radius", "offset0", "offset1")
    ps <- lapply(ps, \(p) {
        if (p %in% basename(parts))
            read_zarr_array(file.path(path, p))
    })
    geom <- ifelse(!is.null(ps$radius), "circle", "polygon")
    md <- fromJSON(file.path(path, ".zattrs"))
    switch(geom,
        circle={
            df <- DataFrame(
                data=I(asplit(ps$coords, 1)),
                index=ps$Index,
                radius=ps$radius,
                type=rep(geom, length(ps$Index)))
        },
        polygon={
            coords <- lapply(seq_along(ps$Index), \(.) {
                idx <- seq(ps$offset0[[.]] + 1, ps$offset0[[. + 1]])
                ps$coords[idx, , drop=FALSE]
            })
            df <- DataFrame(
                data=I(coords),
                index=ps$Index,
                type=rep(geom, length(ps$Index)))
        })
    ShapeFrame(df, zattrs=Zattrs(md))
}
