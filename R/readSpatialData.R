#' @rdname readSpatialData
#' @title Read `SpatialData` OME-Zarr
#' @description ...
#'
#' @param path A character string specifying the path to an
#'   OME-Zarr file adhering to \code{SpatialData} specification.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{\link{SpatialData}}
#'
#' @examples
#' path <- file.path("extdata", "blobs")
#' path <- system.file(path, package="SpatialData")
#' (spd <- readSpatialData(path))
#'
#' @author Constantin Ahlmann-Eltze, Helena L. Crowell
#'
#' @export
readSpatialData <- function(path, ...) {
    layers <- list.dirs(path, recursive=FALSE)

    # used internally only atm
    dots <- list(...)
    i <- if (is.null(n <- dots$n)) TRUE else
        if (length(n) == 1) seq_len(n) else n

    for (var in c("images", "labels", "shapes", "points")) {
        val <- if (var %in% basename(layers)) {
            dir <- file.path(path, var)
            sub <- list.dirs(dir, recursive=FALSE)
            names(sub) <- basename(sub)
            fun <- switch(var,
                shapes=readShapes,
                points=readPoints,
                readArray)
            lapply(sub[i], fun)
        } else list()
        assign(var, val)
    }

    table <- if ("table" %in% basename(layers)) {
        tryCatch(
            error=function(e) NULL,
            readTable(file.path(path, "table/table")))
    }

    SpatialData(
        images=images,
        labels=labels,
        shapes=shapes,
        points=points,
        table=table)
}
