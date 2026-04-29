#' @name combine
#' @title Combine two \code{SpatialData} objects
#' 
#' @param x,y \code{SpatialData} objects to combine.
#' @param ... ignored.
#' 
#' @returns
#' A \code{SpatialData} objects containing all elements 
#' from \code{x} and \code{y} with names made unique.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x)
#' 
#' y <- combine(x, x)
#' imageNames(y)
#' region(table(y, 1))
#' region(table(y, 2))
#' 
#' @importFrom BiocGenerics combine
#' @export
setMethod("combine", 
    c("SpatialData", "SpatialData"), 
    \(x, y, ...) {
        # ensure element names are unique across objects
        old <- list(unlist(colnames(x)), unlist(colnames(y)))
        idx <- rep.int(c(1, 2), vapply(old, length, integer(1)))
        new <- split(make.unique(unlist(old)), idx)
        for (i in c(1, 2)) {
            z <- get(c("x", "y")[i])
            for (l in rownames(z)) {
                j <- match(names(z[[l]]), old[[i]])
                names(z[[l]]) <- new[[i]][j]
            }
            # update tables accordingly
            for (t in tableNames(z)) {
                se <- table(z, t)
                j <- match(region(se), old[[i]])
                region(se) <- new[[i]][j]
                j <- match(regions(se), old[[i]])
                regions(se) <- new[[i]][j]
                table(z, t) <- se
            }
            assign(c("x", "y")[i], z)
        }
        SpatialData(
            images=c(x$images, y$images),
            labels=c(x$labels, y$labels),
            points=c(x$points, y$points),
            shapes=c(x$shapes, y$shapes),
            tables=c(x$tables, y$tables))
    })
