#' #' @name query
#' #' @rdname query
#' #' 
#' #' @title Query an element by another
#' #' 
#' #' @description ...
#' #'
#' #' @param x,y A \code{\link{SpatialData}} element
#' #'   (see Usage for currently supported methods).
#' #' @param fun Function to use for aggregation.
#' #' @param name Character string specifying
#' #'   the name of the output SCE's `assay`.
#' #'
#' #' @return
#' #' A \code{\link[SingleCellExperiment]{SingleCellExperiment}}
#' #' where rows/columns correspond to data from \code{x/y}.
#' #'
#' #' @examples
#' #' dir <- "inst/extdata/toy"
#' #' (spd <- readSpatialData(dir))
#' #' 
#' #' spd <- addTable(spd, 
#' #'   image="raccoon", 
#' #'   label="segmentation")
#' #' 
#' #' (sce <- table(spd, "raccoon"))
#' #'
#' #' @author Helena L. Crowell
#' NULL
#' 
#' filterSD <- function(x, i=NULL, coords=NULL) {
#'     for (el in setdiff(elementNames(x), "table")) {
#'         y <- x[[el]]
#'         # filter elements
#'         fun <- get(paste0(., "<-"))
#'         x <- fun(x, y[intersect(names(y), i)])
#'         # filter coordinate systems
#'         if (el != "points")
#'         for (id in names(y)) {
#'             z <- y[[id]]
#'             c <- coords(z)$output.name
#'             c <- intersect(c, coords)
#'         }
#'     }
#'     return(x)
#' }
#' setReplaceMethod("coords",
#'     c("ImageArray", "list"),
#'     function(x, value) {
#'         x <- image(spd)
#'         md <- metadata(x)
#' 
#'         md$multiscales$axes
#'         x@table <- NULL
#'         return(x)
#'     }
#' )
#' 
#' setGeneric("query", function(x, ...) standardGeneric("query"))
#' setMethod("query", "ZarrArray", function(x, xlim = NULL, ylim = NULL) {
#'     if (!is.null(xlim)) x <- x[, , seq(xlim[1], xlim[2])]
#'     if (!is.null(ylim)) x <- x[, seq(ylim[1], ylim[2]), ]
#'     return(x)
#' })
#' setMethod("query", "ShapeFrame", function(x, xlim = NULL, ylim = NULL) {
#'     switch(x$type[1],
#'         circle={
#'             x$data <- lapply(x$data,
#'                 \(df) df[
#'                     df[1] >= xlim[1] &
#'                     df[1] <= xlim[2] &
#'                     df[2] >= ylim[1] &
#'                     df[2] <= ylim[2] ])
#'             x[vapply(x$data, length, numeric(1)) > 0, ]
#'         },
#'         polygon={
#'             x$data <- lapply(x$data,
#'                 \(df) df[
#'                     df[, 1] >= xlim[1] &
#'                     df[, 1] <= xlim[2] &
#'                     df[, 2] >= ylim[1] &
#'                     df[, 2] <= ylim[2], , drop = FALSE])
#'             x[vapply(x$data, nrow, numeric(1)) > 0, ]
#'         })
#' })
#' setMethod("query", "PointFrame", function(x, xlim = NULL, ylim = NULL) {
#'     x@data <- filter(x@data,
#'         x >= xlim[1], x <= xlim[2],
#'         y >= ylim[1], y <= ylim[2])
#'     return(x)
#' })
#' setMethod("query", "SpatialData", function(x, xlim = NULL, ylim = NULL) {
#'     x <- spd
#'     images(x) <- lapply(images(x), query, xlim=xlim, ylim=ylim)
#' })
