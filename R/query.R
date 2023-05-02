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
#' 
#' #' @rdname SpatialData
#' #' @importFrom S4Vectors DataFrame
#' #' @export
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
