#' @name Array-methods
#' @title Methods for `ImageArray` and `LabelArray` class
#' 
#' @aliases 
#' data,ImageArray-method
#' data,LabelArray-method
#' dim,ImageArray-method
#' dim,LabelArray-method
#' length,ImageArray-method
#' length,LabelArray-method
#' data_type,ImageArray-method
#' data_type,LabelArray-method
#' 
#' @param x \code{ImageArray} or  \code{LabelArray}
#' @param k scalar index specifying which scale to extract.
#' 
#' @return \code{ImageArray}
#'
#' @examples
#' library(SpatialData.data)
#' zs <- get_demo_SDdata("merfish")
#' 
#' # helper that gets path to first element in layer 'l' 
#' fn <- \(l) list.files(file.path(zs, l), full.names=TRUE)[1]
#'   
#' # read individual element  
#' (ia <- readImage(fn("images")))
#' 
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
NULL

#' @rdname Array-methods
#' @export
setMethod("data", "sdArray", \(x, k=1) {
  if (is.null(k)) return(x@data)
  stopifnot(length(k) == 1, is.numeric(k), k > 0)
  n <- length(x@data) # get number of available scales
  if (is.infinite(k)) k <- n # input of Inf uses lowest
  if (k <= n) return(x@data[[k]]) # return specified scale
  stop("'k=", k, "' but only ", n, " resolution(s) available")
})

#' @rdname Array-methods
#' @export
setMethod("dim", "sdArray", \(x) dim(data(x)))

#' @rdname Array-methods
#' @export
setMethod("length", "sdArray", \(x) length(data(x, NULL)))

#' @rdname Array-methods
#' @export
setMethod("data_type", "sdArray", \(x) {
    if (is(y <- data(x), "DelayedArray")) 
        data_type(y) else metadata(x)$data_type
})

#' @rdname Array-methods
#' @importFrom DelayedArray DelayedArray
#' @importFrom Rarr zarr_overview
#' @importFrom ZarrArray path
#' @export
setMethod("data_type", "DelayedArray", \(x) zarr_overview(path(x), as_data_frame=TRUE)$data_type)
