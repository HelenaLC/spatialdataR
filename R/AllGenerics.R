#' @export
setGeneric("data", function(x, ...) standardGeneric("data"))

#' @export
setGeneric("metadata", function(x, ...) standardGeneric("metadata"))

#' @export
setGeneric("images", function(x, ...) standardGeneric("images"))

#' @export
setGeneric("labels", function(x, ...) standardGeneric("labels"))

#' @export
setGeneric("shapes", function(x, ...) standardGeneric("shapes"))

#' @export
setGeneric("points", function(x, ...) standardGeneric("points"))

setGeneric("imageNames", function(x, ...) standardGeneric("imageNames"))
setGeneric("labelNames", function(x, ...) standardGeneric("labelNames"))
setGeneric("shapeNames", function(x, ...) standardGeneric("shapeNames"))
setGeneric("pointNames", function(x, ...) standardGeneric("pointNames"))

setGeneric("elementNames", function(x, ...) standardGeneric("elementNames"))
setGeneric("element", function(x, ...) standardGeneric("element"))

#' @export
setGeneric("getLayer", function(x, ...) standardGeneric("getLayer"))
