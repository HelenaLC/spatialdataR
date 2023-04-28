
#' @export
setGeneric("images", function(x, ...) standardGeneric("images"))

#' @export
setGeneric("labels", function(x, ...) standardGeneric("labels"))

#' @export
setGeneric("shapes", function(x, ...) standardGeneric("shapes"))

#' @export
setGeneric("points", function(x, ...) standardGeneric("points"))

#' @export
setGeneric("image", function(x, ...) standardGeneric("image"))

#' @export
setGeneric("label", function(x, ...) standardGeneric("label"))

#' @export
setGeneric("shape", function(x, ...) standardGeneric("shape"))

#' @export
setGeneric("point", function(x, ...) standardGeneric("point"))

setGeneric("imageNames", function(x, ...) standardGeneric("imageNames"))
setGeneric("labelNames", function(x, ...) standardGeneric("labelNames"))
setGeneric("shapeNames", function(x, ...) standardGeneric("shapeNames"))
setGeneric("pointNames", function(x, ...) standardGeneric("pointNames"))

setGeneric("elementNames", function(x, ...) standardGeneric("elementNames"))
setGeneric("element", function(x, ...) standardGeneric("element"))

setGeneric("coords", function(x, ...) standardGeneric("coords"))
setGeneric("coord", function(x, ...) standardGeneric("coord"))

setGeneric("scaleImage", function(x, ...) standardGeneric("scaleImage"))
setGeneric("rotateImage", function(x, ...) standardGeneric("rotateImage"))
setGeneric("transformImage", function(x, ...) standardGeneric("transformImage"))
