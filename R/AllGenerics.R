#' @export
setGeneric("image", function(x, ...) standardGeneric("image"))
#' @export
setGeneric("images", function(x, ...) standardGeneric("images"))
#' @export
setGeneric("image<-", function(x, i, ..., value) standardGeneric("image<-"))
#' @export
setGeneric("images<-", function(x, value) standardGeneric("images<-"))

#' @export
setGeneric("label", function(x, ...) standardGeneric("label"))
#' @export
setGeneric("labels", function(x, ...) standardGeneric("labels"))
#' @export
setGeneric("label<-", function(x, i, ..., value) standardGeneric("label<-"))
#' @export
setGeneric("labels<-", function(x, value) standardGeneric("labels<-"))

#' @export
setGeneric("shape", function(x, ...) standardGeneric("shape"))
#' @export
setGeneric("shapes", function(x, ...) standardGeneric("shapes"))
#' @export
setGeneric("shape<-", function(x, i, ..., value) standardGeneric("shape<-"))
#' @export
setGeneric("shapes<-", function(x, value) standardGeneric("shapes<-"))

#' @export
setGeneric("point", function(x, ...) standardGeneric("point"))
#' @export
setGeneric("points", function(x, ...) standardGeneric("points"))
#' @export
setGeneric("point<-", function(x, i, ..., value) standardGeneric("point<-"))
#' @export
setGeneric("points<-", function(x, value) standardGeneric("points<-"))

#' @export
setGeneric("table", function(x, ...) standardGeneric("table"))
#' @export
setGeneric("table<-", function(x, value) standardGeneric("table<-"))

setGeneric("imageNames", function(x, ...) standardGeneric("imageNames"))
setGeneric("labelNames", function(x, ...) standardGeneric("labelNames"))
setGeneric("shapeNames", function(x, ...) standardGeneric("shapeNames"))
setGeneric("pointNames", function(x, ...) standardGeneric("pointNames"))

setGeneric("elementNames", function(x, ...) standardGeneric("elementNames"))
setGeneric("element", function(x, ...) standardGeneric("element"))

setGeneric("channels", function(x, ...) standardGeneric("channels"))
setGeneric("coords", function(x, ...) standardGeneric("coords"))
setGeneric("coord", function(x, ...) standardGeneric("coord"))

setGeneric("scaleArray", function(x, ...) standardGeneric("scaleArray"))
setGeneric("rotateArray", function(x, ...) standardGeneric("rotateArray"))
setGeneric("translateArray", function(x, ...) standardGeneric("translateArray"))
setGeneric("transformArray", function(x, ...) standardGeneric("transformArray"))

setGeneric("alignElements", function(...) standardGeneric("alignElements"))
