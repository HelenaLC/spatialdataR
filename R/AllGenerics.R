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
setGeneric("table", function(x, value) standardGeneric("table"))
#' @export
setGeneric("table<-", function(x, value) standardGeneric("table<-"))

setGeneric("element", function(x, ...) standardGeneric("element"))
setGeneric("elementNames", function(x, ...) standardGeneric("elementNames"))

setGeneric("imageNames", function(x, ...) standardGeneric("imageNames"))
setGeneric("labelNames", function(x, ...) standardGeneric("labelNames"))
setGeneric("shapeNames", function(x, ...) standardGeneric("shapeNames"))
setGeneric("pointNames", function(x, ...) standardGeneric("pointNames"))

setGeneric("axes", function(x, ...) standardGeneric("axes"))

setGeneric("channels", function(x, ...) standardGeneric("channels"))
setGeneric("channels<-", function(x, value) standardGeneric("channels<-"))

setGeneric("coord", function(x, ...) standardGeneric("coord"))
setGeneric("coords", function(x, ...) standardGeneric("coords"))
setGeneric("coord<-", function(x, i, ..., value) standardGeneric("coord<-"))
setGeneric("coords<-", function(x, value) standardGeneric("coords<-"))

setGeneric("scaleElement", function(x, ...) standardGeneric("scaleElement"))
setGeneric("rotateElement", function(x, ...) standardGeneric("rotateElement"))
setGeneric("translateElement", function(x, ...) standardGeneric("translateElement"))
setGeneric("transformElement", function(x, ...) standardGeneric("transformElement"))

setGeneric("scaleFrame", function(x, ...) standardGeneric("scaleFrame"))

setGeneric("alignElements", function(...) standardGeneric("alignElements"))

setGeneric("plotElement", function(x, ...) standardGeneric("plotElement"))
