# get one ----

setGeneric("image", \(x, ...) standardGeneric("image"))
setGeneric("label", \(x, ...) standardGeneric("label"))
setGeneric("shape", \(x, ...) standardGeneric("shape"))
setGeneric("point", \(x, ...) standardGeneric("point"))
setGeneric("table", \(x, ...) standardGeneric("table"))

# get all ----

setGeneric("images", \(x, ...) standardGeneric("images"))
setGeneric("labels", \(x, ...) standardGeneric("labels"))
setGeneric("shapes", \(x, ...) standardGeneric("shapes"))
setGeneric("points", \(x, ...) standardGeneric("points"))
setGeneric("tables", \(x, ...) standardGeneric("tables"))

# set one -----

setGeneric("image<-", \(x, i, ..., value) standardGeneric("image<-"))
setGeneric("shape<-", \(x, i, ..., value) standardGeneric("shape<-"))
setGeneric("label<-", \(x, i, ..., value) standardGeneric("label<-"))
setGeneric("point<-", \(x, i, ..., value) standardGeneric("point<-"))
setGeneric("table<-", \(x, i, ..., value) standardGeneric("table<-"))

# set all -----

setGeneric("images<-", \(x, value) standardGeneric("images<-"))
setGeneric("labels<-", \(x, value) standardGeneric("labels<-"))
setGeneric("shapes<-", \(x, value) standardGeneric("shapes<-"))
setGeneric("points<-", \(x, value) standardGeneric("points<-"))
setGeneric("tables<-", \(x, value) standardGeneric("tables<-"))

setGeneric("element", \(x, ...) standardGeneric("element"))
setGeneric("elements", \(x, ...) standardGeneric("elements"))
setGeneric("elementNames", \(x, ...) standardGeneric("elementNames"))

# get nms ----
setGeneric("imageNames", \(x, ...) standardGeneric("imageNames"))
setGeneric("labelNames", \(x, ...) standardGeneric("labelNames"))
setGeneric("shapeNames", \(x, ...) standardGeneric("shapeNames"))
setGeneric("pointNames", \(x, ...) standardGeneric("pointNames"))
setGeneric("tableNames", \(x, ...) standardGeneric("tableNames"))

# set nms ----
setGeneric("imageNames<-", \(x, value) standardGeneric("imageNames<-"))
setGeneric("labelNames<-", \(x, value) standardGeneric("labelNames<-"))
setGeneric("shapeNames<-", \(x, value) standardGeneric("shapeNames<-"))
setGeneric("pointNames<-", \(x, value) standardGeneric("pointNames<-"))
setGeneric("tableNames<-", \(x, value) standardGeneric("tableNames<-"))

setGeneric("geoms", \(x, ...) standardGeneric("geoms"))
setGeneric("axiis", \(x, ...) standardGeneric("axiis"))
setGeneric("channels", \(x, ...) standardGeneric("channels"))
setGeneric("channels<-", \(x, value) standardGeneric("channels<-"))

# transformations -----
setGeneric("alignElements", \(...) standardGeneric("alignElements"))
setGeneric("scaleElement", \(x, ...) standardGeneric("scaleElement"))
setGeneric("rotateElement", \(x, ...) standardGeneric("rotateElement"))
setGeneric("translateElement", \(x, ...) standardGeneric("translateElement"))
setGeneric("transformElement", \(x, ...) standardGeneric("transformElement"))

# zattrs ----

setGeneric("zattrs", \(x, ...) standardGeneric("zattrs"))
setGeneric("zattrs<-", \(x, value) standardGeneric("zattrs<-"))

setGeneric("coord", \(x, ...) standardGeneric("coord"))
setGeneric("coord<-", \(x, i, value) standardGeneric("coord<-"))

setGeneric("coords", \(x, ...) standardGeneric("coords"))
setGeneric("coords<-", \(x, value) standardGeneric("coords<-"))

setGeneric("coordNames", \(x, ...) standardGeneric("coordNames"))
setGeneric("coordNames<-", \(x, value) standardGeneric("coordNames<-"))

setGeneric("getCoord", \(x, ...) standardGeneric("getCoord"))
setGeneric("setCoord", \(x, ...) standardGeneric("setCoord"))
setGeneric("addCoord", \(x, ...) standardGeneric("addCoord"))
setGeneric("rmvCoord", \(x, ...) standardGeneric("rmvCoord"))

# plot ----

setGeneric("plotImage", \(x, ...) standardGeneric("plotImage"))
setGeneric("plotLabel", \(x, ...) standardGeneric("plotLabel"))
setGeneric("plotShape", \(x, ...) standardGeneric("plotShape"))
setGeneric("plotPoint", \(x, ...) standardGeneric("plotPoint"))
setGeneric("pile", \(...) standardGeneric("pile"))

# utils ----

setGeneric("mask", \(x, y, ...) standardGeneric("mask"))
setGeneric("query", \(x, y, ...) standardGeneric("query"))
