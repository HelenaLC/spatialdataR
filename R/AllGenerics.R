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

# get nms ----

setGeneric("imageNames", \(x, ...) standardGeneric("imageNames"))
setGeneric("labelNames", \(x, ...) standardGeneric("labelNames"))
setGeneric("shapeNames", \(x, ...) standardGeneric("shapeNames"))
setGeneric("pointNames", \(x, ...) standardGeneric("pointNames"))
setGeneric("tableNames", \(x, ...) standardGeneric("tableNames"))

# set nms ----

setGeneric("imageNames<-", \(x, ..., value) standardGeneric("imageNames<-"))
setGeneric("labelNames<-", \(x, ..., value) standardGeneric("labelNames<-"))
setGeneric("shapeNames<-", \(x, ..., value) standardGeneric("shapeNames<-"))
setGeneric("pointNames<-", \(x, ..., value) standardGeneric("pointNames<-"))
setGeneric("tableNames<-", \(x, ..., value) standardGeneric("tableNames<-"))

# set one ----

setGeneric("image<-", \(x, i, ..., value) standardGeneric("image<-"))
setGeneric("shape<-", \(x, i, ..., value) standardGeneric("shape<-"))
setGeneric("label<-", \(x, i, ..., value) standardGeneric("label<-"))
setGeneric("point<-", \(x, i, ..., value) standardGeneric("point<-"))
setGeneric("table<-", \(x, i, ..., value) standardGeneric("table<-"))

# set all ----

setGeneric("images<-", \(x, value) standardGeneric("images<-"))
setGeneric("labels<-", \(x, value) standardGeneric("labels<-"))
setGeneric("shapes<-", \(x, value) standardGeneric("shapes<-"))
setGeneric("points<-", \(x, value) standardGeneric("points<-"))
setGeneric("tables<-", \(x, value) standardGeneric("tables<-"))

# trs ----

setGeneric("CTlist", \(x, ...) standardGeneric("CTlist"))
setGeneric("CTdata", \(x, ...) standardGeneric("CTdata"))
setGeneric("CTname", \(x, ...) standardGeneric("CTname"))
setGeneric("CTtype", \(x, ...) standardGeneric("CTtype"))

setGeneric("CTpath", \(x, ...) standardGeneric("CTpath"))
setGeneric("CTgraph", \(x, ...) standardGeneric("CTgraph"))

setGeneric("rmvCT", \(x, ...) standardGeneric("rmvCT"))
setGeneric("addCT", \(x, ...) standardGeneric("addCT"))

setGeneric("scale", \(x, t, ...) standardGeneric("scale"))
setGeneric("rotate", \(x, t, ...) standardGeneric("rotate"))
setGeneric("sequence", \(x, t, ...) standardGeneric("sequence"))
setGeneric("transform", \(x, i, ...) standardGeneric("transform"))
setGeneric("translation", \(x, t, ...) standardGeneric("translation"))

setGeneric("flip", \(x, ...) standardGeneric("flip"))
setGeneric("flop", \(x, ...) standardGeneric("flop"))
setGeneric("mirror", \(x, ...) standardGeneric("mirror"))

# sda ----

setGeneric("region", \(x, ...) standardGeneric("region"))
setGeneric("region<-", \(x, value) standardGeneric("region<-"))
setGeneric("regions", \(x, ...) standardGeneric("regions"))
setGeneric("regions<-", \(x, value) standardGeneric("regions<-"))
setGeneric("instances", \(x, ...) standardGeneric("instances"))
setGeneric("instances<-", \(x, value) standardGeneric("instances<-"))
setGeneric("region_key", \(x, ...) standardGeneric("region_key"))
setGeneric("region_key<-", \(x, value) standardGeneric("region_key<-"))
setGeneric("feature_key", \(x, ...) standardGeneric("feature_key"))
setGeneric("feature_key<-", \(x, value) standardGeneric("feature_key<-"))
setGeneric("instance_key", \(x, ...) standardGeneric("instance_key"))
setGeneric("instance_key<-", \(x, value) standardGeneric("instance_key<-"))

# uts ----

setGeneric("data", \(x, ...) standardGeneric("data"))
setGeneric("meta", \(x, ...) standardGeneric("meta"))

setGeneric("data<-", \(x, ..., value) standardGeneric("data<-"))
setGeneric("meta<-", \(x, ..., value) standardGeneric("meta<-"))

setGeneric("layer", \(x, i, ...) standardGeneric("layer"))
setGeneric("element", \(x, i, ...) standardGeneric("element"))
setGeneric("elements", \(x, i, ...) standardGeneric("elements"))

setGeneric("query", \(x, ...) standardGeneric("query"))
setGeneric("crop", \(x, y, ...) standardGeneric("crop"))
setGeneric("mask", \(x, i, j, ...) standardGeneric("mask"))

setGeneric("axes", \(x, ...) standardGeneric("axes"))
setGeneric("extent", \(x, ...) standardGeneric("extent"))
setGeneric("channels", \(x, ...) standardGeneric("channels"))
setGeneric("centroids", \(x, ...) standardGeneric("centroids"))
setGeneric("data_type", \(x, ...) standardGeneric("data_type"))
setGeneric("geom_type", \(x, ...) standardGeneric("geom_type"))
setGeneric("multiscales", \(x, ...) standardGeneric("multiscales"))

# tbl ----

setGeneric("hasTable", \(x, i, ...) standardGeneric("hasTable"))
setGeneric("getTable", \(x, i, ...) standardGeneric("getTable"))
setGeneric("setTable", \(x, i, ...) standardGeneric("setTable"))
