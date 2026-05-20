#' @importFrom methods setClass setClassUnion setOldClass

.sdImageList <- setClass(
    Class="sdImageList",
    contains="SimpleList",
    prototype=prototype(elementType="SpatialDataImage"))

.sdLabelList <- setClass(
    Class="sdLabelList",
    contains="SimpleList",
    prototype=prototype(elementType="SpatialDataLabel"))

.sdPointList <- setClass(
    Class="sdPointList",
    contains="SimpleList",
    prototype=prototype(elementType="SpatialDataPoint"))

.sdShapeList <- setClass(
    Class="sdShapeList",
    contains="SimpleList",
    prototype=prototype(elementType="SpatialDataShape"))

.sdTableList <- setClass(
    Class="sdTableList",
    contains="SimpleList",
    prototype=prototype(elementType="SingleCellExperiment"))

.sl <- S4Vectors:::new_SimpleList_from_list
.ok <- \(x) length(x) == 1L && is.list(x[[1L]]) || is(x[[1L]], "SimpleList")

sdImageList <- \(...) {
    x <- list(...)
    if (.ok(x)) x <- x[[1L]]
    .sl("sdImageList", as.list(x))
}

sdLabelList <- \(...) {
    x <- list(...)
    if (.ok(x)) x <- x[[1L]]
    .sl("sdLabelList", as.list(x))
}

sdPointList <- \(...) {
    x <- list(...)
    if (.ok(x)) x <- x[[1L]]
    .sl("sdPointList", as.list(x))
}

sdShapeList <- \(...) {
    x <- list(...)
    if (.ok(x)) x <- x[[1L]]
    .sl("sdShapeList", as.list(x))
}

sdTableList <- \(...) {
    x <- list(...)
    if (.ok(x)) x <- x[[1L]]
    .sl("sdTableList", as.list(x))
}

#' @export
#' @rdname SpatialData
.SpatialData <- setClass(
    Class="SpatialData",
    contains=c("list", "Annotated"),
    representation(
        images="sdImageList",
        labels="sdLabelList",
        points="sdPointList",
        shapes="sdShapeList",
        tables="sdTableList")) 

.LAYERS <- `names<-`(. <- c("images","labels","points","shapes","tables"), .)
.SpatialDataAttrs <- setClass("SpatialDataAttrs", contains="list")
setOldClass("duckspatial_df")

setClass("SpatialDataArray", 
    contains=c("Annotated", "VIRTUAL"),
    slots=list(data="list", meta="SpatialDataAttrs"))

setClass("SpatialDataFrame",
    contains=c("Annotated", "VIRTUAL"),
    slots=list(data="duckspatial_df", meta="SpatialDataAttrs"))

.SpatialDataImage <- setClass("SpatialDataImage", contains="SpatialDataArray")
.SpatialDataLabel <- setClass("SpatialDataLabel", contains="SpatialDataArray")

.SpatialDataPoint <- setClass("SpatialDataPoint", contains="SpatialDataFrame")
.SpatialDataShape <- setClass("SpatialDataShape", contains="SpatialDataFrame")

setClassUnion("SpatialDataElement", c(
    "SpatialDataImage", "SpatialDataLabel", 
    "SpatialDataPoint", "SpatialDataShape"))
