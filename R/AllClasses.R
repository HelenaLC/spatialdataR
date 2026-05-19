#' @importFrom methods setClass setClassUnion setOldClass

ele_typ <- list(
    Image="SpatialDataImage",
    Label="SpatialDataLabel",
    Point="SpatialDataPoint",
    Shape="SpatialDataShape",
    Table="SingleCellExperiment")

for (ele in names(ele_typ)) {
    cnm <- sprintf("sd%sList", ele)
    typ <- ele_typ[[ele]]
    setClass(cnm,
        contains="SimpleList",
        prototype=prototype(elementType=typ))
    fun <- eval(substitute(\(...) new(.cnm, listData=list(...))), list(.cnm=cnm))
    assign(cnm, fun, envir=parent.env(environment()))
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
