#' @importFrom methods setClass setClassUnion setOldClass

#' @export
#' @rdname SpatialData
.SpatialData <- setClass(
  Class="SpatialData",
  contains=c("list", "Annotated"),
  representation(
    images="list",  # 'SpatialDataImage's
    labels="list",  # 'SpatialDataLabel's
    points="list",  # 'SpatialDataPoint's
    shapes="list",  # 'SpatialDataShape's
    tables="list")) # 'SingleCellExperiment's

.LAYERS <- `names<-`(. <- c("images","labels","points","shapes","tables"), .)
.SpatialDataAttrs <- setClass("SpatialDataAttrs", contains="list")
setOldClass("duckspatial_df")

setClass("SpatialDataArray", 
         contains=c("Annotated", "VIRTUAL"),
         slots=list(data="ImageArray", meta="SpatialDataAttrs"))

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