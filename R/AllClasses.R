#' @importClassesFrom S4Arrays Array
setClassUnion("Array_OR_array", c("Array", "array"))

#' @importClassesFrom SingleCellExperiment SingleCellExperiment
setClassUnion("SingleCellExperiment_OR_NULL", c("SingleCellExperiment", "NULL"))

#' @exportClass ZarrArray SpatialData
.ZarrArray <- setClass(
    Class="ZarrArray",
    slots=c(data="Array_OR_array"),
    contains=c("Array", "Annotated"),
    prototype=list(data=array(), metadata=list()))

#' @exportClass ImageArray SpatialData
.ImageArray <- setClass(
    Class="ImageArray",
    contains="ZarrArray")

#' @exportClass LabelArray SpatialData
.LabelArray <- setClass(
    Class="LabelArray",
    contains="ZarrArray")

setClassUnion("ImageArray_OR_LabelArray", c("ImageArray", "LabelArray"))

#' @importClassesFrom S4Vectors DFrame
#' @exportClass ShapeFrame SpatialData
.ShapeFrame <- setClass(
    Class="ShapeFrame",
    contains="DFrame")

#' @exportClass SpatialData SpatialData
.SpatialData <- setClass(
    Class="SpatialData",
    contains="Annotated",
    prototype=list(
        metadata=list(),
        images=list(),
        labels=list(),
        shapes=list(),
        points=list(),
        table=NULL),
    representation(
        images="list", # 'ImageArray's
        labels="list", # 'LabelArray's
        shapes="list", # 'DataFrame's
        points="list", # 'ArrowObject's
        table="SingleCellExperiment_OR_NULL"))
