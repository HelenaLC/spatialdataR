#' @importClassesFrom S4Arrays Array
setClassUnion("Array_OR_array_OR_df", c("Array", "array", "data.frame"))

#' @importClassesFrom SingleCellExperiment SingleCellExperiment
setClassUnion("SingleCellExperiment_OR_NULL", c("SingleCellExperiment", "NULL"))

.Zattrs <- setClass(
    Class="Zattrs",
    contains="list")

#' @exportClass ZarrArray SpatialData
.ZarrArray <- setClass(
    Class="ZarrArray",
    contains=c("Array", "Annotated"),
    # temporarily supporting pointers,
    # for the purpose of development...
    slots=c(data="Array_OR_array_OR_df", zattrs="Zattrs"))

#' @exportClass ImageArray SpatialData
.ImageArray <- setClass(
    Class="ImageArray",
    contains="ZarrArray")

#' @exportClass LabelArray SpatialData
.LabelArray <- setClass(
    Class="LabelArray",
    contains="ZarrArray")

setClassUnion(
    "ImageArray_OR_LabelArray",
    c("ImageArray", "LabelArray"))

#' @importClassesFrom S4Vectors DFrame
#' @exportClass ShapeFrame SpatialData
.ShapeFrame <- setClass(
    Class="ShapeFrame",
    contains="DFrame",
    slots=c(zattrs="Zattrs"))

setClassUnion(
    "ZarrArray_OR_ShapeFrame",
    c("ZarrArray", "ShapeFrame"))

#' @importFrom arrow Table
#' @importFrom methods setOldClass
setOldClass("Table")
setOldClass("arrow_dplyr_query")

setClassUnion(
    "Table_OR_df",
    c("Table", "data.frame", "arrow_dplyr_query"))

#' @exportClass PointFrame SpatialData
.PointFrame <- setClass(
    Class="PointFrame",
    contains=c("Table", "Annotated"),
    slots=list(data="Table_OR_df", zattrs="Zattrs"))

# making it easier for methods shared b/w all elements
setClassUnion(
    "SpatialDataElement",
    c("ZarrArray", "ShapeFrame", "PointFrame"))

#' @exportClass SpatialData SpatialData
.SpatialData <- setClass(
    Class="SpatialData",
    contains="Annotated",
    representation(
        images="list", # 'ImageArray's
        labels="list", # 'LabelArray's
        shapes="list", # 'DataFrame's
        points="list", # 'ArrowObject's
        table="SingleCellExperiment_OR_NULL"))
