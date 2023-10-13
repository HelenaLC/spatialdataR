#' @importClassesFrom S4Arrays Array 
setClassUnion(
    "Array_OR_array_OR_df",
    c("Array", "array", "data.frame"))

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

#' @importClassesFrom S4Vectors DFrame
#' @exportClass ShapeFrame SpatialData
.ShapeFrame <- setClass(
    Class="ShapeFrame",
    contains="DFrame",
    slots=c(zattrs="Zattrs"))

setClassUnion(
    "ImageArray_OR_LabelArray",
    c("ImageArray", "LabelArray"))

setClassUnion(
    "ZarrArray_OR_ShapeFrame",
    c("ZarrArray", "ShapeFrame"))

#' @importFrom arrow Table
#' @importFrom methods setOldClass
setOldClass("Table")

# 'arrow' doesn't export this class;
# this somehow does the trick...
setClass("arrow_dplyr_query", "VIRTUAL")

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
        images="list",  # 'ImageArray's
        labels="list",  # 'LabelArray's
        shapes="list",  # 'ShapeFrame's
        points="list",  # 'PointFrame's
        tables="list")) # 'SingleCellExperiment's
