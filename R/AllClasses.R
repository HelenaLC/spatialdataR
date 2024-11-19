.Zattrs <- setClass(
    Class="Zattrs",
    contains="list")

#' @importFrom methods setClassUnion
#' @importClassesFrom S4Arrays Array 
setClassUnion(
    "array_OR_df",
    c("Array", "array", "data.frame"))

.ImageArray <- setClass(
    Class="ImageArray",
    contains=c("Annotated"),
    slots=list(data="list", meta="Zattrs"))

.LabelArray <- setClass(
    Class="LabelArray",
    contains=c("Annotated"),
    slots=list(data="array_OR_df", meta="Zattrs"))

# these are 'R6ClassGenerator's;
# this somehow does the trick...
setClass("FileSystemDataset", "VIRTUAL")
setClass("arrow_dplyr_query", "VIRTUAL")
setClass("Table", "VIRTUAL")

# TODO: this isn't great... arrow::open_dataset gives a FileSystemDataset,
# read_parquet gives a Table, dplyr calls give a query, but also wanna 
# be able to store a normal data.frame, maybe?
#' @importFrom methods setClassUnion
setClassUnion(
    "arrow_OR_df",
    c("FileSystemDataset", "Table", "arrow_dplyr_query", "data.frame"))

.PointFrame <- setClass(
    Class="PointFrame",
    contains=c("Annotated"),
    slots=list(data="arrow_OR_df", meta="Zattrs"))

#' @importClassesFrom S4Vectors DFrame
.ShapeFrame <- setClass(
    Class="ShapeFrame",
    contains=c("Annotated"),
    slots=list(data="arrow_OR_df", meta="Zattrs"))

setClassUnion(
    "SpatialDataElement",
    c("ImageArray", "LabelArray", "PointFrame", "ShapeFrame"))

#' @rdname SpatialData
#' @export
.SpatialData <- setClass(
    Class="SpatialData",
    contains=c("list", "Annotated"),
    representation(
        images="list",  # 'ImageArray's
        labels="list",  # 'LabelArray's
        shapes="list",  # 'ShapeFrame's
        points="list",  # 'PointFrame's
        tables="list")) # 'SingleCellExperiment's
