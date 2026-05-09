.SpatialDataAttrs <- setClass(
    Class="SpatialDataAttrs",
    contains="list")

#' @importFrom methods setClassUnion
#' @importClassesFrom S4Arrays Array
setClassUnion(
    "array_OR_df",
    c("Array", "array", "data.frame"))

.SpatialDataImage <- setClass(
    Class="SpatialDataImage",
    contains=c("Annotated"),
    slots=list(data="list", meta="SpatialDataAttrs"))

.SpatialDataLabel <- setClass(
    Class="SpatialDataLabel",
    contains=c("Annotated"),
    slots=list(data="list", meta="SpatialDataAttrs"))

# these are 'R6ClassGenerator's;
# this somehow does the trick...
setClass("FileSystemDataset", "VIRTUAL")
setClass("arrow_dplyr_query", "VIRTUAL")
setClass("tbl_duckdb_connection", "VIRTUAL")
setClass("duckspatial_df", "VIRTUAL")
setClass("Table", "VIRTUAL")

# TODO: this isn't great... arrow::open_dataset gives a FileSystemDataset,
# read_parquet gives a Table, dplyr calls give a query, but also wanna
# be able to store a normal data.frame, maybe?
#' @importFrom methods setClassUnion
setClassUnion(
    "arrow_OR_df",
    c("tbl_duckdb_connection", "duckspatial_df", "FileSystemDataset", "Table", "arrow_dplyr_query", "data.frame"))

.SpatialDataPoint <- setClass(
    Class="SpatialDataPoint",
    contains=c("Annotated"),
    slots=list(data="arrow_OR_df", meta="SpatialDataAttrs"))

#' @importClassesFrom S4Vectors DFrame
.SpatialDataShape <- setClass(
    Class="SpatialDataShape",
    contains=c("Annotated"),
    slots=list(data="arrow_OR_df", meta="SpatialDataAttrs"))

setClassUnion("SpatialDataArray", c("SpatialDataImage", "SpatialDataLabel"))
setClassUnion("SpatialDataFrame", c("SpatialDataPoint", "SpatialDataShape"))

setClassUnion("SpatialDataElement", c(
    "SpatialDataImage", "SpatialDataLabel", 
    "SpatialDataPoint", "SpatialDataShape"))

#' @rdname SpatialData
#' @export
.SpatialData <- setClass(
    Class="SpatialData",
    contains=c("list", "Annotated"),
    representation(
        images="list",  # 'SpatialDataImage's
        labels="list",  # 'SpatialDataLabel's
        points="list",  # 'SpatialDataPoint's
        shapes="list",  # 'SpatialDataShape's
        tables="list")) # 'SingleCellExperiment's

. <- c("images", "labels", "points", "shapes", "tables")
names(.LAYERS) <- .LAYERS <- .
