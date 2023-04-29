#' @importClassesFrom S4Arrays Array
setClassUnion("Array_OR_array", c("Array", "array"))

#' @importClassesFrom SingleCellExperiment SingleCellExperiment
setClassUnion("SingleCellExperiment_OR_NULL", c("SingleCellExperiment", "NULL"))

#' @exportClass ImageArray ImageArray
.ImageArray <- setClass(
    Class="ImageArray",
    slots=c(data="Array_OR_array"),
    contains=c("Array", "Annotated"),
    prototype=list(data=array(), metadata=list()))

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
        labels="list", # 'ImageArray's
        shapes="list", # 'DataFrame's
        points="list", # 'ArrowObject's
        table="SingleCellExperiment_OR_NULL"))
