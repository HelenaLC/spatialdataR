#' @importClassesFrom S4Arrays Array
setClassUnion("Array_OR_array", c("Array", "array"))
setClassUnion("SingleCellExperiment_OR_NULL", c("SingleCellExperiment", "NULL"))

#' @export
.ImageArray <- setClass("ImageArray",
  slots = c(data = "Array_OR_array"),
  contains = c("Array", "Annotated"),
  prototype = list(metadata = list()))

#' @export
.SpatialData <- setClass("SpatialData",
  contains = "Annotated",
  prototype = list(
    metadata = list(),
    images = list(),
    labels = list(),
    shapes = list(),
    points = list(),
    table = NULL),
  representation(
    images = "list", # list of ImageArrays
    labels = "list", # list of ImageArrays
    shapes = "list", # DataFrame
    points = "list", # arrow lazy tibble
    table = "SingleCellExperiment_OR_NULL")
)
