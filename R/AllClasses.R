#' @importClassesFrom S4Arrays Array
setClassUnion("Array_OR_array", c("Array", "array"))

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
    points = list()),
  representation(
    images = "list", # list of ImageArrays
    labels = "list", # same
    shapes = "list", # tbd
    points = "list") # tbd
    #sce = "SingleCellExperiment"
)