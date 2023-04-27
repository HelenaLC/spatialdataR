readSpatialData <- function(path, ...) {
  layers <- list.dirs(path, recursive=FALSE)
  
  images <- if ("images" %in% layers) {
    lapply(list.dirs(file.path(path, "images"), recursive=FALSE), readImage)
  } else list()
  layers <- if ("labels" %in% layers) {
    lapply(list.dirs(file.path(path, "labels"), recursive=FALSE), readImage)
  } else list()
  
  # shapes <- if ("shapes" %in% layers){
  #   readShapes(file.path(path, "shapes"))
  # }else{
  #   NULL
  # }
  # points <- if("points" %in% layers){
  #   readPoints(file.path(path, "points"))
  # }else{
  #   NULL
  # }
  # table <- if("table" %in% layers){
  #   readTable(file.path(path, "table"))
  # }else{
  #   NULL
  # }
  
  SpatialData(images=images, labels=labels)
}

# path <- "~/packages/ImageArray/inst/extdata/mibitof"
# readSpatialData(path)