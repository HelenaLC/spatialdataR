#' @importClassesFrom S4Arrays Array
setClassUnion("Array_OR_array", c("Array", "array"))

#' @importClassesFrom SingleCellExperiment SingleCellExperiment
setClassUnion("SingleCellExperiment_OR_NULL", c("SingleCellExperiment", "NULL"))

#' @exportClass ZarrArray SpatialData
.ZarrArray <- setClass(
    Class="ZarrArray",
    contains=c("Array", "Annotated"),
    slots=c(data="Array_OR_array", zattrs="list"),
    prototype=list(data=array(), metadata=list(), zattrs=list()))

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
    slots=c("zattrs"))

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

#' #' @importClassesFrom S4Vectors DFrame
#' #' @exportClass CoordFrame SpatialData
#' .CoordFrame <- setClass(
#'     Class="CoordFrame",
#'     contains="DFrame",)
#'
#' #' @rdname CoordFrame
#' #' @importFrom S4Vectors DataFrame
#' CoordFrame <- function(data=list()) {
#'     df <- if (!length(data)) {
        # DataFrame(
        #     name=character(),
        #     type=character(),
        #     data=I(list()))
#'     } else {
        # ms <- data$multiscales
        # df <- if (!is.null(ms)) ms else data
        # df <- df$coordinateTransformations[[1]]
        # df <- DataFrame(
        #     name=df$output$name, type=df$type,
        #     data=I(switch(df$type, identity=list(NA), df[[df$type]])))
#'     }
#'     metadata(df) <- data
#'     .CoordFrame(df)
#' }
#'
#' .validityCoordFrame <- function(obj) {
#'     msg <- NULL
#'     if (!is.null(obj$name)) {
#'         if (!is.character(obj$name)) msg <- c(msg, "'name's should be of type character")
#'     }
#'     if (!is.null(obj$type)) {
#'         if (!is.character(obj$type)) msg <- c(msg, "'type's should be of type character")
#'         typ <- c("identity", "scale")
#'         if (!all(obj$type %in% typ)) {
#'             typ <- paste(dQuote(typ), collapse=", ")
#'             msg <- c(msg, paste("'type' should be in", typ))
#'         }
#'     }
#'     if (!is.null(obj$data)) {
#'         if (!is.list(obj$data)) msg <- c(msg, "'data's should be of type list")
#'     }
#'     if (is.null(msg)) TRUE else msg
#' }
#'
#'
#' #' @importFrom S4Vectors setValidity2
#' setValidity2("CoordFrame", .validityCoordFrame)

# md <- jsonlite::fromJSON("inst/extdata/cosmx_io/images/1_image/.zattrs")
# (df <- CoordFrame(md))
# df$type <- "test"
# df

#' .CoordFrame <- setClass(
#'     Class="CoordFrame",
#'     contains="DFrame",
#'     slots=list(
#'         rownames="NULL",
#'         nrows="integer",
#'         listData="list",
#'         metadata="list",
#'         elementType="character",
#'         elementMetadata="NULL"))
#'
#' CoordFrame <- function(data, metadata=list()) {
#'     l <- if (missing(data)) {
#'         list(
#'             name=character(),
#'             type=character(),
#'             data=I(list()))
#'     } else {
#'         data
#'     }
#'     df <- DataFrame(l)
#'     df <- .CoordFrame(df)
#'     #metadata(df) <- md
#'     return(df)
#' }
#'
#' .validityCoordFrame <- function(obj) {
#'     msg <- NULL
#'     if (is.null(obj$name)) msg <- c(msg, "missing 'name'")
#'     if (is.null(obj$type)) msg <- c(msg, "missing 'type'")
#'     if (is.null(obj$data)) msg <- c(msg, "missing 'data'")
#'     if (is.null(msg)) TRUE else msg
#' }
#'
#' setValidity2("CoordFrame", .validityCoordFrame)
#'
#' .showCoordFrame <- function(object) {
#'     df <- object
#'     cat("Class: CoordFrame\n")
#'     cat(sprintf("Coords(%s):", nrow(df)), "\n")
#'     for (i in seq_len(nrow(df)))
#'         cat(sprintf("|- %s(%s)", df$name[i], df$type[i]), "\n")
#' }
#'
#' setMethod("show", "CoordFrame", .showCoordFrame)
#'
#' #' @importFrom S4Vectors replaceCOLS
#' setMethod("replaceCOLS", "CoordFrame", function(x, i, value) {
#'     if (i %in% c("name", "type", "data")) stop("can't do this")
#'     callNextMethod()
#' })
