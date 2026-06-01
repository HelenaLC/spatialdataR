#' @name SpatialData
#' @title The `SpatialData` class
#' 
#' @aliases data meta 
#' @aliases layer element element<-
#' @aliases image label point shape table
#' @aliases images labels points shapes tables
#' @aliases image<- label<- point<- shape<- table<-
#' @aliases images<- labels<- points<- shapes<- tables<-
#' @aliases imageNames labelNames pointNames shapeNames tableNames
#' @aliases imageNames<- labelNames<- pointNames<- shapeNames<- tableNames<-
#' @aliases [[<-,SpatialData,character,ANY-method
#' @aliases [[<-,SpatialData,numeric,ANY-method
#' 
#' @description 
#' \code{SpatialData} provides an R interface to Python's \code{spatialdata},
#' which enables the representation of diverse spatial omics datasets using 
#' the OME-NGFF (Next Generation File Format) standard. In R, 
#' \itemize{
#' \item images and labels are \code{ZarrArray}s (\code{Rarr} package).
#' \item points and shapes are managed using \code{duckspatial} tables.
#' \item tables are \code{SingleCellExperiment}s (read with \code{anndataR}).}
#' 
#' @param images list of \code{\link{SpatialDataImage}}s
#' @param labels list of \code{\link{SpatialDataLabel}}s
#' @param points list of \code{\link{SpatialDataPoint}}s
#' @param shapes list of \code{\link{SpatialDataShape}}s
#' @param tables list of \code{SingleCellExperiment}s
#' @param x,object \code{SpatialData} object.
#' @param i,j character string, scalar or vector of indices
#'   specifying the element to extract from a given layer.
#' @param drop ignored.
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param value (list of) element(s) with layer-compliant object(s), 
#'   or NULL/\code{list()} to remove an element/layer completely; 
#'   for \code{element<-}, a single \code{SpatialDataElement}
#'   of the same class as \code{element(x, i)}.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return \code{SpatialData}
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' (x <- readSpatialData(x))
#' 
#' # subsetting
#' # layers are taken in order of appearance
#' # (images, labels, points, shapes, tables)
#' x[-4] # drop layer
#' x[4, -2] # drop element
#' x["shapes", c(1, 3)] # subset layer
#' x[c(1, 2), list(1, c(1, 2))] # multiple
#' 
#' @export
SpatialData <- \(
    images=list(), 
    labels=list(), 
    points=list(), 
    shapes=list(), 
    tables=list())
{
    sd <- .SpatialData()
    for (l in .LAYERS)
        sd[[l]] <- get(l)
    return(sd)
}
