#' @name ImageArray
#' @title The `ImageArray` class
#' 
#' @param x zarr store of the image
#' @param .. additional arguements
#'
#' @return \code{ImageArray}
#'
#' @examples
#' dir.create(td <- tempfile())
#' pa <- unzip_merfish_demo(td)
#' pa <- file.path(pa, "images", "rasterized")
#' (ia <- readImage(pa))
#' 
#' a <- as.array(data(ia))
#' a <- aperm(a, c(2,3,1))
#' plot(EBImage::Image(a/255))
#'
#' @importFrom S4Vectors metadata<-
#' @export
ImageArray <- function(x, ...) {
  
    # get .zattr
    md <- fromJSON(file.path(x, ".zattrs"))
    paths <- .get_multiscales_dataset_paths(md)
    
    # get ZarrArrays
    za_list <- sapply(paths, function(ps){
      ZarrArray(file.path(x, as.character(ps)))
    })

    # create ImageArray from ZarrArray list
    x <- .ImageArray(data=za_list, meta=Zattrs(md), ...)
    # metadata(x) <- metadata
    return(x)
}

#' @importFrom S4Vectors isSequence
#' 
#' @noRd
.get_multiscales_dataset_paths <- function(md){
  
  # validate multiscales attributes
  .validate_multiscales_dataset_path(md)
  
  # get paths
  paths <- md$multiscales$datasets[[1]]$path
  paths <- suppressWarnings({as.numeric(sort(paths, decreasing = FALSE))})
  
  # TODO: how to check if a vector of values here are integers
  # check paths and return
  # if(all(paths %% 0 == 0)){
  #   if(S4Vectors::isSequence(paths))
  #     return(paths) 
  # }
  return(paths)
  
  # stop if not a sequence of integers
  stop("ImageArray paths are ill-defined, should be e.g. 0,1,2, ..., n")
}

#' @noRd
.validate_multiscales_dataset_path <- function(md){
  
  # validate multiscales 
  if("multiscales" %in% names(md)){
    md_multiscales <- md[["multiscales"]]
    
    # validate datasets 
    if("datasets" %in% names(md_multiscales)){
      md_datasets <- md_multiscales[["datasets"]]
      
      # validate paths
      path_check <- sapply(md_datasets, function(ds){
        if("path" %in% colnames(ds))
          return(TRUE)
        return(FALSE)
      })
      
      if(!all(path_check)){
        stop("ImageArray paths are ill-defined, no 'path' attribute under 'multiscale-datasets'")
      }
    } else {
      stop("ImageArray paths are ill-defined, no 'datasets' attribute under 'multiscale'")
    }
  } else {
    stop("ImageArray paths are ill-defined, no 'multiscales' attribute under '.zattrs'")
  }
}

#' @rdname ImageArray
#' @export
setMethod("dim", "ImageArray", \(x) dim(data(x)))
