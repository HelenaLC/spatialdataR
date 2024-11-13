#' @name ImageArray
#' @title The `ImageArray` class
#' 
#' @param data_list a list of arrays
#' @param meta ...
#' @param metadata ....
#'
#' @return \code{ImageArray}
#'
#' @examples
#' dir.create(td <- tempfile())
#' pa <- unzip_merfish_demo(td)
#' pa <- file.path(pa, "images", "rasterized")
#' (ia <- readImage(pa))
#'
#' @importFrom S4Vectors metadata<-
#' @export
ImageArray <- function(data=list(), meta=Zattrs(), metadata=list(), ...) {
    x <- .ImageArray(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @importFrom S4Vectors isSequence
.get_multiscales_dataset_paths <- function(md) {
  
    # validate multiscales attributes
    .validate_multiscales_dataset_path(md)
  
    # get paths
    paths <- md$multiscales$datasets[[1]]$path
    paths <- suppressWarnings({as.numeric(sort(paths, decreasing=FALSE))})
  
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
.validate_multiscales_dataset_path <- function(md) {
    # validate 'multiscales' 
    if ("multiscales" %in% names(md)) {
        ms <- md[["multiscales"]]
    
        # validate 'datasets' 
        if("datasets" %in% names(ms)) {
          ds <- ms[["datasets"]]
          
          # validate 'paths'
          valid <- vapply(ds, \(ds) "path" %in% colnames(ds), logical(1))
          
          if (!all(valid)) {
            stop("'ImageArray' paths are ill-defined,",
                " no 'path' attribute under 'multiscale-datasets'")
          } 
          
        } else {
            stop("'ImageArray' paths are ill-defined,",
                " no 'datasets' attribute under 'multiscale'")
        }
    } else {
        stop("'ImageArray' paths are ill-defined,",
            " no 'multiscales' attribute under '.zattrs'")
    }
}

#' @rdname ImageArray
#' @export
setMethod("data", "ImageArray", \(x, scale=1) x@data[[scale]])

#' @rdname ImageArray
#' @export
setMethod("dim", "ImageArray", \(x) dim(data(x)))

.guess_scale <- \(x, width, height) {
    dim_list <- vapply(x@data, \(a) {
        dim_a <- dim(a)
        (dim_a[2] > height & dim_a[3] > width)
    }, logical(length(x@data)))
    dim_list_ind <- which(dim_list)
    if (any(dim_list_ind))
        return(max(dim_list_ind))
    return(1)
}

.get_plot_data <- \(x, width=800, height=800) {
    image_scale_ind <- .guess_scale(x, width, height)
    x@data[[image_scale_ind]]
}
