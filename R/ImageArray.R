#' @name ImageArray
#' @title The `ImageArray` class
#' 
#' @param x \code{ImageArray}
#' @param data list of \code{\link[Rarr]{ZarrArray}}s
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param scale scalar index specifying which resolution to extract.
#' @param ... option arguments passed to and from other methods.
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
#' @importFrom methods new
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
setMethod("data", "ImageArray", \(x, i=1) {
    if (i <= (n <- length(x@data))) return(x@data[[i]])
    stop("'scale", i, "' but only ", n, " resolution(s) available")
})

#' @rdname ImageArray
#' @export
setMethod("dim", "ImageArray", \(x) dim(data(x)))

#' #' @rdname ImageArray
#' #' @exportMethod [
#' setMethod("[", "ImageArray", \(x, i, j, k, ..., drop=FALSE) {
#'     # TODO: subsetting for multiscales
#'     if (missing(i)) i <- TRUE
#'     if (missing(j)) j <- TRUE
#'     if (missing(k)) k <- TRUE
#'     # get scale factor between pyramid layers
#'     is <- seq_along(x@data)
#'     as <- lapply(is, \(.) data(x, .))
#'     ds <- vapply(as, dim, numeric(3))
#'     sf <- if (length(is) == 1) 1 else {
#'         cumprod(vapply(
#'         is[-1], \(.) ds[,.]/ds[,.-1], numeric(3))[, 1])
#'     }
#'     # validity
#'     if (isTRUE(j)) j <- seq(ds[2,1])
#'     if (isTRUE(k)) k <- seq(ds[3,1])
#'     # for (. in seq_along(ij <- list(j=j, k=k)))
#'     #     if ((ds[.+1,1] %% length(ij[[.]])) != 0 |
#'     #         max(ij[[.]]) %% (min(ds[.+1,])*min(sf)) != 0)
#'     #         stop("invalid '", names(ij)[.], "'")
#'     for (. in seq_along(sf)) {
#'         .j <- if (!isTRUE(j)) unique(ceiling(j*sf[.])) else j
#'         .k <- if (!isTRUE(k)) unique(ceiling(k*sf[.])) else k
#'         x@data[[.]] <- data(x, .)[i, .j, .k, drop=FALSE]
#'     }
#'     return(x)
#' })

.guess_scale <- \(x, w, h) {
    lys <- vapply(x@data, \(a) dim(a)[2] > h & dim(a)[3] > w, logical(1))
    if (any(idx <- which(lys))) max(idx) else 1
}

.get_plot_data <- \(x, k=NULL, width=800, height=800) {
    if (!is.null(k)) return(data(x, k))
    data(x, .guess_scale(x, width, height))
}
