# helpers ----

.make_axes_meta <- function(x, unit = FALSE){
  lapply(x, \(.){
    meta <- list(names = .,
                 type = if(. == "c") "channel" else "space")
    if(unit)
      meta <- c(meta, list(unit = "unit"))
    meta
  })
}

.make_empty_ct <- function(x){
  space <- .make_axes_meta(x, unit = TRUE)
  input <- list(axes = space,
                name = paste(x, collapse = ""))
  output <- list(axes = space, name = "global")
  meta <- list(
    list(input = input,
         output = output,
         type = "identity")
  )
  meta
}

# .make_datasets <- function(x, axes){
#   paths <- paste0(seq_len(length(x)) - 1)
#   mapply(\(p) {
#     list(
#       coordinateTransformations = list(
#         list(
#           scale = vapply(axes, \(.){
#             if(. == "c") 1 else (2^as.numeric(p))
#           }, numeric(1)),
#           type = "scale"
#         )
#       ),
#       path = p
#     )
#   }, paths, USE.NAMES = FALSE, SIMPLIFY = FALSE)
# }

# metadata constructors ----
#' @title Make point/shape metadata
#' @description Make point/shape metadata
#' @param x A points or shapes object
#' @param axes A character vector of axes names
#' @param encoding_type A string specifying the encoding type
#' @param feature_key A string specifying the feature key
#' @param instance_key A string specifying the instance key
#' @param version A string specifying the version
#' @return A list of metadata for the point/shape object
#' @importFrom jsonlite fromJSON toJSON
#' @noRd
.make_pointshape_meta <- function(x,
                                  axes = NULL,
                                  encoding_type = "ngff:points",
                                  feature_key = NULL, 
                                  instance_key = NULL, 
                                  version = 0.1){
  meta <- list()
  ax <- "axes"
  ct <- "coordinateTransformations"
  sa <- "spatialdata_attrs"
  
  # axis
  # NOTE: rev dimensions since points and shapes want x, y
  # whereas images and labels want y, x, etc.
  meta[[ax]] <- rev(.get_valid_axes(x, axes = axes, image = FALSE))
  
  # encoding type
  meta[["encoding-type"]] <- encoding_type
  
  # spatialdata_attrs
  meta[[sa]] <-  list(version = version)
  if(!is.null(feature_key)) 
    meta[[sa]][["feature_key"]] <- feature_key
  if(!is.null(instance_key)) 
    meta[[sa]][["instance_key"]] <- instance_key
  
  # coordinate transformations
  meta[[ct]] <- .make_empty_ct(meta[[ax]])
  
  # update json list
  meta <- fromJSON(toJSON(meta, auto_unbox = TRUE), simplifyVector = FALSE)
  Zattrs(meta)
}
# TODO: make it the functions take a global option e.g. sd_zarr_version
# as an argument for the default zarr version

#' @title Make image metadata
#' @description Make image metadata
#' @param x An image object
#' @param axes A character vector of axes names
#' @param version A string specifying the version
#' @return A list of metadata for the image object
#' @importFrom jsonlite fromJSON toJSON
#' @noRd
.make_image_meta <- function(x, 
                             axes = NULL,
                             version = 0.4){
  meta <- list()
  ax <- "axes"
  ct <- "coordinateTransformations"
  ds <- "datasets"
  mt <- "metadata"
  v <- "version"
  n <- "name"
  
  # axis
  axes <- .get_valid_axes(x, axes, image = TRUE)
  meta[[ax]] <- .make_axes_meta(axes, unit = FALSE)
  
  # coordinate transformations
  # TODO: shall we do coordinate transformations only
  # without datasets:coordinateTransformations
  # see https://ngff.openmicroscopy.org/0.4/index.html#multiscale-md
  meta[[ct]] <- .make_empty_ct(axes)
  
  # datasets 
  meta[[ds]] <- .make_datasets(x, axes)
  
  # metadata
  meta[[mt]] <- list(omero = list(
    channels = lapply(seq_len(length(axes))-1, \(.) 
                      list(label = .))
  ))
  
  # name
  meta[[n]] <- ""
  
  # version
  meta[[v]] <-  list(version = version)
  
  # multiscales
  meta <- list(multiscales = list(meta),
               omero = list(
                 channels = lapply(seq_len(length(axes))-1, \(.)
                                   list(label = .))
               ),
               spatialdata_attrs = list(version = "0.1"))

  # update json list
  # meta <- fromJSON(toJSON(meta, auto_unbox = TRUE), simplifyVector = FALSE)
  Zattrs(meta)
}

#' @title Make label metadata
#' @description Make label metadata
#' @param x A label object
#' @param axes A character vector of axes names
#' @param version A string specifying the version
#' @return A list of metadata for the label object
#' @importFrom jsonlite fromJSON toJSON
#' @noRd
.make_label_meta <- function(x, 
                             axes = NULL,
                             version = 0.4){
  meta <- list()
  ax <- "axes"
  ct <- "coordinateTransformations"
  ds <- "datasets"
  v <- "version"
  n <- "name"
  
  # axis
  axes <- .get_valid_axes(x, axes, image = FALSE)
  if(is.null(axes)){
    axes <- c("y", "x")
    axes <- if(length(dim(x)) == 3) c("z", axes) else axes
  }
  meta[[ax]] <- .make_axes_meta(axes, unit = FALSE)
  
  # coordinate transformations
  # TODO: shall we do coordinate transformations only
  # without datasets:coordinateTransformations
  # see https://ngff.openmicroscopy.org/0.4/index.html#multiscale-md
  meta[[ct]] <- .make_empty_ct(axes)
  
  # datasets 
  meta[[ds]] <- .make_datasets(x, axes)
  
  # name
  meta[[n]] <- ""
  
  # version
  meta[[v]] <-  list(version = version)
  
  # multiscales
  meta <- list(`image-label`= list(version = version), 
               multiscales = list(meta), 
               spatialdata_attrs = list(version = "0.1"))
  
  # update json list
  meta <- fromJSON(toJSON(meta, auto_unbox = TRUE), simplifyVector = FALSE)
  Zattrs(meta)
}

#' @title Get valid axes
#' @description Get validated axes
#'
#' @inheritParams write_image
#' 
#' @noRd
.get_valid_axes <- function(
    x,
    axes = NULL,
    image = FALSE
) {
  
  # axes may be string e.g. "tczyx"
  if (is.character(axes) && length(axes) == 1L) 
    axes <- strsplit(axes, "", fixed = TRUE)[[1]]

  # We can guess axes for images, labels, points/shapes
  ndim <- length(.get_dim(x))
  if (is.null(axes)) {
    if (ndim == 2) {
      axes <- c("y", "x")
    } else {
      if(image){
        stop("axes must be provided. Can't be guessed beyond 2D image", 
             call. = FALSE)
      } else {
        if(ndim == 3) {
          axes <- c("z", "y", "x")
        } else {
          stop("axes must be provided. Can't be guessed beyond 2D or 3D data", 
               call. = FALSE)
        }
      }
    } 
  } else {
    if (length(axes) != ndim) {
      stop(
        sprintf("axes length (%d) must match number of dimensions (%d)", 
                length(axes), ndim),
        call. = FALSE
      )
    }
  }
  
  axes
}

# TODO: what is the best way to get the inherint dimension of geometry 
# objects

.get_dim <- function(x){
  if(is.list(x) && length(x) > 0 &&
     !is.matrix(x) && !is.data.frame(x))
    x <- x[[1]]
  if("arrow_OR_df" %in% is(x)){
    return(.get_arrow_dim(x))
  } else if(!is.null(nd <- dim(x))) {
    return(nd)
  } else {
    # TODO: I don't like this message!
    stop("no dimensions!")
  }
}

#' @importFrom sf st_as_sf st_geometry
.get_arrow_dim <- function(x){
  if("geometry" %in% colnames(x)){
    sfx <- st_as_sf(x)
    sfx <- st_geometry(sfx)
    axes <- class(st_geometry(st_as_sf(sfx))[[1]])
    if("XY" %in% axes){
      n_col <- 2
    } else if("XYZ" %in% axes){
      n_col <- 3
    } else{
      stop("No geometry object is detected!")
    } 
  } else {
    axes <- colnames(x)
    axes <- axes[axes %in% c("x", "y", "z")]
    n_col <- length(axes)
  }
  return(c(nrow(x), n_col))
}


  
