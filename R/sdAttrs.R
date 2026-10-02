#' @name SpatialDataAttrs
#' @title The `SpatialDataAttrs` class
#' 
#' @aliases region region<- 
#' @aliases regions regions<- 
#' @aliases instances instances<- 
#' @aliases region_key region_key<- 
#' @aliases feature_key feature_key<- 
#' @aliases instance_key instance_key<- 
#' 
#' @param x element or list extracted from a OME-NGFF compliant .zattrs file.
#' @param name character string for extraction (see ?base::`$`).
#' @param type character string; either "image", "label", "point" or "shape"
#' @param trans list of coordinate transformations; defaults to identity only.
#' @param value character string (for one \code{region} and \code{_key}s), 
#'   or vector (for many \code{region}s, \code{instances} and \code{regions}).
#' @param ver character string; specifies the version of the SpatialData 
#'   element to comply with.
#' @param dim scalar integer in 2-4 when \code{type} is either "image" or 
#'   "label", and 2-3 when "shape" or "point";
#'   number of dimensions: 2 = XY, 3 adds Z, 4 adds T (time) for image and 
#'   label; when \code{type="image"}, C (channel) will be added (for any 
#'   \code{dim}).
#' @param nch scalar integer; how many channels should there be?
#'   (ignored if \code{type="label"}, \code{type="shape"}, or 
#'   \code{type="point"}). 
#' 
#' @details 
#' When \code{x} is a spatial element, the following applies:
#' \code{SpatialDataFrame}: \code{feature/instance_key},
#' \code{SingleCellExperiment}: \code{region}, \code{region/instance_key}.
#' 
#' When missing \code{x}, \code{SpatialDataAttrs} will generate a valid object 
#' with default axes (image: cyx, label: yx, point/shape: xy) and 
#' transformations (identify) according to the specified type.
#' 
#' @return SpatialDataAttrs object
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x)
#' 
#' # tables
#' region(table(x))
#' region_key(table(x))
#' 
#' # points
#' instance_key(point(x))
#' fk <- feature_key(point(x))
#' base::table(point(x)[[fk]])
#' 
#' # transformations
#' (z <- meta(label(x)))
#' CTname(z)
#' CTtype(z)
#' CTdata(z, "scale")
#' 
#' # constructor
#' SpatialDataAttrs(type="point")
#' SpatialDataAttrs(type="shape")
#' SpatialDataAttrs(type="image", nch=7)
#' SpatialDataAttrs(type="label", dim=3)
#' 
#' @export
SpatialDataAttrs <- \(x, type=c("image", "label", "point", "shape"), 
    trans=NULL, ver=NULL, dim=2, nch=3) 
{
    if (!missing(x)) return(.SpatialDataAttrs(x))
    type <- match.arg(type)
    stopifnot(
        length(dim) == 1, is.numeric(dim), 
        dim %in% seq(2, if(type %in% c("point", "shape")) 3 else 4),
        length(nch) == 1, is.numeric(nch), round(nch) == nch, nch > 0)
    if(is.null(ver)) ver <- if(type == "point") "0.2" else "0.3"
    ver <- .val_sd_ver(ver, type)
    ax <- .default_ax(type, dim)
    # transformations:
    ct <- trans %||% .default_ct(ax)
    # zarr attributes list:
    if (!type %in% c("point", "shape")) {
        # datasets:
        ds <- .default_ds(.ax_names(ax)) 
        # default structure
        res <- list()
        if(type != "label")
          res <- c(res,
                   list(omero=list(channels=lapply(letters[seq_len(nch)], 
                                                   \(.) list(label = .)))))
        res <- c(res,
                 list(
                   version=.get_ome_version(ver),
                   multiscales=
                     list(
                       list(
                         axes=ax,
                         coordinateTransformations=ct,
                         datasets=ds
                       )
                     )
                 )
        )
        if (ver == "0.3") res <- list(ome=res)
    } else {
        # points/shapes
        res <- list(
          axes=.ax_names(ax), # point and shape take only names
          coordinateTransformations=ct
        )
    }
    res$spatialdata_attrs <- list(version=ver)
    SpatialDataAttrs(res)
}

# Internal helper to generate OME-NGFF axes
.default_ax <- \(type=c("image", "label", "point", "shape"), dim=2) {
    c <- list(name="c", type="channel")
    t <- list(name="t", type="time")
    z <- list(name="z", type="space")
    y <- list(name="y", type="space")
    x <- list(name="x", type="space")
    switch(match.arg(type), 
        # xyz for points/shapes
        point=,
        shape={
            ax <- list(x, y)
            if (dim > 2) {
                ax <- c(ax, list(z))
            }
        },
        # tczyx for images/labels
        {
            ax <- list(y, x)
            if (dim > 2) {
                ax <- c(list(z), ax)
                if (dim > 3) ax <- c(list(t), ax)
            }
            if (type == "image") ax <- c(list(c), ax)
        }
    )
    return(ax)
}

# Internal helper to get axes names
.ax_names <- function(ax){
  if (is.character(ax[[1]])) {
    unlist(ax)
  } else {
    vapply(ax, \(.) .$name, character(1))
  }
}

# Internal helper to generate coordinate transformations
.default_ct <- \(axes, name="global", type="identity", data=NULL) {
    ct <- list(input=list(axes=axes, 
                          name=paste(.ax_names(axes), collapse = "")), 
               output=list(axes=axes, 
                           name=name), 
               type=type)
    if (!is.null(data)) ct[[type]] <- data
    list(ct)
}

# Internal helper to generate datasets
.default_ds <- function(axes, scale_factors = NULL){
  scale_factors <- cumprod(c(1,scale_factors))
  paths <- paste0(seq_along(scale_factors) - 1)
  mapply(\(p,s) {
    list(
      coordinateTransformations = list(
        list(
          scale = lapply(
            axes,
            \(.) if(. == "c") 1 else s),
          type = "scale"
        )
      ),
      path = p
    )
  }, paths, scale_factors, USE.NAMES = FALSE, SIMPLIFY = FALSE)
}

#' @export
#' @importFrom utils .DollarNames
.DollarNames.SpatialDataAttrs <- \(x, pattern="") names(x)

#' @rdname SpatialDataAttrs
#' @exportMethod $
setMethod("$", "SpatialDataAttrs", \(x, name) x[[name]])

# internal use only!
#' @noRd 
.ome_ver <- \(x) {
    v <- 
        x$multiscales[[1]]$version %||%
        x$omero$version %||% 
        x$ome$version
    if (!length(v)) stop("couldn't find 'version' in 'spatialdata_attrs'")
    v <- .val_ome_ver(v)
    return(v)
}

.get_ome_version <- \(x){
  switch(as.character(x), 
         "0.1" = "0.4",
         "0.2" = "0.4-dev-spatialdata",
         "0.3" = "0.5-dev-spatialdata",
         stop("Invalid SpatialDataImage/Label format! ", 
              "Must be 0.1, 0.2, or 0.3"))
}

# internal use only!
#' @noRd 
setMethod("multiscales", "list", \(x) {
    v <- tryCatch(.ome_ver(x), error=\(e) NULL)
    if (is.null(v)) return()
    switch(v, "0.5"=x$ome$multiscales, x$multiscales)
})

# internal use only!
#' @noRd 
setMethod("datasets", "list", \(x, ...) {
    ds <- .get_ms(x)$datasets
    vapply(ds, \(.) .$path, character(1))
})

# features ----

#' @export
#' @rdname SpatialDataAttrs
setMethod("feature_key", "SpatialDataPoint", \(x) feature_key(meta(x)))
#' @export
#' @rdname SpatialDataAttrs
setMethod("feature_key", "SpatialDataAttrs", \(x) x$spatialdata_attrs$feature_key)
#' @export
#' @rdname SpatialDataAttrs
setReplaceMethod("feature_key", c("SpatialDataAttrs", "character"), 
    \(x, value) { x$spatialdata_attrs$feature_key <- value; x })

# region(s) ----

#' @export
#' @rdname SpatialDataAttrs
setMethod("region_key", "SingleCellExperiment", \(x) meta(x)$region_key)

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region_key", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(length(value) == 1, nzchar(value))
    int_metadata(x)$spatialdata_attrs$region_key <- value
    return(x)
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region_key", c("SingleCellExperiment", "NULL"), \(x, value) {
    int_metadata(x)$spatialdata_attrs$region_key <- value
    return(x)
})

#' @export
#' @rdname SpatialDataAttrs
setMethod("region", "SingleCellExperiment", \(x) meta(x)[["region"]])

#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData
setMethod("regions", "SingleCellExperiment", \(x) {
    rk <- region_key(x)
    if (is.null(rk)) return(NULL)
    int_colData(x)[[rk]]
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(all(nzchar(value), na.rm=TRUE))
    if (is.null(rk <- region_key(x))) 
        rk <- region_key(x) <- "region"
    int_metadata(x)$spatialdata_attrs[[rk]] <- sort(unique(value))
    return(x)
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region", c("SingleCellExperiment", "NULL"), \(x, value) {
    if (!is.null(rk <- region_key(x)))
        int_metadata(x)$spatialdata_attrs[[rk]] <- value
    return(x)
})

#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("regions", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(length(value) %in% c(1, ncol(x)))
    stopifnot(all(nzchar(value) > 0, na.rm=TRUE))
    if (is.null(rk <- region_key(x))) region_key(x) <- "region"
    int_metadata(x)$spatialdata_attrs[[rk]] <- sort(unique(value))
    int_colData(x)[[rk]] <- value
    return(x)
})

#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("regions", c("SingleCellExperiment", "NULL"), \(x, value) {
    if (!is.null(rk <- region_key(x))) {
        int_metadata(x)$spatialdata_attrs[[rk]] <- value
        int_colData(x)[[rk]] <- value
    }
    region_key(x) <- value
    return(x)
})

# instances ----

# NOTE: does not apply to images
#' @export
#' @rdname SpatialDataAttrs
setMethod("instance_key", "list", \(x) x$instance_key)
#' @export
#' @rdname SpatialDataAttrs
setMethod("instance_key", "SingleCellExperiment", \(x) instance_key(meta(x)))
#' @export
#' @rdname SpatialDataAttrs
setMethod("instance_key", "SpatialDataFrame", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SpatialDataAttrs
setMethod("instance_key", "SpatialDataLabel", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SpatialDataAttrs
setReplaceMethod("instance_key", c("SpatialDataAttrs", "character"), \(x, value) {
    x$spatialdata_attrs$instance_key <- value
    return(x)
})
#' @export
#' @rdname SpatialDataAttrs
setReplaceMethod("instance_key", c("SingleCellExperiment", "character"), \(x, value) {
    int_metadata(x)$spatialdata_attrs$instance_key <- value
    return(x)
})

#' @export
#' @rdname SpatialDataAttrs
setMethod("instances", "SpatialDataLabel", \(x) {
    # unique values in first scale, excluding 0
    z <- data(x, 1)
    as.integer(setdiff(unique(as.vector(z)), 0))
})
#' @export
#' @rdname SpatialDataAttrs
#' @importFrom dplyr pull
setMethod("instances", "SpatialDataPoint", \(x) pull(data(x), instance_key(x)))
#' @export
#' @rdname SpatialDataAttrs
setMethod("instances", "SpatialDataShape", \(x) {
    ik <- tryCatch(instance_key(x), error=\(e) NULL)
    if (is.null(ik)) return(seq_len(nrow(x)))
    pull(data(x), ik)
})
#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData
setMethod("instances", "SingleCellExperiment", \(x) {
    if (is.null(ik <- instance_key(x))) 
        stop("no 'instance_key' found in 'x'")
    int_colData(x)[[ik]]
})

#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("instances", c("SingleCellExperiment", "ANY"), \(x, value) {
    ik <- instance_key(x)
    if (is.null(ik)) 
        ik <- "instance_id"
    int_colData(x)[[ik]] <- value
    return(x)
})
