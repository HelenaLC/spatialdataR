#' @name Zattrs
#' @title The `Zattrs` class
#' 
#' @param x list extracted from a OME-NGFF compliant .zattrs file.
#' @param name character string for extraction (see ?base::`$`).
#' @param type character string; either "array" (image/label) or "frame" (point/shape).
#' @param axes list of axes; if NULL, defaults to cyx (array) or xy (frame).
#' @param transformations list of transformations; if NULL, defaults to global identity.
#' @param ... additional attributes (e.g., version, feature_key).
#' 
#' @details 
#' When missing \code{x}, \code{Zattrs} will generate a valid object with
#' default axes (array: cyx, frame: xy) and transformations (identify) 
#' according to the specified type.
#' 
#' @return \code{Zattrs}
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' (z <- meta(label(x)))
#' 
#' CTname(z)
#' CTtype(z)
#' CTdata(z, "scale")
#' 
#' feature_key(point(x))
#' 
#' # constructor
#' Zattrs(type="frame")
#' Zattrs(type="array")
#' Zattrs(type="array", n=7)
#' Zattrs(type="array", label=TRUE)
#' 
#' @export
Zattrs <- function(x, type=c("array", "frame"), label=FALSE, trans=NULL, 
                   ver="0.3", n=3, scale_factors = NULL, ...) {
    if (!missing(x)) return(.Zattrs(x))
    type <- match.arg(type)
    # axes:
    ax <- .default_ax(type, label)
    # transformations:
    ct <- trans %||% .default_ct(ax)
    ds <- .default_ds(.ax_names(ax), scale_factors) 
    # .zattrs list:
    if (type == "array") {
        # default structure
        res <- list()
        if(!label)
          res <- c(res,
                   list(omero=list(channels=lapply(letters[seq_len(n)], 
                                                   \(.) list(label = .)))))
        res <- c(res,
                 list(
                   multiscales=
                     list(
                       list(
                         axes=ax,
                         version="0.4",
                         coordinateTransformations=ct,
                         datasets=ds
                         )
                       )
                   )
                 )
        if (ver == "0.3") res <- list(ome=res)
    } else {
        # points/shapes
        res <- list(axes=ax, coordinateTransformations=ct)
    }
    res$spatialdata_attrs <- list(version=ver)
    Zattrs(res)
}

# Internal helper to generate OME-NGFF axes
.default_ax <- \(type=c("array", "frame"), label = FALSE) {
    switch(match.arg(type),
        # (c)yx for images/labels
        array={
          ax <-  list(
            list(name="x", type="space"), 
            list(name="y", type="space"))
          if (type == "array") {
            # yx for labels
            ax <- rev(ax)
            # yx for images, cyx if requested
            if (!label) ax <- c(list(list(name="c", type="channel")), ax)
          }
          ax
        },
        # xy for points/shapes
        list("x", "y"))
}

.ax_names <- function(ax){
  if (is.character(ax[[1]])) {
    unlist(ax)
  } else {
    vapply(ax, \(.) .$name, character(1))
  }
}

# Internal helper to generate coordinate transformations
.default_ct <- \(axes, name="global", type="identity", data=NULL) {
    ct <- list(
      input=list(axes = axes,
                 name = if(length(axes) == 3) "cyx" else "yx"), 
      output=list(axes = axes,
                  name = name), 
      type = type)
    if (!is.null(data)) ct[[type]] <- data
    list(ct)
}

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
.DollarNames.Zattrs <- \(x, pattern="") names(x)

#' @rdname Zattrs
#' @exportMethod $
setMethod("$", "Zattrs", \(x, name) x[[name]])

# internal use only!
#' @noRd 
.ms <- \(x) switch(version(x), "0.3"=x$ome$multiscales, x$multiscales)

# internal use only!
#' @noRd 
.ch <- \(x) {
  if (version(x) == "0.3") x <- x$ome
  unlist(x$omero$channels)
}

# internal use only!
#' @noRd 
setMethod("multiscales", "list", .ms)

#' @export
setMethod("channels", "Zattrs", \(x, ...) .ch(x))
