#' @rdname ImageArray
#' @importFrom S4Vectors DataFrame
#' @export
setMethod("coords", "ImageArray", function(x) {
  df <- metadata(x)$multiscales$coordinateTransformations[[1]]
  data <- lapply(seq(nrow(df)), \(.)
    ifelse(
      df$type[.] == "identity",
      list(NA), I(df[., df$type[.]])))
  DataFrame(
    input.name=df$input$name,
    output.name=df$output$name,
    input.axes=I(df$input$axes),
    output.axes=I(df$output$axes),
    type=df$type, data=I(unlist(data, recursive=FALSE)))
})

setMethod("coord", "ImageArray", function(x, name) {
  df <- coords(x)
  if (missing(name))
    name <- df$output.name[1]
  stopifnot(
    length(name) == 1,
    is.character(name))
  idx <- match(name, df$output.name)
  if (is.na(idx))
    stop(sprintf("couldn't find coords '%s'", name))
  return(df[idx, ])
})

#' #' @rdname ImageArray
#' #' @export
#' setMethod("transformation", "ImageArray", function(x, which=1, ...) {
#'   df <- transformations(x)
#'   idx <- .get_idx(df, which)
#'   df$data[[idx]]
#' })

#' @rdname ImageArray
#' @importFrom EBImage abind resize
#' @export
setMethod("scaleImage", "ImageArray", function(x, t=rep(1, length(dim(x)))) {
  stopifnot(
    is.numeric(t),
    length(t) == length(dim(x)))
  a <- as.array(x)
  y <- apply(a, 1, \(.) resize(., nrow(.)*t[2], ncol(.)*t[3]), simplify=FALSE)
  y <- abind(y, along=0)
  ImageArray(y, metadata(x))
})

#' @rdname ImageArray
#' @importFrom EBImage abind rotate
#' @export
setMethod("rotateImage", "ImageArray", function(x, t=0) {
  stopifnot(
    is.numeric(t),
    length(t) == 1)
  a <- as.array(x)
  y <- apply(a, 1, rotate, t, simplify = FALSE)
  y <- abind(y, along=0)
  ImageArray(y, metadata(x))
})

#' @rdname ImageArray
#' @importFrom EBImage abind translate
#' @export
setMethod("translateImage", "ImageArray", function(x, t=c(0,0)) {
  stopifnot(
    is.numeric(t),
    length(t) == 2,
    round(t) == t)
  a <- as.array(x)
  y <- apply(a, 1, translate, t, simplify = FALSE)
  y <- abind(y, along=0)
  ImageArray(y, metadata(x))
})

#' @importFrom EBImage Image
setMethod("transformImage", "ImageArray", function(x, coords) {
  df <- coord(x, coords)
  t <- df$data[[1]]
  switch(df$type,
    "identity" = x,
    "scale" = scaleImage(x, t),
    "rotate" = rotateImage(x, t),
    sprintf("transformation of type '%s' yet to be supported.", df$type))
})

#' #' @rdname ImageArray
#' #' @param x,y \code{ImageArray}s to be aligned.
#' #' @param which A character string specifying
#' #'   the joint reference coordinate system.
#' #' @export
#' alignImages <- function(x, y, which, ...) {
#'   x <- .trans(x, which)
#'   y <- .trans(y, which)
#'   x <- as.array(aperm(x))
#'   y <- as.array(aperm(y))
#'   x <- Image(x/max(x), dim(x), "Color")
#'   y <- Image(y/max(y), dim(y), "Grayscale")
#'   # TODO: transformations are applied correctly
#'   # but cannot plot this right now...
#' }
