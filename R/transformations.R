#' @rdname ImageArray
#' @importFrom S4Vectors DataFrame
#' @export
setMethod("transformations", "ImageArray", function(x) {
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

.get_idx <- function(df, which) {
  stopifnot(length(which) == 1)
  if (is.character(which)) {
    idx <- match(which, df$output.name)
    if (is.na(idx)) 
      stop(sprintf("couldn't find transformation '%s'", which))
  } else {
    stopifnot(which %in% seq(nrow(df)))
    idx <- which
  }
  return(idx)
}

#' @rdname ImageArray
#' @export
setMethod("transformation", "ImageArray", function(x, which=1, ...) {
  df <- transformations(x)
  idx <- .get_idx(df, which)
  df$data[[idx]]
})

#' @importFrom EBImage abind resize Image
.trans <- \(x, which=1) {
  # x <- image(spd)
  # which <- "global"
  i <- as.array(x)
  df <- transformations(x)
  idx <- .get_idx(df, which)
  t <- transformation(x, which)
  type <- df$type[idx]
  switch(type,
    "scale" = {
      j <- apply(i, 1, \(j) {
        resize(j, nrow(j)*t[2], ncol(j)*t[3])
      }, simplify = FALSE)
      j <- abind(j, along=0)
    },
    "identity" = { 
      j <- i
    },
    sprintf("transformation of type '%s' yet to be supported.", type)
  )
  ImageArray(data=j, metadata=metadata(x))
}

#' @rdname ImageArray
#' @param x,y \code{ImageArray}s to be aligned.
#' @param which A character string specifying
#'   the joint reference coordinate system.
#' @export
alignImages <- function(x, y, which, ...) {
  x <- .trans(x, which)
  y <- .trans(y, which)
  x <- as.array(aperm(x))
  y <- as.array(aperm(y))
  x <- Image(x/max(x), dim(x), "Color")
  y <- Image(y/max(y), dim(y), "Grayscale")
  # TODO: transformations are applied correctly
  # but cannot plot this right now...
}