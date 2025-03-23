#' @name sdArray
#' @title Methods for `ImageArray` and `LabelArray` class
#' 
#' @param x \code{ImageArray} or  \code{LabelArray}
#' @param data list of \code{\link[Rarr]{ZarrArray}}s
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param i,j indices specifying elements to extract.
#' @param k scalar index specifying which scale to extract.
#' @param drop ignored.
#' @param ... option arguments passed to and from other methods.
#'
#' @return \code{ImageArray}
#'
#' @examples
#' # asdasd
#'
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
NULL

#' @rdname sdArray
#' @export
setMethod("data", "sdArray", \(x, k=1) {
  if (is.null(k)) return(x@data)
  stopifnot(length(k) == 1, is.numeric(k), k > 0)
  n <- length(x@data) # get number of available scales
  if (is.infinite(k)) k <- n # input of Inf uses lowest
  if (k <= n) return(x@data[[k]]) # return specified scale
  stop("'k=", k, "' but only ", n, " resolution(s) available")
})

#' @rdname sdArray
#' @export
setMethod("dim", "sdArray", \(x) dim(data(x)))

#' @rdname sdArray
#' @export
setMethod("length", "sdArray", \(x) length(data(x, NULL)))

#' @rdname sdArray
#' @importFrom utils head tail
#' @exportMethod [
setMethod("[", "sdArray", \(x, i, j, k, ..., drop=FALSE) {
  if (missing(i)) i <- TRUE
  if (missing(j)) j <- TRUE else if (isFALSE(j)) j <- 0 else .check_jk(j, "j")
  if (missing(k)) k <- TRUE else if (isFALSE(k)) k <- 0 else .check_jk(k, "k")
  ijk <- list(i, j, k)
  n <- length(data(x, NULL))
  x@data <- lapply(seq_len(n), \(.) {
    d <- dim(data(x, .))
    j <- if (isTRUE(j)) seq_len(d[2]) else j
    k <- if (isTRUE(k)) seq_len(d[3]) else k
    jk <- lapply(list(j, k), \(jk) {
      fac <- 2^(.-1)
      seq(floor(head(jk, 1)/fac), 
          ceiling(tail(jk, 1)/fac))
    })
    data(x, .)[i, jk[[1]], jk[[2]], drop=FALSE]
  })
  x
})