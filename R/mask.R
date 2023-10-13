#' @name mask
#' @rdname mask
#' 
#' @title Mask an element by another
#' 
#' @description ...
#'
#' @param x,y A \code{\link{SpatialData}} element
#'   (see Usage for currently supported methods).
#' @param fun Function to use for aggregation.
#' @param name Character string specifying
#'   the name of the output SCE's `assay`.
#'
#' @return
#' A \code{\link[SingleCellExperiment]{SingleCellExperiment}}
#' where rows/columns correspond to data from \code{x/y}.
#'
#' @examples
#' dir <- file.path("extdata", "blobs")
#' dir <- system.file(dir, package="SpatialData")
#' spd <- readSpatialData(dir)
#' (sce <- mask(image(spd), label(spd)))
#'
#' @author Helena L. Crowell
#' @importFrom SingleCellExperiment SingleCellExperiment
NULL

#' @rdname mask
#' @export
setMethod("mask", 
    c("ImageArray", "LabelArray"), 
    \(x, y, fun=mean, name="X") {
        stopifnot(
            length(name) == 1,
            is.character(name))
        # retrieve data
        a <- as.array(x)
        b <- as.array(y)
        # aggregate
        i <- axiis(x, "channel")
        i <- match(i, axiis(x))
        z <- array(NA, dim(a))
        c <- t(apply(a, i, tapply, b, fun))
        for (. in colnames(c)) 
            z[b == .] <- c[, .]
        z <- as.data.frame(z)
        z <- as.matrix(z)
        # construct SCE
        cd <- expand.grid(
            seq_len(nrow(b)), 
            seq_len(ncol(b)))
        names(cd) <- axiis(x)[-i]
        cd$instance_id <- c(b)
        rownames(z) <- channels(x)
        as <- list(z); names(as) <- name
        SingleCellExperiment(as, colData=cd)[, cd$instance_id != 0]
    })

#' @rdname mask
#' @export
setMethod("mask", 
    c("ImageArray", "ShapeFrame"), 
    \(x, y, fun=mean, name="X") {
        stopifnot(
            length(name) == 1, 
            is.character(name))
        # retrieve data
        a <- as.array(x)
        b <- as.data.frame(y)
        # aggregate
        # TODO: polygon
        z <- apply(b, 1, \(.) {
            . <- as.list(.)
            r <- .$radius*c(-1, 1)
            xy <- lapply(rev(.$data), `+`, r)
            ij <- lapply(xy, \(.) seq(.[1], .[2]))
            vapply(seq_len(nrow(a)), \(.) {
                ijk <- do.call(expand.grid, c(., ij))
                fun(a[as.matrix(ijk)])
            }, numeric(1)) 
        })
        # construct SCE
        dimnames(z) <- list(channels(x), b$index)
        as <- list(z); names(as) <- name
        SingleCellExperiment(as, colData=b)
    })

#' @rdname mask
#' @export
setMethod("mask", 
    c("LabelArray", "ShapeFrame"), 
    \(x, y, fun=mean, name="X") {
        stopifnot(
            length(name) == 1, 
            is.character(name))
        # retrieve data
        a <- as.array(x)
        b <- as.data.frame(y)
        ids <- sort(unique(c(a)))
        # aggregate
        # TODO: polygon
        df <- .df_circle(y)
        df[, 2] <- ceiling(df[, 2])
        df[, 3] <- floor(df[, 3])
        df <- unique(df)
        z <- lapply(split(df, df$index), \(fd) {
            fd <- split(df, df$index)[[1]]
            ij <- as.matrix(fd[, c("y", "x")])
            # TODO: border only, not good :/
            base::table(factor(a[ij], ids))
        }) |> do.call(what=cbind)
        # construct SCE
        dimnames(z) <- list(ids, b$index)
        as <- list(z); names(as) <- name
        xy <- do.call(rbind, b$data)
        colnames(xy) <- c("x", "y")
        cd <- cbind(xy, b); cd$data <- NULL
        SingleCellExperiment(as, colData=cd)
    })
  
#' #' @rdname mask
#' #' @export
#' setMethod("getTable", 
#'     c("PointFrame", "LabelFrame"), 
#'     \(x, y, fun=mean, name="X") {
#'         
#'     })
#' 
#' #' @rdname mask
#' #' @export
#' setMethod("getTable", 
#'     c("PointFrame", "ShapeFrame"), 
#'     \(x, y, fun=mean, name="X") {
#'         x <- point(spd); y <- shape(spd); fun <- mean; name <- "X"
#'         stopifnot(
#'             length(name) == 1, 
#'             is.character(name))
#'         # retrieve data
#'         a <- as.data.frame(x)
#'         b <- as.data.frame(y)
#'     })
