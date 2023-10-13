#' @name mock
#' @rdname mock
#' @title mock data
#' @description ...
#' 
#' @param n Scalar integer specifying the number of points/shapes to generate.
#' @param type Character string specifying which type of shapes to generate.
#' 
#' @examples
#' x <- SpatialData(
#'   images=list(noise=mockImage()),
#'   labels=list(squares=mockLabel()),
#'   points=list(dots=mockPoint()),
#'   shapes=list(
#'     blobs=mockShape(type="circle"), 
#'     wedges=mockShape(type="polygon")))
#' plotImage(x)
#' plotLabel(x)
#' plotShape(x, i="blobs")
#' plotShape(x, i="wedges")
#' plotPoint(x, col="z")
#' 
#' @return A \code{SpatialData} element.
#' @author Helena L. Crowell
NULL

#' @rdname mock
#' @importFrom stats runif
#' @export
mockImage <- \() {
    d <- c(3, 40, 60)
    rgb <- array(runif(prod(d)), dim=d)
    za <- Zattrs(list(multiscales=list(
        axes=list(data.frame(
            name=c("c", "y", "x"),
            type=c("channel", "space", "space"))),
        coordinateTransformations=list(newCoord("foo")))))
    ImageArray(rgb, zattrs=za)
}

#' @rdname mock
#' @export
mockLabel <- \(n=10) {
    x <- matrix(0, 40, 60)
    for (. in seq_len(n)) {
        i <- sample(nrow(x), 1)
        j <- sample(ncol(x), 1)
        n <- sample(5, 1)
        ij <- expand.grid(
            seq(i-n, i+n),
            seq(j-n, j+n))
        ij <- ij[!(
            ij[, 1] < 0 | ij[, 1] > nrow(x) |
            ij[, 2] < 0 | ij[, 2] > ncol(x) ), ]
        x[as.matrix(ij)] <- n
    }
    za <- Zattrs(list(multiscales=list(
        axes=list(data.frame(
            name=c("y", "x"),
            type=c("space", "space"))),
        coordinateTransformations=list(newCoord("foo")))))
    LabelArray(x, zattrs=za)
}

#' @rdname mock
#' @importFrom stats runif
#' @export
mockShape <- \(n=10, type=c("circle", "polygon")) {
    data <- switch(type <- match.arg(type), 
        circle={
            asplit(cbind(x=runif(n, 0, 60), y=runif(n, 0, 40)), 1)
        },
        polygon={
            replicate(n, {
                m <- sample(seq(3, 5), 1) # vertices
                cbind(x=runif(m, 0, 60), y=runif(m, 0, 40))
            }, simplify=FALSE)
        }) 
    df <- data.frame(index=seq_len(n), type)
    df$data <- data; df <- df[, c("data", "index", "type")]
    if (type == "circle") df$radius <- runif(n, 0, 5)
    za <- Zattrs(list(coordinateTransformations=newCoord("foo")))
    ShapeFrame(df, zattrs=za)
}

#' @rdname mock
#' @importFrom stats runif
#' @export
mockPoint <- \(n=100) {
    z <- sample(letters[seq_len(3)], n, TRUE)
    df <- data.frame(x=runif(n, 0, 60), y=runif(n, 0, 40), z)
    za <- Zattrs(list(coordinateTransformations=newCoord("foo")))
    PointFrame(df, zattrs=za)
}
