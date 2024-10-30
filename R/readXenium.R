#' @rdname readXenium
#' @title Read Xenium data
#'
#' @return ...
#'
#' @examples
#' # TODO
#'
#' @importFrom arrow read_parquet
#' @export
readXenium <- function(dir) {
    se <- DropletUtils::read10xCounts(file.path(dir, "cell_feature_matrix.h5"))
    tx <- read.csv(file.path(dir, "transcripts.csv.gz"))
    cb <- read_parquet(file.path(dir, "cell_boundaries.parquet"), as_data_frame=FALSE)
    nb <- read_parquet(file.path(dir, "nucleus_boundaries.parquet"), as_data_frame=FALSE)
    SpatialData(
        shapes=list(
            cell_boundaries=ShapeFrame(cb),
            nucleus_boundaries=ShapeFrame(nb)),
        points=list(transcripts=PointFrame(data=tx)),
        tables=list(transcripts.cell_boundaries=se))
}

# TODO: R-native readers for commercial platform?
# won't be great for many, i.e., gotta read into memory 
# if .parquet, .zarr etc. isn't supplied...