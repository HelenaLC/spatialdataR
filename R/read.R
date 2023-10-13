#' @rdname readArray
#' @title Read `images/labels` element
#' @description ...
#'
#' @param path A character string specifying
#'   a .zarray or .zattrs file-containing directory.
#' @param resolution A charactering specifiying
#'   the image resolution (pyramid level) to read in.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{\link{ImageArray}}
#'
#' @examples
#' path <- system.file("extdata", "blobs", package="SpatialData")
#' (ia <- readArray(file.path(path, "images", "blobs_image")))
#' (la <- readArray(file.path(path, "labels", "blobs_labels")))
#'
#' @author Helena L. Crowell
#'
#' @importFrom jsonlite fromJSON
#' @importFrom Rarr read_zarr_array zarr_overview
#' @export
readArray <- function(path=".", resolution="0", ...) {
    if (file.exists(file.path(path, ".zarray"))) {
        json <- file.path(dirname(path), ".zattrs")
        if (!file.exists(json))
            stop("couldn't find .zattrs upstream of .zarray")
        zarr <- path
    } else {
        json <- file.path(path, ".zattrs")
        zarr <- file.path(path, resolution)
        if (!file.exists(zarr))
            stop("couldn't find .zarray under resolution /", resolution)
    }
    md <- fromJSON(json)
    za <- zarr_overview(zarr, as_data_frame=TRUE)
    # using overview for development purposes
    # since we are missing delayed support atm

    is_img <- !is.null(md$channels_metadata)
    fun <- if (is_img) ImageArray else LabelArray
    fun(data=za, metadata=list(), zattrs=Zattrs(md))
}

#' @rdname readShapes
#' @title Read `shapes` element
#' @description ...
#'
#' @param path A character string specifying
#'   the path to a \code{shapes/} subdirectory.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{\link[S4Vectors]{DataFrame}}
#'
#' @examples
#' dir <- file.path("extdata", "blobs", "shapes", "blobs_polygons")
#' dir <- system.file(dir, package="SpatialData")
#' (sf <- readShapes(dir))
#' as.data.frame(sf)
#'
#' @author Tim Treis, Helena L. Crowell
#'
#' @importFrom jsonlite fromJSON
#' @importFrom S4Vectors DataFrame
#' @importFrom Rarr read_zarr_array
#' @export
readShapes <- function(path, ...) {
    # TODO: metadata are currently being ignored here...
    # might need another data structure to accommodate these.
    parts <- list.dirs(path, recursive=FALSE)
    names(ps) <- ps <- c("coords", "Index", "radius", "offset0", "offset1")
    ps <- lapply(ps, \(p) {
        if (p %in% basename(parts))
            read_zarr_array(file.path(path, p))
    })
    geom <- ifelse(!is.null(ps$radius), "circle", "polygon")
    md <- fromJSON(file.path(path, ".zattrs"))
    switch(geom,
        circle={
            df <- DataFrame(
                data=I(asplit(ps$coords, 1)),
                index=ps$Index,
                radius=ps$radius,
                type=rep(geom, length(ps$Index)))
        },
        polygon={
            coords <- lapply(seq_along(ps$Index), \(.) {
                idx <- seq(ps$offset0[[.]] + 1, ps$offset0[[. + 1]])
                ps$coords[idx, , drop=FALSE]
            })
            df <- DataFrame(
                data=I(coords),
                index=ps$Index,
                type=rep(geom, length(ps$Index)))
        })
    ShapeFrame(df, zattrs=Zattrs(md))
}

#' @rdname readPoints
#' @title Read `points` element
#' @description ...
#'
#' @param path A character string specifying
#'   a .parquet file-containing directory.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return Arrow \code{\link{Dataset}}
#'
#' @examples
#' dir <- "extdata/blobs/points/blobs_points"
#' dir <- system.file(dir, package = "SpatialData")
#' (pf <- readPoints(dir))
#'
#' @author Tim Treis
#'
#' @importFrom arrow read_parquet
#' @export
readPoints <- function(path, ...) {
    path <- gsub("/$", "", path)
    za <- fromJSON(file.path(path, ".zattrs"))
    dirs <- list.files(path, full.names=TRUE, recursive=TRUE)
    pq <- grep("*\\.parquet$", dirs, value=TRUE)
    at <- read_parquet(pq, as_data_frame=FALSE)
    PointFrame(data=at, zattrs=Zattrs(za))
}

#' @rdname readTable
#' @title `SingleCellExperiment` from `AnnData`-Zarr
#' @description ...
#'
#' @param path A character string specifying
#'   the path to a `table/` subdirectory.
#'
#' @return \code{SingleCellExperiment}
#'
#' @examples
#' dir <- file.path("extdata", "blobs", "table", "table")
#' dir <- system.file(dir, package="SpatialData")
#' (sce <- readTable(dir))
#'
#' @author Constantin Ahlmann-Eltze
#'
#' @importFrom reticulate import
#' @importFrom jsonlite fromJSON
#' @importFrom zellkonverter AnnData2SCE
#' @importFrom SingleCellExperiment int_metadata<-
#' @importFrom basilisk basiliskStart basiliskStop basiliskRun
#' @export
readTable <- function(path) {
    proc <- basiliskStart(.env)
    on.exit(basiliskStop(proc))
    sce <- basiliskRun(proc, function(zarr) {
        # return value MUST be a pure R object,
        # i.e., no reticulate Python objects
        # or pointers to shared memory
        ad <- import("anndata")
        obj <- ad$read_zarr(zarr)
        sce <- AnnData2SCE(obj)
    }, zarr=path)
    za <- fromJSON(file.path(path, ".zattrs"))
    int_metadata(sce)$zattrs <- za
    metadata(sce) <- list()
    return(sce)
}

#' @rdname readSpatialData
#' @title Read `SpatialData` OME-Zarr
#' @description ...
#'
#' @param path A character string specifying the path to an
#'   OME-Zarr file adhering to \code{SpatialData} specification.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{\link{SpatialData}}
#'
#' @examples
#' dir <- file.path("extdata", "blobs")
#' dir <- system.file(dir, package="SpatialData")
#' (spd <- readSpatialData(dir))
#'
#' @author Constantin Ahlmann-Eltze, Helena L. Crowell
#'
#' @export
readSpatialData <- function(path, ...) {
    layers <- list.dirs(path, recursive=FALSE)
    
    # used internally only atm
    dots <- list(...)
    i <- if (is.null(n <- dots$n)) TRUE else
        if (length(n) == 1) seq_len(n) else n
    
    for (l in setdiff(LAYERS, "tables")) {
        val <- if (l %in% basename(layers)) {
            dir <- file.path(path, l)
            sub <- list.dirs(dir, recursive=FALSE)
            names(sub) <- basename(sub)
            fun <- switch(l,
                shapes=readShapes,
                points=readPoints,
                readArray)
            lapply(sub[i], fun)
        } else list()
        assign(l, val)
    }
    
    # TODO: update once the tables PR goes through
    tables <- if ("table" %in% basename(layers)) {
        tbl <- tryCatch(
            error=function(e) NULL,
            readTable(file.path(path, "table/table")))
        if (is(tbl, "SingleCellExperiment")) {
            key <- tbl$region[1]
            tbl <- list(tbl)
            names(tbl) <- key
            tbl
        }
    }
    
    SpatialData(images=images, labels=labels, shapes=shapes, points=points, tables=tables)
}
