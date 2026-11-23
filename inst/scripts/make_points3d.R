# FIXME: Submit this upstream.
# 
#  Adds a 3D points element 'blobs_points3d' to the Zarr v3 example
# 'inst/extdata/blobs_v3.zarr': a copy of 'blobs_points' with an added
# 'z' axis (z = x/2), used to test reading of 3D points.
# Run from the package root: source("inst/scripts/make_points3d.R")

sd <- file.path("inst", "extdata", "blobs_v3.zarr")
src <- file.path(sd, "points", "blobs_points")
dst <- file.path(sd, "points", "blobs_points3d")
unlink(dst, recursive=TRUE)
dir.create(file.path(dst, "points.parquet"), recursive=TRUE)

# element metadata: add 'z' axis (input/output axes included)
attrs <- Rarr::read_zarr_attributes(src)
attrs$axes <- c(attrs$axes, list("z"))
Rarr::write_zarr_group(sd, file.path("points", "blobs_points3d"))
z <- list(name="z", type="space", unit="unit")
for (i in seq_along(attrs$coordinateTransformations)) {
    attrs$coordinateTransformations[[i]][["input"]]$axes <- c(attrs$coordinateTransformations[[i]][["input"]]$axes, list(z))
    attrs$coordinateTransformations[[i]][["output"]]$axes <- 
    attrs$coordinateTransformations[[i]]$input$name <- "xyz"
}
Rarr::write_zarr_attributes(dst, attrs)

# data: add 'z' column
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, sprintf(
    "COPY (SELECT *, x/2 AS z FROM read_parquet('%s')) TO '%s' (FORMAT parquet)",
    file.path(src, "points.parquet", "part.0.parquet"),
    file.path(dst, "points.parquet", "part.0.parquet")))
DBI::dbDisconnect(con, shutdown=TRUE)

# register in consolidated metadata of the root group
Rarr::zarr_consolidate_metadata(sd)
