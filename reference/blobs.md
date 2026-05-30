# \`SpatialData\` .zarr toy datasets

data were retrieved on Nov. 11th, 2024, from
[here](https://github.com/scverse/spatialdata-notebooks/tree/main/notebooks/developers_resources/storage_format/multiple_elements.zarr).

## Value

zarr store.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
(x <- readSpatialData(x))
#> class: SpatialData
#> - images(2):
#>   - blobs_image (3,64,64)
#>   - blobs_multiscale_image (3,64,64)
#> - labels(2):
#>   - blobs_labels (64,64)
#>   - blobs_multiscale_labels (64,64)
#> - points(1):
#>   - blobs_points (200)
#> - shapes(3):
#>   - blobs_circles (5,circle)
#>   - blobs_multipolygons (2,polygon)
#>   - blobs_polygons (5,polygon)
#> - tables(1):
#>   - table (3,10) [blobs_labels]
#> coordinate systems(5):
#> - global(8): blobs_image blobs_multiscale_image ... blobs_polygons
#>   blobs_points
#> - scale(1): blobs_labels
#> - translation(1): blobs_labels
#> - affine(1): blobs_labels
#> - sequence(1): blobs_labels
```
