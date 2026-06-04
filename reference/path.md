# Retrieve `SpatialData` on-disk paths

Retrieve `SpatialData` on-disk paths

## Usage

``` r
# S4 method for class 'SpatialDataArray'
path(object, ...)

# S4 method for class 'SpatialDataFrame'
path(object, ...)

# S4 method for class 'SingleCellExperiment'
path(object, ...)

# S4 method for class 'SpatialData'
path(object, simplify = TRUE, ...)
```

## Arguments

- object:

  [`SpatialData`](https://helenalc.github.io/SpatialData/reference/SpatialData.md)
  object or one of its elements.

- ...:

  ignored.

- simplify:

  logical scalar; whether to flatten paths into a tibble.

## Value

for single elements, a character string; for
[SpatialData](https://helenalc.github.io/SpatialData/reference/SpatialData.md)
objects, if `simplify=TRUE` (default), a `tibble` where rows=elements
and columns=layers/elements/paths. if `simplify=FALSE`, a depth-3 list
where levels=layers/elements/paths.

## Examples

``` r
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

# element-wise
path(shape(sd))
#> [1] "/home/runner/work/_temp/Library/spatialdataR/extdata/blobs.zarr/shapes/blobs_circles/shapes.parquet"

# object-wide
path(sd)
#> # A tibble: 9 × 3
#>   layer  element                 path                                           
#>   <chr>  <chr>                   <chr>                                          
#> 1 images blobs_image             /home/runner/work/_temp/Library/spatialdataR/e…
#> 2 images blobs_multiscale_image  /home/runner/work/_temp/Library/spatialdataR/e…
#> 3 labels blobs_labels            /home/runner/work/_temp/Library/spatialdataR/e…
#> 4 labels blobs_multiscale_labels /home/runner/work/_temp/Library/spatialdataR/e…
#> 5 points blobs_points            /home/runner/work/_temp/Library/spatialdataR/e…
#> 6 shapes blobs_circles           /home/runner/work/_temp/Library/spatialdataR/e…
#> 7 shapes blobs_multipolygons     /home/runner/work/_temp/Library/spatialdataR/e…
#> 8 shapes blobs_polygons          /home/runner/work/_temp/Library/spatialdataR/e…
#> 9 tables table                   /home/runner/work/_temp/Library/spatialdataR/e…
path(sd, FALSE)$labels
#> $blobs_labels
#> [1] "/home/runner/work/_temp/Library/spatialdataR/extdata/blobs.zarr/labels/blobs_labels"
#> 
#> $blobs_multiscale_labels
#> [1] "/home/runner/work/_temp/Library/spatialdataR/extdata/blobs.zarr/labels/blobs_multiscale_labels"
#> 
```
