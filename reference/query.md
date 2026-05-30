# queries

`query` provides a interface for table-based subsetting of `SpatialData`
objects. It filters a specified table using
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
logic and propagates the result to all associated spatial elements
(i.e., only instances present in the filtered table are kept).

For spatial cropping, see
[`crop`](https://helenalc.github.io/SpatialData/reference/crop.md).

## Usage

``` r
# S4 method for class 'SpatialData'
query(x, ..., i = 1)
```

## Arguments

- x:

  `SpatialData` object.

- ...:

  logic passed to
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).

- i:

  index or name of table to query.

## Value

`SpatialData` object

## Examples

``` r
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

# filter by 'region' and propagate to shapes/points
t <- table(sd)
query(sd, i=1, region == region(t))
#> Warning: arguments in '...' ignored
#> Warning: arguments in '...' ignored
#> class: SpatialData
#> - images(0):
#> - labels(1):
#>   - blobs_labels (64,64)
#> - points(0):
#> - shapes(0):
#> - tables(1):
#>   - table (3,10) [blobs_labels]
#> coordinate systems(5):
#> - global(1): blobs_labels
#> - scale(1): blobs_labels
#> - translation(1): blobs_labels
#> - affine(1): blobs_labels
#> - sequence(1): blobs_labels
```
