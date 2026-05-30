# Combine two `SpatialData` objects

Combine two `SpatialData` objects

## Usage

``` r
# S4 method for class 'SpatialData,SpatialData'
combine(x, y, ...)
```

## Arguments

- x, y:

  `SpatialData` objects to combine.

- ...:

  ignored.

## Value

A `SpatialData` objects containing all elements from `x` and `y` with
names made unique.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

y <- combine(x, x)
imageNames(y)
#> [1] "blobs_image"              "blobs_multiscale_image"  
#> [3] "blobs_image.1"            "blobs_multiscale_image.1"
region(table(y, 1))
#> [1] "blobs_labels"
region(table(y, 2))
#> [1] "blobs_labels.1"
```
