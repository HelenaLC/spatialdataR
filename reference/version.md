# Retrieve versions of `SpatialData` elements

Retrieve versions of `SpatialData` elements

## Usage

``` r
# S4 method for class 'SpatialDataElement'
version(x)

# S4 method for class 'SpatialDataAttrs'
version(x)

# S4 method for class 'SingleCellExperiment'
version(x)

# S4 method for class 'SpatialData'
version(x, simplify = TRUE, ...)
```

## Arguments

- x:

  [`SpatialData`](https://helenalc.github.io/spatialdataR/reference/SpatialData.md)
  object or one of its elements.

- simplify:

  logical scalar; whether to flatten paths into a tibble.

- ...:

  ignored.

## Value

for single elements, a character string; for
[SpatialData](https://helenalc.github.io/spatialdataR/reference/SpatialData.md)
objects, if `simplify=TRUE` (default), a `tibble` where rows=elements
and columns=layers/elements/versions. if `simplify=FALSE`, a depth-3
list where levels=layers/elements/versions.

## Examples

``` r
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

# element-wise
version(shape(sd))
#> [1] "0.2"

# object-wide
version(sd)
#> # A tibble: 9 × 3
#>   layer  element                 version
#>   <chr>  <chr>                   <chr>  
#> 1 images blobs_image             0.1    
#> 2 images blobs_multiscale_image  0.1    
#> 3 labels blobs_labels            0.1    
#> 4 labels blobs_multiscale_labels 0.1    
#> 5 points blobs_points            0.1    
#> 6 shapes blobs_circles           0.2    
#> 7 shapes blobs_multipolygons     0.2    
#> 8 shapes blobs_polygons          0.2    
#> 9 tables table                   0.1    
version(sd, FALSE)$labels
#> $blobs_labels
#> [1] "0.1"
#> 
#> $blobs_multiscale_labels
#> [1] "0.1"
#> 
```
