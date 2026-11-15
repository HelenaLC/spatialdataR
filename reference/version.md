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
#> Error: unable to find an inherited method for function ‘version’ for signature ‘object = "SpatialDataShape"’

# object-wide
version(sd)
#> Error: unable to find an inherited method for function ‘version’ for signature ‘object = "SpatialData"’
version(sd, FALSE)$labels
#> Error: unable to find an inherited method for function ‘version’ for signature ‘object = "SpatialData"’
```
