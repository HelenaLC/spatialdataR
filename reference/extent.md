# Spatial element extent

Spatial element extent

## Usage

``` r
# S4 method for class 'SpatialData'
extent(x, i = 1)

# S4 method for class 'SpatialDataArray'
extent(x, i = 1)

# S4 method for class 'SpatialDataFrame'
extent(x, i = 1)
```

## Arguments

- x:

  a `SpatialData` element (any but table).

- i:

  scalar integer or string; target coordinate space.

## Value

Length-2 list with numeric x and y ranges.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

# object-wide
extent(x)
#> $x
#> [1]  0 64
#> 
#> $y
#> [1]  0 64
#> 

# element-wise
extent(image(x))
#> $x
#> [1]  0 64
#> 
#> $y
#> [1]  0 64
#> 
extent(point(x))
#> $x
#> [1]  1 62
#> 
#> $y
#> [1]  1 62
#> 
extent(shape(x))
#> $x
#> [1] 18.74077 46.17780
#> 
#> $y
#> [1] 23.57794 46.41484
#> 

# with transformation(s)
extent(label(x), "scale")
#> $x
#> [1]   0 128
#> 
#> $y
#> [1]   0 192
#> 
extent(label(x), "translation")
#> $x
#> [1] 10 74
#> 
#> $y
#> [1] -50  14
#> 
```
