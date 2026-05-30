# Miscellaneous \`SpatialData\` methods

Miscellaneous methods (e.g., `show`) for the
[`SpatialData`](https://helenalc.github.io/SpatialData/reference/SpatialData.md)
class and its elements.

## Usage

``` r
# S4 method for class 'SpatialData'
show(object)

# S4 method for class 'SpatialDataArray'
show(object)

# S4 method for class 'SpatialDataPoint'
show(object)

# S4 method for class 'SpatialDataShape'
show(object)
```

## Arguments

- object:

  [`SpatialData`](https://helenalc.github.io/SpatialData/reference/SpatialData.md)
  object or one of its elements, i.e., a
  `SpatialDataImage/Label/Point/Shape`.

## Value

`NULL`

## Examples

``` r
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
(sd <- readSpatialData(zs))
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

# show element
image(sd)
#> class:  SpatialDataImage  
#> Scales (1): (3,64,64)
label(sd)
#> class:  SpatialDataLabel  
#> Scales (1): (64,64)
point(sd)
#> class: SpatialDataPoint
#> count: 200 
#> data(4): genes instance_id __null_dask_index__ geometry
shape(sd)
#> class: SpatialDataShape
#> count: 5 
#> data(2): geometry radius

# show .zattrs
meta(label(sd))
#> class: SpatialDataAttrs
#> axes(2):
#> - name: y x 
#> - type: space space 
#> coordTrans(5):
#> - global: (identity)
#> - scale: (scale:[3,2])
#> - translation: (translation:[-50,10])
#> - affine: (affine:[[20,10,30],[50,40,60]])
#> - sequence: (scale:[3,2]), (translation:[-50,10])
#> datasets(1): 0
#> - 0: (scale:[1,1])
meta(image(sd, 2))
#> class: SpatialDataAttrs
#> axes(3):
#> - name: c y x 
#> - type: channel space space 
#> coordTrans(1):
#> - global: (identity)
#> datasets(3): 0 1 2
#> - 0: (scale:[1,1,1])
#> - 1: (scale:[1,2,2])
#> - 2: (scale:[1,4,4])
#> channels(3): 0 1 2
```
