# Coord. trans. utilities

Coord. trans. utilities

## Usage

``` r
# S4 method for class 'SpatialDataAttrs'
axes(x, y = NULL, ...)

# S4 method for class 'SpatialDataAttrs'
CTlist(x, ...)

# S4 method for class 'SpatialDataAttrs'
CTdata(x, i = 1, ...)

# S4 method for class 'SpatialDataAttrs'
CTtype(x, ...)

# S4 method for class 'SpatialDataAttrs'
CTname(x, ...)

# S4 method for class 'SpatialDataElement'
axes(x, y = NULL, ...)

# S4 method for class 'SpatialDataElement'
CTlist(x, ...)

# S4 method for class 'SpatialDataElement'
CTtype(x, ...)

# S4 method for class 'SpatialDataElement'
CTname(x, ...)

# S4 method for class 'SpatialDataElement'
CTdata(x, i = 1, ...)

# S4 method for class 'SpatialData'
CTname(x, ...)

# S4 method for class 'SpatialDataElement'
rmvCT(x, i)

# S4 method for class 'SpatialDataAttrs'
rmvCT(x, i)

# S4 method for class 'SpatialDataElement'
addCT(x, name, type = "identity", data = NULL)

# S4 method for class 'SpatialDataAttrs'
addCT(x, name, type = "identity", data = NULL)
```

## Arguments

- x:

  `SpatialData`, an element, or `SpatialDataAttrs`.

- y:

  NULL (default) returns a list where each element is an axis: a list
  with name/type/unit (e.g., x/space/micrometer); `y="name/type/unit"`
  extracts specific data over all axiis.

- ...:

  option arguments passed to and from other methods.

- i:

  for `CTpath`, source node label; else, string or scalar integer giving
  the name or index of a coordinate space.

- name:

  character(1); name of coordinate space

- type:

  character(1); type of transformation

- data:

  transformation data; size and shape depend on transformation and
  element type (e.g., numeric(1) for rotation, numeric(2) for scaling in
  2D)

## Value

- `CTname`: character string; transformation name (e.g., "global")

- `CTtype`: character string; transformation type (e.g., "affine")

- `CTdata`: list; transformation data (e.g., scalar numeric for
  rotation)

- `CTlist`: list; list of transformation specifications per OME-NGFF
  spec

- `add/rmvCT`: `SpatialDataElement` or `SpatialDataAttrs` with
  transformation(s) added/removed

- `axes`: list; each element is a character string (name), or list with
  axis name and type (e.g., "space" or "channel")

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

# view available target coordinate systems
CTname(z <- meta(label(x)))
#> [1] "global"      "scale"       "translation" "affine"      "sequence"   

# add
addCT(z, "scale", "scale", c(12, 34)) # overwrite
#> class: SpatialDataAttrs
#> axes(2):
#> - name: y x 
#> - type: space space 
#> coordTrans(5):
#> - global: (identity)
#> - scale: (scale:[12,34])
#> - translation: (translation:[-50,10])
#> - affine: (affine:[[20,10,30],[50,40,60]])
#> - sequence: (scale:[3,2]), (translation:[-50,10])
#> datasets(1): 0
#> - 0: (scale:[1,1])
CTname(addCT(z, "new", "translation", c(12, 34)))
#> [1] "global"      "scale"       "translation" "affine"      "sequence"   
#> [6] "new"        

# rmv
CTname(rmvCT(z, 2))        # by index
#> [1] "global"      "translation" "affine"      "sequence"   
CTname(rmvCT(z, "scale"))  # by name
#> [1] "global"      "translation" "affine"      "sequence"   
CTname(rmvCT(z, "global")) # identity is protected
#> Warning: can't drop identity
#> [1] "global"      "scale"       "translation" "affine"      "sequence"   
```
