# The \`SpatialData\` class

`SpatialData` provides an R interface to Python's `spatialdata`, which
enables the representation of diverse spatial omics datasets using the
OME-NGFF (Next Generation File Format) standard. In R,

- images and labels are `ZarrArray`s (`Rarr` package).

- points and shapes are managed using `duckspatial` tables.

- tables are `SingleCellExperiment`s (read with `anndataR`).

## Usage

``` r
SpatialData(
  images = list(),
  labels = list(),
  points = list(),
  shapes = list(),
  tables = list()
)

# S4 method for class 'SpatialData'
x$name

# S4 method for class 'SpatialData'
x$name <- value

# S4 method for class 'SpatialData,numeric,ANY'
x[[i, j, ...]]

# S4 method for class 'SpatialData,character,ANY'
x[[i, j, ...]]

# S4 method for class 'SpatialDataElement'
data(x, k = 1, ...)

# S4 method for class 'SpatialDataElement'
meta(x)

# S4 method for class 'SpatialData,ANY,ANY,ANY'
x[i, j, ..., drop = FALSE]

# S4 method for class 'SpatialData'
rownames(x)

# S4 method for class 'SpatialData'
colnames(x)

# S4 method for class 'SpatialData,character'
layer(x, i)

# S4 method for class 'SpatialData,ANY'
layer(x, i)

# S4 method for class 'SpatialData,character'
element(x, i)

# S4 method for class 'SpatialData,numeric'
element(x, i)

# S4 method for class 'SpatialData,missing'
element(x, i)

# S4 method for class 'SpatialData,ANY'
element(x, i)

# S4 method for class 'SpatialData,character'
element(x, i) <- value

# S4 method for class 'SpatialData'
images(x)

# S4 method for class 'SpatialData'
labels(object)

# S4 method for class 'SpatialData'
points(x)

# S4 method for class 'SpatialData'
shapes(x)

# S4 method for class 'SpatialData'
tables(x)

# S4 method for class 'SpatialData,character,ANY'
x[[i]] <- value

# S4 method for class 'SpatialData,numeric,ANY'
x[[i]] <- value

# S4 method for class 'SpatialData,ANY,ANY'
x[[i]] <- value
```

## Arguments

- images:

  list of
  [`SpatialDataImage`](https://helenalc.github.io/SpatialData/reference/SpatialDataArray.md)s

- labels:

  list of
  [`SpatialDataLabel`](https://helenalc.github.io/SpatialData/reference/SpatialDataArray.md)s

- points:

  list of
  [`SpatialDataPoint`](https://helenalc.github.io/SpatialData/reference/SpatialDataFrame.md)s

- shapes:

  list of
  [`SpatialDataShape`](https://helenalc.github.io/SpatialData/reference/SpatialDataFrame.md)s

- tables:

  list of `SingleCellExperiment`s

- x, object:

  `SpatialData` object.

- name:

  character string for extraction (see
  [`` ?base::`$` ``](https://rdrr.io/r/base/Extract.html)).

- value:

  (list of) element(s) with layer-compliant object(s), or
  NULL/[`list()`](https://rdrr.io/r/base/list.html) to remove an
  element/layer completely; for `element<-`, a single
  `SpatialDataElement` of the same class as `element(x, i)`.

- i, j:

  character string, scalar or vector of indices specifying the element
  to extract from a given layer.

- ...:

  optional arguments passed to and from other methods.

- k:

  scalar index specifying which scale to use; `Inf` to use lowest
  available resolution.

- drop:

  ignored.

## Value

`SpatialData`

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

# subsetting
# layers are taken in order of appearance
# (images, labels, points, shapes, tables)
x[-4] # drop layer
#> class: SpatialData
#> - images(2):
#>   - blobs_image (3,64,64)
#>   - blobs_multiscale_image (3,64,64)
#> - labels(2):
#>   - blobs_labels (64,64)
#>   - blobs_multiscale_labels (64,64)
#> - points(1):
#>   - blobs_points (200)
#> - shapes(0):
#> - tables(1):
#>   - table (3,10) [blobs_labels]
#> coordinate systems(5):
#> - global(5): blobs_image blobs_multiscale_image blobs_labels
#>   blobs_multiscale_labels blobs_points
#> - scale(1): blobs_labels
#> - translation(1): blobs_labels
#> - affine(1): blobs_labels
#> - sequence(1): blobs_labels
x[4, -2] # drop element
#> dropping table 'table' because all its annotated regions were removed
#> class: SpatialData
#> - images(0):
#> - labels(0):
#> - points(0):
#> - shapes(2):
#>   - blobs_circles (5,circle)
#>   - blobs_polygons (5,polygon)
#> - tables(0):
#> coordinate systems(1):
#> - global(2): blobs_circles blobs_polygons
x["shapes", c(1, 3)] # subset layer
#> dropping table 'table' because all its annotated regions were removed
#> class: SpatialData
#> - images(0):
#> - labels(0):
#> - points(0):
#> - shapes(2):
#>   - blobs_circles (5,circle)
#>   - blobs_polygons (5,polygon)
#> - tables(0):
#> coordinate systems(1):
#> - global(2): blobs_circles blobs_polygons
x[c(1, 2), list(1, c(1, 2))] # multiple
#> class: SpatialData
#> - images(1):
#>   - blobs_image (3,64,64)
#> - labels(2):
#>   - blobs_labels (64,64)
#>   - blobs_multiscale_labels (64,64)
#> - points(0):
#> - shapes(0):
#> - tables(0):
#> coordinate systems(5):
#> - global(3): blobs_image blobs_labels blobs_multiscale_labels
#> - scale(1): blobs_labels
#> - translation(1): blobs_labels
#> - affine(1): blobs_labels
#> - sequence(1): blobs_labels
```
