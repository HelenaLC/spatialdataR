# `SpatialData` annotations

`SpatialData` annotations

## Usage

``` r
# S4 method for class 'SingleCellExperiment'
meta(x)

# S4 method for class 'SpatialData,ANY'
hasTable(x, i)

# S4 method for class 'SpatialData,character'
hasTable(x, i, name = FALSE)

# S4 method for class 'SpatialData,ANY'
getTable(x, i, j, assay = 1, drop = TRUE)

# S4 method for class 'SpatialData,character'
getTable(x, i, j, assay = 1, drop = TRUE)

# S4 method for class 'SpatialData,ANY'
setTable(x, i, ..., name = NULL, rk = "rk", ik = "ik")

# S4 method for class 'SpatialData,character'
setTable(x, i, y, name = NULL, rk = "region", ik = "instance_id")
```

## Arguments

- x:

  [`SpatialData`](https://helenalc.github.io/SpatialData/reference/SpatialData.md)
  object.

- i:

  character string; name of the element for which to get/set a `table`.

- name:

  logical; should the `table` name be returned instead of TRUE/FALSE?

- j:

  character string; `colData` column, or row name to retrieve `assay`
  data.

- assay:

  character string or scalar integer; specifies which `assay` to use
  when `j` is a row name.

- drop:

  logical; should observations (columns) that don't belong to `i` be
  filtered out?

- ...:

  option arguments passed to and from other methods.

- rk, ik:

  character string; region and instance key (the latter will be ignored
  if an instance key is already specified within element `i`).

- y:

  `SingleCellExperiment` containing annotations for `i`.

## Value

- `hasTable`: logical scalar (or character string, if `name=TRUE`);
  whether or not a `table` annotating `i` exists in `x`

- `getTable`: `SingleCellExperiment`; the `table` annotating `i` with
  optional filtering of matching observations

- `valTable`: vector of values (according to `j`) from the `table`
  annotating `i`

## Examples

``` r
library(SingleCellExperiment)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

# check if element has a 'table'
hasTable(x, "blobs_points")
#> [1] FALSE
hasTable(x, "blobs_labels")
#> [1] TRUE

# retrieve 'table' for element 'i'
sce <- getTable(x, i="blobs_labels")
head(colData(sce))
#> DataFrame with 6 rows and 0 columns
meta(sce)
#> $instance_key
#> [1] "instance_id"
#> 
#> $region
#> [1] "blobs_labels"
#> 
#> $region_key
#> [1] "region"
#> 

# get values from 'table'
getTable(x,
  i="blobs_labels",
  j="channel_0_sum")
#>          3          4          5          8         10         11         12 
#> 25.0537079 10.6787960  3.2422082  3.4709382  7.7414086  0.9912003 40.3020305 
#>         13         15         16 
#>  0.2252217  1.2125781  7.0430209 

# add 'table' annotating an element 'i'

# labels
y <- x; tables(y) <- list()
mtx <- matrix(0, 1, length(instances(label(y))))
sce <- SingleCellExperiment(list(counts=mtx))
y <- setTable(y, i <- "blobs_labels", sce)
getTable(y, i)
#> class: SingleCellExperiment 
#> dim: 1 10 
#> metadata(0):
#> assays(1): counts
#> rownames: NULL
#> rowData names(0):
#> colnames: NULL
#> colData names(0):
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):

# shapes
i <- "blobs_circles"
mtx <- matrix(0, 1, nrow(shape(x, i)))
sce <- SingleCellExperiment(list(counts=mtx))
y <- setTable(x, i, sce)
getTable(y, i)
#> class: SingleCellExperiment 
#> dim: 1 5 
#> metadata(0):
#> assays(1): counts
#> rownames: NULL
#> rowData names(0):
#> colnames: NULL
#> colData names(0):
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
```
