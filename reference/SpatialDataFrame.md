# `SpatialDataFrame`

The `SpatialDataPoint` and `-Shape` classes represent elements from a
`SpatialData`'s `points/` and `shapes/` layers, respectively. In both
cases, these are represented as a `duckspatial_df` (`data` slot), and
associated with .zattrs represented as
[`SpatialDataAttrs`](https://helenalc.github.io/SpatialData/reference/SpatialDataAttrs.md)
(`meta` slot); a list of `metadata` stores other arbitrary info.

Currently defined methods (here, `x` is an `SpatialDataFrame`):

- `data/meta(x)` access underlying data/.zattrs

- `geom_type(x)` get the shape's type (e.g., POLYGON)

- `names(x)` returns the underlying table's column names

- `dim(x)` returns the dimensions of `data(x)`

- `` `$`,`[[` `` directly access columns of `data(x)`

- `filter,select` to subset rows/columns à la `dplyr`

- `as.data.frame` to coerce `x` to a `data.frame`

## Usage

``` r
SpatialDataPoint(
  data = NULL,
  meta = SpatialDataAttrs(type = "frame"),
  metadata = list(),
  ik = NULL,
  fk = NULL,
  ...
)

SpatialDataShape(
  data = NULL,
  meta = SpatialDataAttrs(type = "frame"),
  metadata = list(),
  ...
)

# S4 method for class 'SpatialDataFrame'
length(x)

# S4 method for class 'SpatialDataFrame'
dim(x)

# S4 method for class 'SpatialDataFrame'
names(x)

# S4 method for class 'SpatialDataFrame'
as.data.frame(x)

# S4 method for class 'SpatialDataShape'
geom_type(x)

# S3 method for class 'SpatialDataFrame'
pull(.data, ...)

# S3 method for class 'SpatialDataFrame'
select(.data, ...)

# S3 method for class 'SpatialDataFrame'
mutate(.data, ...)

# S3 method for class 'SpatialDataFrame'
filter(.data, ...)

# S4 method for class 'SpatialDataFrame,ANY,ANY'
x[[i, j, ...]]

# S4 method for class 'SpatialDataPoint'
x$name

# S3 method for class 'SpatialDataShape'
.DollarNames(x, pattern = "")

# S4 method for class 'SpatialDataShape'
x$name

# S4 method for class 'SpatialDataFrame,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]
```

## Arguments

- data:

  `duckspatial_df` for on-disk representation, or a `data.frame` to be
  converted.

- meta:

  [`SpatialDataAttrs`](https://helenalc.github.io/SpatialData/reference/SpatialDataAttrs.md)

- metadata:

  optional list of arbitrary content describing the overall object.

- ik, fk:

  character string specifying "instance\_/feature_key" of the
  spatialdata_attrs; used to match observations/features.

- ...:

  optional arguments passed to and from other methods.

- x, .data:

  `SpatialDataFrame`

- i, j:

  indices for subsetting (see
  [`?base::Extract`](https://rdrr.io/r/base/Extract.html)).

- name:

  character string for extraction (see
  [`` ?base::`$` ``](https://rdrr.io/r/base/Extract.html)).

- drop, pattern:

  ignored.

## Value

`SpatialDataFrame`

## Examples

``` r
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")

# points
pa <- list.dirs(
  file.path(zs, "points"), 
  recursive=FALSE, full.names=TRUE)
(x <- readPoint(pa))
#> class: SpatialDataPoint
#> count: 200 
#> data(4): genes instance_id __null_dask_index__ geometry

y <- filter(x,
  genes == "gene_b",
  instance_id == 7) 
head(as.data.frame(y))
#>    genes instance_id __null_dask_index__      geometry
#> 1 gene_b           7                   1 POINT (41 28)
#> 2 gene_b           7                  70 POINT (61 14)
#> 3 gene_b           7                  75 POINT (54 18)
#> 4 gene_b           7                  83 POINT (20 11)
#> 5 gene_b           7                  92 POINT (31 28)
#> 6 gene_b           7                 111 POINT (44 13)

# shapes
pa <- list.dirs(
  file.path(zs, "shapes"), 
  recursive=FALSE, full.names=TRUE)

# circles
(x <- readShape(pa[1]))
#> class: SpatialDataShape
#> count: 5 
#> data(2): geometry radius
length(x)
#> [1] 5
x$radius
#> [1] 6 6 6 6 6

# polygons
(y <- readShape(pa[2]))
#> class: SpatialDataShape
#> count: 2 
#> data(1): geometry
df <- as.data.frame(y)
plot(df, col=seq(nrow(df)))


# multi-polygons
(z <- readShape(pa[3]))
#> class: SpatialDataShape
#> count: 5 
#> data(1): geometry
df <- as.data.frame(z)
plot(df, col=seq(nrow(df)))
```
