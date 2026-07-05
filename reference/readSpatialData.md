# Reading \`SpatialData\`

Reading \`SpatialData\`

## Usage

``` r
readImage(x, ...)

readLabel(x, ...)

readPoint(x, ...)

readShape(x, ...)

readTable(x)

readSpatialData(
  x,
  images = TRUE,
  labels = TRUE,
  points = TRUE,
  shapes = TRUE,
  tables = TRUE
)
```

## Arguments

- x:

  For `readImage/Label/Point/Shape/Table`, path to a `SpatialData`
  element. For `readSpatialData`, path to a `SpatialData`-.zarr store.

- ...:

  option arguments passed to and from other methods.

- images, labels, points, shapes, tables:

  Control which elements should be read for each layer. The default,
  NULL, reads all elements; alternatively, may be FALSE to skip a layer,
  or a integer vector specifying which elements to read.

## Value

- For `readSpatialData`, a `SpatialData`.,

- For element readers, a `SpatialDataImage/Label/Point/Shape` or
  `SingleCellExperiment`.

## Examples

``` r
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")

# read complete Zarr store
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

# helper that gets path to last element in layer 'l'
fn <- \(.) tail(list.files(file.path(zs, .), full.names=TRUE), 1)

# read individual elements
(i <- readImage(fn("images")))
#> class:  SpatialDataImage (MultiScale) 
#> Scales (3): (3,64,64 3,32,32 3,16,16)
channels(i)
#> [1] 0 1 2

(p <- readPoint(fn("points")))
#> class: SpatialDataPoint
#> count: 200 
#> data(4): genes instance_id __null_dask_index__ geometry
as.data.frame(head(p))
#>    genes instance_id __null_dask_index__      geometry
#> 1 gene_b           9                   0  POINT (6 48)
#> 2 gene_b           7                   1 POINT (41 28)
#> 3 gene_b           3                   2 POINT (27 54)
#> 4 gene_a           9                   3  POINT (6 44)
#> 5 gene_b           4                   4  POINT (13 6)
#> 6 gene_b           5                   5 POINT (33 61)

(s <- readShape(fn("shapes")))
#> class: SpatialDataShape
#> count: 5 
#> data(1): geometry
data(s)
#> # A duckspatial lazy spatial table
#> # ● CRS: NA 
#> # ● Geometry column: geometry 
#> # ● Geometry type: POLYGON 
#> # ● Bounding box: xmin: 18.741 ymin: 23.578 xmax: 55.838 ymax: 57.732 
#> # Data backed by DuckDB (dbplyr lazy evaluation)
#> # Use ddbs_collect() or st_as_sf() to materialize to sf
#> #
#> # A query:  ?? x 1
#> # Database: DuckDB 1.5.4 [unknown@Linux 6.17.0-1018-azure:R 4.7.0/:memory:]
#>   geometry                                                                      
#>   <wk_wkb>                                                                      
#> 1 <POLYGON ((42.52463 32.27672, 39.52212 24.63317, 36.38277 25.66096, 42.52463 …
#> 2 <POLYGON ((35.51764 52.5567, 33.40608 46.41484, 32.37829 57.73151, 35.51764 5…
#> 3 <POLYGON ((25.39938 28.69106, 35.68826 25.55172, 24.37159 35.21175, 25.39938 …
#> 4 <POLYGON ((30.05745 24.60573, 28.40081 23.57794, 18.74077 30.03591, 30.05745 …
#> 5 <POLYGON ((55.83783 43.67915, 52.63577 32.36248, 46.1778 39.86363, 55.83783 4…
```
