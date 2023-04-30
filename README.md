# SpatialData in R

Draft implementation of a SpatialData class in R.

## Installation

```r
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("HelenaLC/SpatialData")
```
## [DEMO](https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/devel/vignettes/SpatialData.html)

## Useful links
- Specs for raster-type data (images, segmentation masks) follow [OME-NGFF][]
- Specs for coordinate systems and transformations follow OME-NGFF proposal:
    - [Proposal docs][]
    - [Proposal PR][]
- Specs for shapes, polygons and table generally follow the spatialdata [design doc][]
- Design document for [AnnData<>SCE][] integration

## NOTEs

- Presumably should split into separate packages in the long run,  
  e.g., `plotSpatialData` and `utilSpatialData` for convenience  
  visualization and query/wrangling/analysis functions.
- Although, in principle, we should be able to support `Array`s  
  (e.g., `DelayedArray`), nearly everything (except for _.parquet_)  
  is currently being read into memory because of `Rarr`.
- `EBImage` transformation work on 2D images only, so we cannot use  
  these for anything more sophisticated than scaling, shifting, rotating;  
  might have to do these ourselves if there are no alternatives to be found.

### Check list

- [ ] IO for Elements (and associated metadata)
  - [x] Images
    - [ ] Multiscale
  - [x] Labels
    - [ ] Multiscale
  - [x] Shapes
  - [x] Points (*missing metadata)
  - [x] Table

- [ ] Transformations
  - [ ] Affine
  - [ ] MapAxis
  - [ ] Sequence
  - [ ] ByDimension
  - [x] Identity
  - [x] Translation
  - [x] Scale
  - [x] Rotate

- [ ] Operations
  - [ ] Aggregation
    - [x] images by labels
    - [ ] images by shapes
    - [ ] points by labels
    - [ ] points by shapes
  - [ ] Query

<!-- Links -->
[Link to tutorial]: https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/devel/inst/SpatialData.html
[OME-NGFF]: https://ngff.openmicroscopy.org/latest/
[Proposal docs]: http://api.csswg.org/bikeshed/?url=https://raw.githubusercontent.com/ome/ngff/b92f540dc95440f2d6b7012185b09c2b862aa744/latest/index.bs
[Proposal PR]:https://github.com/ome/ngff/pull/138
[design doc]: https://spatialdata.scverse.org/en/latest/design_doc.html
[AnnData<>SCE]: https://github.com/scverse/scverseio/blob/main/doc/design.md
