# SpatialData in R

Draft implementation of a SpatialData class in R.

## Installation

```r
library(devtools)
install_github("HelenaLC/SpatialData")
```

## Tutorial

[Link to tutorial][]

## Useful links
- Specs for raster-type data (images, segmentation masks) follow [OME-NGFF][]
- Specs for coordinate systems and transformations follow OME-NGFF proposal:
    - [Proposal docs][]
    - [Proposal PR][]
- Specs for shapes, polygons and table generally follow the spatialdata [design doc][]
- Design document for [AnnData<>SCE][] integration

## Milestones

### Level 0
- [ ] preliminarily pass `R CMD check` and `BiocCheck`

### Check list

- [ ] IO for Elements (and associated metadata)
  - [x] Images (raster)
    - [ ] Multiscale
  - [x] Labels (raster)
    - [ ] Multiscale
  - [x] Shapes (polygons)
  - [x] Points
  - [ ] Table 

- [ ] Transformations
  - [ ] Affine
  - [x] Scale
  - [x] Translation
  - [x] Rotate
  - [ ] Sequence
  - [x] Identity
  - [ ] ByDimension
  - [ ] MapAxis

- [ ] Operations
  - [ ] Aggregation
  - [ ] Query

<!-- Links -->
[Link to tutorial]: https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/devel/inst/SpatialData.html
[OME-NGFF]: https://ngff.openmicroscopy.org/latest/
[Proposal docs]: http://api.csswg.org/bikeshed/?url=https://raw.githubusercontent.com/ome/ngff/b92f540dc95440f2d6b7012185b09c2b862aa744/latest/index.bs
[Proposal PR]:https://github.com/ome/ngff/pull/138
[design doc]: https://spatialdata.scverse.org/en/latest/design_doc.html
[AnnData<>SCE]: https://github.com/scverse/scverseio/blob/main/doc/design.md
