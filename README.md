# SpatialData in R

Draft implementation of a SpatialData class in R.

## Installation

```r
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("HelenaLC/SpatialData")
```
## Demos

- [vignette](https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/devel/vignettes/SpatialData.html)
- [mibitof](https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/devel/vignettes/mibitof.html)

## Useful links
- Specs for raster-type data (images, segmentation masks) follow [OME-NGFF][]
- Specs for coordinate systems and transformations follow OME-NGFF proposal:
    - [Proposal docs][]
    - [Proposal PR][]
- Specs for shapes, polygons and table generally follow the spatialdata [design doc][]
- Design document for [AnnData<>SCE][] integration

## TODOs

### Transformations

<!-- yes: &#x2714; no: &#x274C; -->

|          | images | labels | shapes | points
|---------:|:------:|:------:|:------:|:------:
|identity  |&#x2714;|&#x2714;|&#x2714;|&#x2714;
|translate |&#x2714;|&#x2714;|&#x2714;|&#x2714;
|rotate    |&#x2714;|&#x2714;|&#x2714;|&#x2714;
|scale     |&#x2714;|&#x2714;|&#x2714;|&#x2714;
|affine    |&#x274C;|&#x274C;|&#x274C;|&#x274C;
|by dim.   |&#x274C;|&#x274C;|&#x274C;|&#x274C;
|sequence  |&#x274C;|&#x274C;|&#x274C;|&#x274C;
|map axis  |&#x274C;|&#x274C;|&#x274C;|&#x274C;

- [ ] IO for Elements (and associated metadata)
  - [x] Images
    - [ ] Multiscale
  - [x] Labels
    - [ ] Multiscale
  - [x] Shapes
  - [x] Points (*missing metadata)
  - [x] Table

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
