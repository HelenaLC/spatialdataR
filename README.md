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
=======

- [raccoon](https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/devel/vignettes/SpatialData.html)

- [mibitof](https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/devel/vignettes/mibitof.html)

## Useful links
- Specs for raster-type data (images, segmentation masks) follow [OME-NGFF][]
- Specs for coordinate systems and transformations follow OME-NGFF proposal:
    - [Proposal docs][]
    - [Proposal PR][]
- Specs for shapes, polygons and table generally follow the spatialdata [design doc][]
- Design document for [AnnData<>SCE][] integration

## Qs for GP,LM,...

- Plotting high-resolution images (e.g., in the CosMx example) takes a hell of a long time. Are you rescaling these for visualization [here](https://spatialdata.scverse.org/en/latest/tutorials/notebooks/notebooks/examples/technology_cosmx.html) and simply keeping the original axis labels? Or does Python do this by magick?

- I'm assuming that people also plot multiple elements together, however, I am not seeing this in any of the demos. Would be could to have a data set to implement & test this. I.e., in the current examples, only one "stack" of image/label/point is in each plot.

- Related to the above: My understanding is that the 1st entries of each element type somehow match. However, they have different identifiers. If this is a design choice, ok, but it's rather confusing (e.g., "1_image" and "1_point"); and, I wonder whether things break anywhere if that ordering is not upheld?

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
|sequence  |&#x2714;|&#x2714;|&#x2714;|&#x2714;
|map axis  |&#x274C;|&#x274C;|&#x274C;|&#x274C;

- [x] IO for Elements (and associated metadata)
  - [x] Images
  - [x] Labels
  - [x] Shapes
  - [x] Points
  - [x] Table
  - [ ] link elements with Table data

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
