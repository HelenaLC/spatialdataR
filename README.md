# `SpatialData` in R

## [vignette][]

...currently including `blobs`, `raccoon`, and `merfish` [datasets][]

## status

<span style="color:red">TODOs</span> are shown in red,
all else is currently (more or less) supported.

### notes

- <span style="color:red">`as(<ShapeFrame>, "LabelArray")` coercion  
would probably make `mask()`ing a lot easier...</span>
- <span style="color:red">`as(<LabelArray>, "ShapeFrame")` coercion  
would probably make plotting a lot more efficient...</span>

### design

- `Zattrs` class for *.zattrs* (missing validity checks atm...)
- `SpatialData` class putting together various layers
- `Image/LabelArray,Shape/PointFrame` classes to represent elements

### handling

- get one/all element(s) via `image/label/shape/point/table(s)()`,  
and their names via via `image/label/shape/point/tableNames()`
- get (named, nested) list of available elements via `elements()`,  
and their names via `elementNames()`

### plotting

- `plotImages()`
  - <span style="color:red">rendering of selected channels</span>
  - <span style="color:red">option to display channel names</span>
- `plotLabels()`
  - color by `table` data
  - rendering of discrete and continuous scales
- `plotShapes()`
  - circles and polygons
  - scale resolution of circles inversely  
  with number of instances being rendered
  - aesthetics by `table` data
- `plotPoints()`
  - on-the-fly filtering
  - aesthetics by `table` data
- `pile()` to combine spatial layers by readjusting axiis

### masking

- `mask()` to aggregate signals across spatial layers `x,y` according to some summary function `fun`
- results are added as a `table` element where rows,columns correspond to data from `x,y`
  - `ImageArray` by `LabelArray`
  - <span style="color:red">`ImageArray` by `ShapeFrame`</span>
  - <span style="color:red">`LabelArray` by `ShapeFrame`</span>
  
### transformations

- accessors for coordinate system (CS) data
  - `coordNames()` gets output CS names
  - `coord()` gets one, `coords()` gets all CS data
  - when input is a `SpatialData` object, only shared CS (names) are returned
  - `axiis()` to access axis names (optional) by type ("space", "channel", ...)
  - `channels()` to access channel names (if axis of that type exists) 
  
- `alignElements()` to align arbitrary `SpatialData` elements
- `transformElement()` to transform an arbitrary `SpatialData` element

|           | image    | label    | shape    | point    |
|----------:|:--------:|:--------:|:--------:|:--------:| 
| identity  | &#x2714; | &#x2714; | &#x2714; | &#x2714; |
| translate | &#x2714; | &#x2714; | &#x2714; | &#x2714; |
| rotate    | &#x2714; | &#x2714; | &#x2714; | &#x2714; |
| scale     | &#x2714; | &#x2714; | &#x2714; | &#x2714; |
| affine    | &#x274C; | &#x274C; | &#x274C; | &#x274C; |
| sequence  | &#x2714; | &#x2714; | &#x2714; | &#x2714; |

## resources

- specs for raster-type data (images, segmentation masks) follow [OME-NGFF][]
- specs for coordinate systems and transformations follow OME-NGFF proposal:
    - [Proposal docs][]
    - [Proposal PR][]
- specs for shapes, polygons and tables generally follow the `spatialdata` [design doc][]
- design document for [AnnData<>SCE][] integration

<!-- Links -->
[OME-NGFF]: https://ngff.openmicroscopy.org/latest/
[Proposal docs]: http://api.csswg.org/bikeshed/?url=https://raw.githubusercontent.com/ome/ngff/b92f540dc95440f2d6b7012185b09c2b862aa744/latest/index.bs
[Proposal PR]:https://github.com/ome/ngff/pull/138
[design doc]: https://spatialdata.scverse.org/en/latest/design_doc.html
[AnnData<>SCE]: https://github.com/scverse/scverseio/blob/main/doc/design.md
[vignette]: https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/devel/vignettes/SpatialData.html
[datasets]: https://spatialdata.scverse.org/en/latest/tutorials/notebooks/datasets/README.html
