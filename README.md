# `SpatialData`

> for a demo of the class, see the [vignette](https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/main/vignettes/SpatialData.html)

> for visualization capabilites, see [`SpatialData.plot`](https://github.com/HelenaLC/SpatialData.plot)

> for data reading/writing and examples, see [`SpatialData.data`](https://github.com/HelenaLC/SpatialData.data)

# Introduction

`SpatialData` provides an interface to Python's `SpatialData`, and currently includes:

- reticulate-based use of `spatialdata-io` for reading of manufacturer data and writing to .zarr
- on-disk representation of images/labels as `ZarrArray`s (`Rarr`) and shapes/points as `arrow` objects
- method drafts for visualization and coordinate system handling

In addition, *scverse* data examples have been made available through 
Bioconductor's NSF OSN bucket, are accessible from within R, and `BiocFileCache`d; 
there's also an interface to aligning data into a common (identity) space.

# Installation

```
BiocManager::install("HelenaLC/SpatialData")
```
