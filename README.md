# SpatialData

[![R-universe](https://github.com/HelenaLC/spatialdataR/actions/workflows/r-universe.yaml/badge.svg?branch=main&event=push)](https://github.com/HelenaLC/spatialdataR/actions/workflows/r-universe.yaml)

`spatialdataR` provides an R interface to Python's [spatialdata](https://spatialdata.scverse.org) framework. 
It enables the representation, handling, and integration of diverse spatial omics datasets 
using the [OME-NGFF (Next Generation File Format)](https://ngff.openmicroscopy.org) standard.
For more details on the framework, see [Marconato et al. (2024)](https://doi.org/10.1038/s41592-024-02212-x).

## Resources

- [SpatialData class](https://helenalc.github.io/spatialdataR/articles/spatialdataR.html) documentation.
- [SpatialData.plot](https://github.com/HelenaLC/SpatialData.plot): Visualization capabilities.
- [SpatialData.demo](https://helenalc.github.io/SpatialData.demo/): Biotechnology workflows.
- [SpatialData.data](https://github.com/HelenaLC/SpatialData.data): Example `SpatialData`sets.

## Key features

- Out-of-memory handling of images and labels using `ZarrArray` (via the [Rarr](https://bioconductor.org/packages/Rarr) package).
- Points and shapes are managed using [duckdb](https://cran.r-project.org/package=duckdb)-backed tables.
- Functional annotations (e.g., gene expression) are represented as `SingleCellExperiment` objects, integrated via [anndataR](https://bioconductor.org/packages/anndataR).
- A system for mapping data across multiple coordinate spaces, including support for transformation graphs.

## Installation

```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")

# Install the development version from GitHub
BiocManager::install("HelenaLC/spatialdataR")
```

## Quick Start

```r
library(spatialdataR)
zs <- system.file("extdata", "blobs.zarr", package="spatialdataR")
(sd <- readSpatialData(zs))
```

```
class: SpatialData   
- images(2):
  - blobs_image (3,64,64)
  - blobs_multiscale_image (3,64,64)
- labels(2):
  - blobs_labels (64,64)
  - blobs_multiscale_labels (64,64)
- points(1):
  - blobs_points (200)
- shapes(3):
  - blobs_circles (5,circle)
  - blobs_multipolygons (2,polygon)
  - blobs_polygons (5,polygon)
- tables(1):
  - table (3,10) [blobs_labels]
coordinate systems(5):
- global(8): blobs_image
  blobs_multiscale_image ... blobs_polygons
  blobs_points
- scale(1): blobs_labels
- translation(1): blobs_labels
- affine(1): blobs_labels
- sequence(1): blobs_labels
```

***

*Past and current contributors include (in alphabetical order):
Vince Carey, Helena L. Crowell, Louise Deconinck, Yixing E. Dong, Hugo Gruson, 
Samuel Gunz, Artür Manukyan, Dario Righelli, Charlotte Soneson, Michael Stadler.*
