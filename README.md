# SpatialData: An R interface for OME-NGFF spatial omics

[![Bioc Check](https://github.com/HelenaLC/SpatialData/actions/workflows/check-bioc.yml/badge.svg?branch=main&event=push)](https://github.com/HelenaLC/SpatialData/actions/workflows/check-bioc.yml)

`SpatialData` provides an R interface to the [scverse](https://scverse.org) [SpatialData](https://spatialdata.scverse.org) framework. It enables the representation, handling, and integration of diverse spatial omics datasets using the [OME-NGFF (Next Generation File Format)](https://ngff.openmicroscopy.org) standard.

For more details on the framework, see the *Nature Methods* publication: [Marconato et al. (2024)](https://doi.org/10.1038/s41592-024-02212-x).

## Key Features

- **Scalable Representation**: Out-of-memory handling of large-scale images and segmentation labels using `ZarrArray` (via the [Rarr](https://bioconductor.org/packages/Rarr) package).
- **Efficient Geometry Handling**: Points and shapes are managed using [arrow](https://cran.r-project.org/package=arrow) or [duckdb](https://cran.r-project.org/package=duckdb)-backed tables.
- **Bioconductor Integration**: Functional annotations (e.g., gene expression) are represented as `SingleCellExperiment` objects, integrated via [anndataR](https://github.com/keller-mark/anndataR).
- **Coordinate Transformations**: A robust system for mapping data across multiple coordinate spaces, including support for transformation graphs.
- **Interoperability**: Seamlessly read and write OME-NGFF `.zarr` stores compatible with the Python `spatialdata` ecosystem.

## Installation

```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")

# Install the development version from GitHub
BiocManager::install("HelenaLC/SpatialData")
```

## Quick Start

```r
library(SpatialData)

# Read a SpatialData Zarr store
path <- system.file("extdata", "blobs.zarr", package="SpatialData")
sd <- readSpatialData(path)

# Access layers
imageNames(sd)
lbl <- label(sd, 1)

# Coordinate transformations
transform(lbl, "global")

# Spatial queries
sd_cropped <- crop(sd, list(xmin=10, xmax=50, ymin=10, ymax=50), j="global")
```

## Related Packages

- [SpatialData.plot](https://github.com/HelenaLC/SpatialData.plot): Visualization tools for `SpatialData` objects.
- [SpatialData.data](https://github.com/HelenaLC/SpatialData.data): Example datasets and data reading/writing utilities.

## Links

- **Vignette**: [Introduction to SpatialData](https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/main/vignettes/SpatialData.html)
- **Framework Documentation**: [spatialdata.scverse.org](https://spatialdata.scverse.org)
- **Community**: [scverse.org](https://scverse.org)
