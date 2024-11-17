# "SpatialData" R package

> [DEMO](https://htmlpreview.github.io/?https://github.com/HelenaLC/SpatialData/blob/main/vignettes/SpatialData.html)

This package explores strategies for Bioconductor-oriented solutions to
importing and analyzing data from Spatial Transcriptomics platforms.

# Installation

```
BiocManager::install("HelenaLC/SpatialData") # or use vjcitn repo
```

To *interrogate* our S3 bucket you will need [paws](https://cran.r-project.org/web/packages/paws/index.html) 
installed; it is not necessary for retrievals.

# Ingestion workflow

## Query Bioconductor Open Storage Network Bucket

```
> available_spd_zarr_zips()  # as of Nov 10 2024
checking Bioconductor OSN bucket...
[1] "mcmicro_io.zip"                         
[2] "merfish.zarr.zip"                       
[3] "mibitof.zip"                            
[4] "steinbock_io.zip"                       
[5] "visium_associated_xenium_io_aligned.zip"
[6] "visium_hd_3.0.0_io.zip"                 
```

## Bring a zip archive into your local cache

```
tf = tempfile()
dir.create(tf)
pa = unzip_spd_demo(zipname="mibitof.zip", destination=tf, source="biocOSN")
dir(pa, full.names=TRUE)  # see the files
```

## Import the SpatialData instance, and work with it

```
mibi = readSpatialData(pa)
```
Outputs:
```
> mibi
class: SpatialData
images(3): point16_image point23_image point8_image
labels(3): point16_labels point23_labels point8_labels
shapes(0):
points(0):
tables(1): table
```

```
> table(mibi)
class: SingleCellExperiment 
dim: 36 3309 
metadata(1): spatialdata_attrs
assays(1): X
rownames(36): ASCT2 ATP5A ... XBP1 vimentin
rowData names(0):
colnames(3309): 9376-1 9377-1 ... 4273-0 4274-0
colData names(12): row_num point ... batch library_id
reducedDimNames(3): X_scanorama X_umap spatial
mainExpName: NULL
altExpNames(0):
```

# TODO: build provenance for each example

```
make_spd_prov = function( outfile=tempfile(), zarr_url,
   prose_tag,
   pub_url,
   date_uploaded) {
   if (missing(date_uploaded)) stop("must supply upload date")
   if (missing(pub_url)) stop("must supply pub_url")
   if (missing(prose_tag)) stop("must supply upload prose_tag")
   basic = list(
    SpatialDataTag = prose_tag,
    zarr_url = zarr_url,
    pub_url = pub_url,
    date_uploaded = date_uploaded)
  jsonlite::write_json(jsonlite::toJSON(basic), outfile)
}

make_spd_prov(zarr_url = "https://s3.embl.de/spatialdata/spatialdata-sandbox/xenium_rep1_io_aligned.zip",
   prose_tag = "spatialdata notebooks aligned rep1",
   pub_url = "https://pubmed.ncbi.nlm.nih.gov/38114474/",
   date_uploaded = "2024.11.10")
```
