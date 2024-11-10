# explore images()
library(SpatialData)
library(EBImage)
available_spd_zarr_zips()
td = tempfile()
dir.create(td)
ali = unzip_spd_demo(destination=td, zipname="visium_associated_xenium_io_aligned.zip")
rali = readSpatialData(ali)
th = images(rali)[[3]]@data  # simplify?
dim(th)
tha = as(th, "array")   # avoid?
dim(ptha <- aperm(tha, c(2,3,1)))
zz = Image(ptha/255)  # ??
plot(zz, all=TRUE)
