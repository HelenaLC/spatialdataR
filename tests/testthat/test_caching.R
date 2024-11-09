library(SpatialData)

context("checking cache operations for zipped zarr")

test_that("OSN payloads exist", {
  if (requireNamespace("paws")) {
   expect_true(length(available_spd_zarr_zips())>0L)
  }
})

test_that("merfish demo zip has expected content", {
  tf = tempfile()
  dir.create(tf)
  dem = unzip_merfish_demo(cache=BiocFileCache::BiocFileCache(), destination=tf)
  cont = dir(tf, full.names=TRUE, recursive=TRUE)
  expect_true(length(cont) == 29L)
  parq = grep("parquet", cont)
  expect_true(length(parq) == 3L)
})
 
