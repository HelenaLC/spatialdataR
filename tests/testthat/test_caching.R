library(SpatialData)

context("checking cache operations for zipped zarr")

test_that("OSN payloads exist", {
    if (requireNamespace("paws"))
        expect_true(length(available_spd_zarr_zips()) > 0L)
})

test_that("merfish demo zip has expected content", {
    tf <- tempfile()
    dir.create(tf)
    dem <- unzip_merfish_demo(cache=BiocFileCache::BiocFileCache(), destination=tf)
    cont <- dir(tf, full.names=TRUE, recursive=TRUE)
    expect_true(length(cont) == 29L)
    parq <- grep("parquet", cont)
    expect_true(length(parq) == 3L)
})

clean_cache = function(zipname) {
    ca <- BiocFileCache::BiocFileCache()
    ans <- BiocFileCache::bfcquery(ca, zipname)
    if (nrow(ans) > 0) BiocFileCache::bfcremove(ca, ans$rid)
}
 
test_that("sandbox data can be acquired and used", {
    clean_cache("mibitof.zip")
    tf <- tempfile()
    dir.create(tf)
    dem <- unzip_spd_demo(
        zipname="mibitof.zip",
        cache=BiocFileCache::BiocFileCache(),
        destination=tf,
        source="sandbox")
    grab <- readSpatialData(dem)
    expect_true(all(dim(SpatialData::table(grab)) == c(36L, 3309L)))
})
