library(SpatialData)

context("checking cache operations for zipped zarr")

test_that("available_sdio()", {
    expect_error(available_sdio(1))
    x <- available_sdio()
    expect_is(x, "character")
    expect_true(length(x) > 0)
    expect_true(any(grepl("^(vis|xen)", x)))
})

test_that("use_sdio()", {
    # get dataset
    zip <- path_to_10x_xen_demo()
    dir.create(src <- tempfile())
    unzip(zip, exdir=src)
    # directory already exists
    dir.create(out <- tempfile())
    expect_error(use_sdio("xenium", src, out))
    # invalid platform specification
    out <- tempfile()
    expect_error(use_sdio(".", src, out))
    # read'n'write using 'spatialdata-io'
    expect_silent(use_sdio("xenium", src, out))
    x <- readSpatialData(out)
    expect_s4_class(x, "SpatialData")
})

test_that("OSN payloads exist", {
    if (requireNamespace("paws", quietly=TRUE)) {
        x <- available_spd_zarr_zips()
        expect_gt(length(x), 0)
        expect_is(x, "character")
        expect_true(all(grepl("\\.zip$", x)))
    }
})

test_that("merfish demo zip has expected content", {
    tf <- tempfile()
    dir.create(tf)
    bfc <- BiocFileCache::BiocFileCache()
    dem <- unzip_merfish_demo(cache=bfc, destination=tf)
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
