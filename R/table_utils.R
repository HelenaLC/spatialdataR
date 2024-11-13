setMethod("getRegionData", c("SpatialData", "character"), \(x, re) {
    se <- table(x, 1)
    md <- metadata(se)[[1]]
    se <- se[, se[[md$region_key]] == re]
    se
})