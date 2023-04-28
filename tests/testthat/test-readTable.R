test_that("readTable", {
  path <- file.path("extdata", "mibitof", "table", "table")
  path <- system.file(path, package="SpatialData")
  sce <- readTable(path)
  expect_s4_class(sce, "SingleCellExperiment")
})
