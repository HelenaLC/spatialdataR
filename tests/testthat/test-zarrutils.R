test_that("create zarr/group", {
  
  # open zarr
  store <- tempfile(fileext = ".zarr")
  create_zarr(store = store)
  expect_true(dir.exists(store))
  expect_true(file.exists(file.path(store, ".zgroup")))
  
  # create group one group
  create_zarr_group(store = store, name = "group1")
  expect_true(dir.exists(file.path(store, "group1")))
  expect_true(file.exists(file.path(store, "group1", ".zgroup")))
  
  # create nested two groups
  create_zarr_group(store = store, name = "group2/subgroup1")
  expect_true(dir.exists(file.path(store, "group2")))
  expect_true(file.exists(file.path(store, "group2", ".zgroup")))
  expect_true(dir.exists(file.path(store, "group2/subgroup1")))
  expect_true(file.exists(file.path(store, "group2/subgroup1", ".zgroup")))
  
  # create nested three groups
  create_zarr_group(store = store, name = "group3/subgroup1/subsubgroup1")
  expect_true(dir.exists(file.path(store, "group3")))
  expect_true(file.exists(file.path(store, "group3", ".zgroup")))
  expect_true(dir.exists(file.path(store, "group3/subgroup1")))
  expect_true(file.exists(file.path(store, "group3/subgroup1", ".zgroup")))
  expect_true(dir.exists(file.path(store, "group3/subgroup1/subsubgroup1")))
  expect_true(file.exists(file.path(store, "group3/subgroup1/subsubgroup1", ".zgroup")))
  
  # invalid version string
  dir.create(td <- tempfile())
  name <- "test"
  expect_error(create_zarr(dir = td, name = name, version = "4"), pattern = "version must be '2' or '3'")
})

test_that("create zarr/group v3", {

  # open v3 zarr store
  store <- tempfile(fileext = ".zarr")
  create_zarr(store = store, version = 3)
  expect_true(dir.exists(store))
  expect_true(file.exists(file.path(store, "zarr.json")))
  expect_false(file.exists(file.path(store, ".zgroup")))

  # check zarr.json exists and attributes are empty
  expect_true(file.exists(file.path(store, "zarr.json")))
  expect_equal(Rarr::read_zarr_attributes(store), list())

  # create a sub-group
  create_zarr_group(store = store, name = "images", version = 3)
  expect_true(file.exists(file.path(store, "images", "zarr.json")))
  expect_false(file.exists(file.path(store, "images", ".zgroup")))

  # create nested groups — parent group should also be v3
  create_zarr_group(store = store, name = "points/blobs_points", version = 3)
  expect_true(file.exists(file.path(store, "points", "zarr.json")))
  expect_true(file.exists(file.path(store, "points/blobs_points", "zarr.json")))
})


# create a v2 zarr array for the v2 zattrs tests
dir.create(td <- tempfile())
path <- file.path(td, "test.zarr")
x <- array(runif(n = 10), dim = c(2, 5))
Rarr::write_zarr_array(
  x = x, zarr_array_path = path,
  chunk_dim = c(2, 5), zarr_version = 2L
)

test_that("read/write zattrs", {
  
  # add .zattrs to /
  zattrs <- list(foo = "foo", bar = "bar")
  Rarr::write_zarr_attributes(path, new.zattrs = zattrs)
  expect_true(file.exists(file.path(path, ".zattrs")))

  # check .zattrs
  read.zattrs <- Rarr::read_zarr_attributes(path)
  expect_equal(read.zattrs, zattrs)

  # add new elements to .zattrs
  zattrs.new.elem <- list(foo2 = "foo")
  Rarr::write_zarr_attributes(path, new.zattrs = zattrs.new.elem)
  read.zattrs <- Rarr::read_zarr_attributes(path)
  expect_equal(read.zattrs, c(zattrs,zattrs.new.elem))

  # overwrite
  zattrs.new.elem <- list(foo2 = "foo2")
  Rarr::write_zarr_attributes(path, new.zattrs = zattrs.new.elem)
  read.zattrs <- Rarr::read_zarr_attributes(path)
  zattrs[names(zattrs.new.elem)] <- zattrs.new.elem
  expect_equal(read.zattrs, c(zattrs))

  # overwrite = FALSE
  zattrs.new.elem <- list(foo2 = "foo")
  Rarr::write_zarr_attributes(path, new.zattrs = zattrs.new.elem, overwrite = FALSE)
  read.zattrs <- Rarr::read_zarr_attributes(path)
  zattrs[names(zattrs.new.elem)] <- "foo2"
  expect_contains(read.zattrs, zattrs)
  expect_contains(zattrs, read.zattrs)
})

test_that("read/write zattrs v3", {

  # create a v3 zarr group to use as the target path
  dir.create(td <- tempfile())
  grp <- file.path(td, "elem")
  create_zarr_group(store = td, name = "elem", version = 3)

  # write attributes into zarr.json
  zattrs <- list(foo = "foo", bar = "bar")
  Rarr::write_zarr_attributes(grp, new.zattrs = zattrs)
  expect_true(file.exists(file.path(grp, "zarr.json")))
  expect_false(file.exists(file.path(grp, ".zattrs")))

  # read back attributes from zarr.json
  read.zattrs <- Rarr::read_zarr_attributes(grp)
  expect_equal(read.zattrs, zattrs)

  # zarr.json must still exist (zarr_format / node_type preserved by Rarr internally)
  expect_true(file.exists(file.path(grp, "zarr.json")))

  # add new element
  Rarr::write_zarr_attributes(grp, new.zattrs = list(baz = "baz"))
  read.zattrs <- Rarr::read_zarr_attributes(grp)
  expect_equal(read.zattrs, c(zattrs, list(baz = "baz")))

  # overwrite existing key
  Rarr::write_zarr_attributes(grp, new.zattrs = list(foo = "FOO"))
  read.zattrs <- Rarr::read_zarr_attributes(grp)
  expect_equal(read.zattrs$foo, "FOO")
  expect_equal(read.zattrs$bar, "bar")   # untouched key preserved

  # overwrite = FALSE should not overwrite existing key
  Rarr::write_zarr_attributes(grp, new.zattrs = list(foo = "original"), overwrite = FALSE)
  read.zattrs <- Rarr::read_zarr_attributes(grp)
  expect_equal(read.zattrs$foo, "FOO")   # unchanged

})