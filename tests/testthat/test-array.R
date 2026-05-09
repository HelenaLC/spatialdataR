rgb <- seq_len(255)

test_that("SpatialDataImage()", {
    val <- sample(rgb, 3*20*20, replace=TRUE)
    mat <- array(val, dim=c(3, 20, 20))
    # invalid
    expect_error(SpatialDataImage(mat))
    expect_error(SpatialDataImage(mat, 1))
    expect_error(SpatialDataImage(mat, list()))
    # single scale
    expect_silent(SpatialDataImage(list()))
    expect_silent(SpatialDataImage(list(mat)))
    expect_silent(SpatialDataImage(list(mat), SpatialDataAttrs()))
    # multiscale
    dim <- lapply(c(20, 10, 5), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(sample(rgb, prod(.), replace=TRUE), dim=.))
    expect_silent(SpatialDataImage(lys))
})

test_that("data(),SpatialDataImage", {
    dim <- lapply(c(8, 4, 2), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(0, dim=.))
    img <- SpatialDataImage(lys)
    for (. in seq_along(lys))
        expect_identical(data(img, .), lys[[.]])
    expect_identical(data(img, Inf), lys[[3]])
    expect_error(data(img, 0))
    expect_error(data(img, -1))
    expect_error(data(img, 99))
    expect_error(data(img, ""))
    expect_error(data(img, c(1,2)))
})

test_that("SpatialDataLabel()", {
    val <- sample(seq_len(12), 20*20, replace=TRUE)
    mat <- array(val, dim=c(20, 20))
    # invalid
    expect_error(SpatialDataLabel(mat))
    expect_error(SpatialDataLabel(mat, 1))
    expect_error(SpatialDataLabel(mat, list()))
    # single scale
    expect_silent(SpatialDataLabel(list()))
    expect_silent(SpatialDataLabel(list(mat)))
    expect_silent(SpatialDataLabel(list(mat), SpatialDataAttrs()))
    # multiscale
    dim <- lapply(c(20, 10, 5), \(.) rep(., 2))
    lys <- lapply(dim, \(.) array(sample(seq_len(12), prod(.), replace=TRUE), dim=.))
    expect_silent(SpatialDataLabel(lys))
})

test_that("data(),SpatialDataLabel", {
    dim <- lapply(c(8, 4, 2), \(.) rep(., 2))
    lys <- lapply(dim, \(.) array(0L, dim=.))
    lab <- SpatialDataLabel(lys)
    for (. in seq_along(lys))
        expect_identical(data(lab, .), lys[[.]])
    expect_identical(data(lab, Inf), lys[[3]])
    expect_error(data(lab, 0))
    expect_error(data(lab, -1))
    expect_error(data(lab, 99))
    expect_error(data(lab, ""))
    expect_error(data(lab, c(1,2)))
})
