rgb <- seq_len(255)

test_that("ImageArray()", {
    val <- sample(rgb, 3*20*20, replace=TRUE)
    mat <- array(val, dim=c(3, 20, 20))
    # invalid
    expect_error(ImageArray(mat))
    expect_error(ImageArray(mat, 1))
    expect_error(ImageArray(mat, list()))
    # single scale
    expect_silent(ImageArray(list()))
    expect_silent(ImageArray(list(mat)))
    expect_silent(ImageArray(list(mat), Zattrs()))
    # multiscale
    dim <- lapply(c(20, 10, 5), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(sample(rgb, prod(.), replace=TRUE), dim=.))
    expect_silent(ImageArray(lys))
})

test_that("data(),ImageArray", {
    dim <- lapply(c(8, 4, 2), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(0, dim=.))
    img <- ImageArray(lys)
    for (. in seq_along(lys))
        expect_identical(data(img, .), lys[[.]])
    expect_identical(data(img, Inf), lys[[3]])
    expect_error(data(img, 0))
    expect_error(data(img, -1))
    expect_error(data(img, 99))
    expect_error(data(img, ""))
    expect_error(data(img, c(1,2)))
})
