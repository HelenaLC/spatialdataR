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

test_that("data,ImageArray", {
    mtx <- array(sample(rgb, dim <- 3*20*20, replace=TRUE), dim=dim)
    img <- ImageArray(list(mtx))
    expect_silent(mty <- data(img, 1))
    expect_identical(mtx, mty)
    expect_error(data(img, 2))
})

test_that(".guess_scale", {
    img <- ImageArray(
        lys <- lapply(dim <- lapply(c(6, 3), \(.) c(3, rep(., 2))), 
        \(.) array(sample(rgb, prod(.), replace=TRUE), dim=.)))
    # manual scale
    expect_identical(.get_plot_data(img, k=1), lys[[1]]) 
    expect_identical(.get_plot_data(img, k=2), lys[[2]])
    # automatic scale
    expect_identical(.get_plot_data(img, k=NULL, width=5, height=5), lys[[1]]) 
    expect_identical(.get_plot_data(img, k=NULL, width=2, height=2), lys[[2]])
})
