x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

test_that("combine two SpatialData objects", {
    # auto-fixed names
    expect_no_message(y <- combine(x, x))
    f <- \(.) unlist(colnames(.))
    expect_all_true(f(x) %in% f(y))
    expect_length(f(y), 2*length(f(x)))
    r <- unlist(lapply(tables(y), region))
    expect_all_true(r %in% f(y))
    expect_true(!all(r %in% f(x)))
    expect_all_true(!duplicated(r))
    expect_true(r[1] == region(table(x)))
    
    f <- \(x, y) `names<-`(x, paste(names(x), y, sep="."))
    a <- b <- x
    # alter names
    for (. in rownames(x)) {
        a[[.]] <- f(a[[.]], "a")
        b[[.]] <- f(b[[.]], "b")
    }
    # alter data
    t <- assay(table(b))
    assay(table(b)) <- t+.37
    c <- combine(a, b)
    f <- \(.) unlist(colnames(.))
    expect_contains(f(c), f(a))
    expect_contains(f(c), f(b))
    expect_length(f(c), 2*length(f(x)))
    n <- vapply(colnames(x), length, integer(1))
    for (. in names(which(n == 1))) {
        expect_identical(
            colnames(c)[[.]], 
            paste(colnames(x)[[.]], c("a","b"), sep="."))
        expect_identical(c[[.]][[1]], a[[.]][[1]])
        expect_identical(c[[.]][[2]], b[[.]][[1]])
    }
})

test_that("combine length-2+ list of objects", {
    # partially named
    y <- combine(list(a=x, b=x, x))
    old <- unlist(colnames(x))
    new <- unlist(colnames(y))
    expect_true(all(old %in% new))
    expect_true(!any(duplicated(new)))
    expect_true(all(paste0("a.", old) %in% new))
    expect_true(all(paste0("b.", old) %in% new))
    expect_length(new, 3*length(unlist(colnames(x))))
    # unnamed
    y <- combine(list(x, x))
    new <- unlist(colnames(y))
    expect_true(all(old %in% new))
    expect_length(new, 2*length(old))
    expect_true(!any(duplicated(new)))
    expect_true(all(paste0(old, ".1") %in% new))
})
