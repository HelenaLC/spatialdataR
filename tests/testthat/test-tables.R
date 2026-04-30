require(SingleCellExperiment, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

se <- SpatialData::table(x)
md <- int_metadata(se)
md <- md$spatialdata_attrs
i <- md[[rk <- md$region_key]]

test_that("hasTable()", {
    # TRUE
    i <- region(SpatialData::table(x))
    expect_true(hasTable(x, i))
    # FALSE
    j <- setdiff(unlist(colnames(x)), c(i, tableNames(x)))
    expect_true(all(!vapply(j, hasTable, x=x, logical(1))))
    # 'name' argument
    expect_error(hasTable(x, i, 123))
    expect_error(hasTable(x, i, "."))
    expect_error(hasTable(x, i, c(TRUE, FALSE)))
    expect_identical(hasTable(x, i, name=TRUE), tableNames(x))
    # invalid
    expect_error(hasTable(x, 123))
    expect_error(hasTable(x, "."))
    expect_error(hasTable(x, character(2)))
    expect_error(hasTable(x, sample(j, 1), name=TRUE)) # none
    expect_error(hasTable(setTable(x, i), i, name=TRUE)) # many
})

test_that("getTable()", {
    # invalid
    expect_error(getTable(x, 123))
    expect_error(getTable(x, "."))
    expect_error(getTable(x, character(2)))
    # valid
    expect_silent(t <- getTable(x, i))
    expect_identical(t, SpatialData::table(x))
    # 'drop' argument
    expect_error(getTable(x, i, 123))
    expect_error(getTable(x, i, "."))
    expect_error(getTable(x, i, c(TRUE, FALSE)))
    # alter 'region' of a couple random observations
    s <- t; y <- x
    int_colData(s)[[rk]] <- paste(int_colData(s)[[rk]])
    int_colData(s)[[rk]][. <- sample(ncol(s), 2)] <- "."
    SpatialData::table(y) <- s
    # these should be gone when 'drop=TRUE'
    t1 <- getTable(y, i, drop=FALSE)
    t2 <- getTable(y, i, drop=TRUE)
    expect_identical(t1, s)
    expect_identical(t2, s[, -.])
})

test_that("valTable()", {
    n <- ncol(t <- getTable(x, i))
    # invalid
    expect_error(getTable(x, i, "."))
    expect_error(getTable(x, i, 123))
    expect_error(getTable(x, i, sample(rownames(t), 2)))
    expect_error(getTable(x, i, sample(names(colData(t)), 2)))
    # 'colData'
    cd <- DataFrame(a=sample(letters, n), b=runif(n))
    s <- t; colData(s) <- cd
    y <- x; SpatialData::table(y) <- s
    expect_identical(getTable(y, i, j <- "a"), s[[j]])
    expect_identical(getTable(y, i, j <- "b"), s[[j]])
    expect_error(getTable(y, i, "c"))
    # 'assay' data
    j <- sample(rownames(t), 1)
    v <- getTable(x, i, j)
    expect_identical(v, assay(t)[j, ])
    # 'assay' argument
    assay(t, ".") <- 1+assay(t); SpatialData::table(x) <- t
    v <- getTable(x, i, j, assay=".")
    expect_identical(v, assay(t, ".")[j, ])
    expect_error(getTable(x, i, rownames(t)[1], assay=".."))
})

test_that("setTable(),labels", {
    # invalid 'i'
    expect_error(setTable(x, 123, SingleCellExperiment()))
    expect_error(setTable(x, ".", SingleCellExperiment()))
    expect_error(setTable(x, character(2), SingleCellExperiment()))
    # 'name' that already exists fails
    expect_error(setTable(x, i, SingleCellExperiment(), name=tableNames(x)))
    # valid w/o name
    e <- element(x, i)
    sce <- SingleCellExperiment(matrix(0, 0, length(instances(e))))
    # set instances manually
    int_colData(sce)$instance_id <- instances(e)
    y <- setTable(x, i, sce)
    expect_length(tables(y), 2)
    expect_true(hasTable(y, i))
})

test_that("setTable(),shapes", {
    for (. in c("shape")) {
        nms <- paste0(., "Names")
        i <- get(nms)(x)[1]
        e <- element(x, i)
        # ncol must match nrow(e)
        sce <- SingleCellExperiment(matrix(0, 0, nrow(e)))
        
        # valid
        expect_silent(y <- setTable(x, i, sce))
        expect_length(tables(y), 2)
        expect_true(hasTable(y, i))
        t <- getTable(y, i)
        expect_identical(region(t), i)
    }
})

test_that("setTable() correctly associates a SingleCellExperiment with an element", {
    tables(x) <- list() # clear existing tables
    
    # 1. Basic association with a label element
    i <- "blobs_labels"
    e <- element(x, i)
    sce <- SingleCellExperiment(matrix(0, 0, length(instances(e))))
    
    # Manually inject metadata and required colData columns
    int_metadata(sce)$spatialdata_attrs <- list(
        region = i,
        region_key = "region",
        instance_key = "instance_id"
    )
    int_colData(sce)$region <- factor(rep(i, ncol(sce)))
    int_colData(sce)$instance_id <- instances(e)
    
    sd_new <- setTable(x, i, sce)
    
    expect_true(paste0(i, "_table") %in% tableNames(sd_new))
    t <- getTable(sd_new, i)
    expect_equal(region(t), i)
})

test_that("setTable() handles custom name and keys", {
    tables(x) <- list() # clear existing tables
    
    i <- "blobs_circles"
    e <- element(x, i)
    sce <- SingleCellExperiment(matrix(0, 0, nrow(e)))
    
    # Manually inject metadata
    md <- list(region=i, region_key="my_rk", instance_key="my_ik")
    int_metadata(sce)$spatialdata_attrs <- md
    int_colData(sce)$my_rk <- factor(rep(i, ncol(sce)))
    int_colData(sce)$my_ik <- seq_len(nrow(e))
    
    sd_new <- setTable(x, i, sce, name="my_custom_table", rk="my_rk", ik="my_ik")
    
    expect_true("my_custom_table" %in% tableNames(sd_new))
    t <- SpatialData::table(sd_new, "my_custom_table")
    expect_equal(region_key(t), "my_rk")
    expect_equal(instance_key(t), "my_ik")
})

test_that("setTable() fails with invalid inputs", {
    e <- element(x, i <- "blobs_labels")
    
    # Not an SCE
    expect_error(setTable(x, i, data.frame(a=1)))
    
    # Mismatched dimensions (if instances are not set)
    # The current implementation checks ncol(y) vs nrow(e) if instances(y) is NULL
    sce_wrong <- SingleCellExperiment(matrix(0, 0, length(instances(e)) + 1))
    expect_error(setTable(x, i, sce_wrong), "ncol\\(y\\)' must match 'nrow\\(element\\(x, i\\)\\)'")
    
    # Non-existent element
    expect_error(setTable(x, "non_existent", SingleCellExperiment()), "is not an element of 'x'")
})
