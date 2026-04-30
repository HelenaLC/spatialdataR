zs <- system.file("extdata", "blobs.zarr", package="SpatialData")
sd <- readSpatialData(zs)

test_that("query() correctly filters SpatialData elements based on table annotations", {
    # 1. Basic query: filter the main table (annotating blobs_labels)
    t <- table(sd)
    all_instances <- instances(t)
    n_total <- length(all_instances)
    
    # keep only first 5 instances
    keep_ids <- head(all_instances, 5)
    sd_q <- query(sd, i=1, instance_id %in% keep_ids)
    
    expect_s4_class(sd_q, "SpatialData")
    expect_equal(ncol(table(sd_q)), 5)
    expect_setequal(instances(table(sd_q)), keep_ids)
    
    # By default, blobs table only annotates 'blobs_labels'. 
    # query() removes elements NOT in the filtered table's regions.
    expect_length(images(sd_q), 0)
    expect_length(points(sd_q), 0)
    expect_length(shapes(sd_q), 0)
    expect_length(labels(sd_q), 1)
    expect_true("blobs_labels" %in% labelNames(sd_q))
    
    # 2. Query propagation to points
    # We'll create a mock table that annotates points
    p_name <- pointNames(sd)[1] # blobs_points
    p_element <- point(sd, p_name)
    p_instances <- instances(p_element)
    
    sce_p <- SingleCellExperiment(matrix(0, 1, length(p_instances)))
    int_metadata(sce_p)$spatialdata_attrs <- list(
        region = p_name,
        region_key = "region",
        instance_key = instance_key(p_element)
    )
    # Using factor for region as observed in blobs
    int_colData(sce_p)[[instance_key(p_element)]] <- p_instances
    int_colData(sce_p)$region <- factor(rep(p_name, length(p_instances)))
    
    # Add table to sd
    tabs <- tables(sd)
    tabs$points_table <- sce_p
    tables(sd) <- tabs
    
    # Filter points using the new table
    keep_p_ids <- head(p_instances, 3)
    n_keep <- sum(p_instances %in% keep_p_ids)
    # query uses instance_id because that's the column name in blobs points
    sd_q_p <- query(sd, i="points_table", instance_id %in% keep_p_ids)
    
    expect_equal(length(point(sd_q_p, p_name)), n_keep)
    expect_equal(ncol(sd_q_p$tables$points_table), n_keep)
    expect_setequal(instances(point(sd_q_p, p_name)), keep_p_ids)
    
    # Other layers should be gone because they are not in points_table's regions
    expect_length(labels(sd_q_p), 0)
    expect_length(shapes(sd_q_p), 0)
    
    # 3. Query propagation to shapes
    s_name <- shapeNames(sd)[1] # blobs_circles
    s_element <- shape(sd, s_name)
    # Add instance_id column to shapes to enable filtering by it
    # We modify the data frame directly in the ShapeFrame object
    s_element@data$instance_id <- seq_len(nrow(s_element))
    shape(sd, s_name) <- s_element
    
    s_instances <- instances(s_element)
    
    sce_s <- SingleCellExperiment(matrix(0, 1, length(s_instances)))
    int_metadata(sce_s)$spatialdata_attrs <- list(
        region = s_name,
        region_key = "region",
        instance_key = "instance_id"
    )
    int_colData(sce_s)$instance_id <- s_instances
    int_colData(sce_s)$region <- factor(rep(s_name, length(s_instances)))
    
    tabs <- tables(sd)
    tabs$shapes_table <- sce_s
    tables(sd) <- tabs
    
    n_keep_s <- 2
    keep_s_ids <- head(s_instances, n_keep_s)
    sd_q_s <- query(sd, i="shapes_table", instance_id %in% keep_s_ids)
    
    expect_equal(ncol(sd_q_s$tables$shapes_table), n_keep_s)
    expect_equal(length(shape(sd_q_s, s_name)), n_keep_s)
    
    # 4. Error cases
    # No rows left
    expect_error(query(sd, i=1, instance_id == -1), "Nothing left after query")
    
    # No tables
    sd_empty <- sd
    tables(sd_empty) <- list()
    expect_error(query(sd_empty, i=1, TRUE), "There aren't any tables")
    
    # Invalid table index/name
    expect_error(query(sd, i=99, TRUE))
    expect_error(query(sd, i="non_existent_table", TRUE))
})
