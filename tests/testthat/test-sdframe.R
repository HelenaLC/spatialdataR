require(sf, quietly=TRUE)

test_that("new,PointFrame", {
    # 1. Empty construction
    expect_silent(p0 <- PointFrame())
    expect_s4_class(p0, "PointFrame")
    expect_equal(nrow(p0), 0)
    
    # 2. Construction from data.frame with x, y
    df <- data.frame(x=1:5, y=1:5, genes=letters[1:5], cell_id=1:5)
    expect_silent(p1 <- PointFrame(df))
    expect_equal(nrow(p1), 5)
    expect_true("geometry" %in% names(p1))
    
    # 3. Supplying ik and fk
    expect_silent(p2 <- PointFrame(df, ik="cell_id", fk="genes"))
    expect_equal(instance_key(p2), "cell_id")
    expect_equal(feature_key(p2), "genes")
    
    # 4. Geometry validation (must be POINT)
    # Use sf object to force non-POINT geometry
    poly <- st_sfc(st_polygon(list(matrix(c(0,1,1,0,0,0,0,1,1,0), ncol=2))))
    df_poly <- st_sf(data.frame(a=1), geometry=poly)
    expect_error(PointFrame(df_poly), "only 'POINT' geometries supported")
})

test_that("new,ShapeFrame", {
    # 1. Empty construction
    expect_silent(s0 <- ShapeFrame())
    expect_s4_class(s0, "ShapeFrame")
    expect_equal(nrow(s0), 0)
    
    # 2. Construction from data.frame with x, y (points)
    df_pts <- data.frame(x=1:5, y=1:5)
    expect_silent(s1 <- ShapeFrame(df_pts))
    expect_equal(nrow(s1), 5)
    expect_equal(geom_type(s1), "POINT")
    
    # 3. Polygon creation from x, y, i
    df_poly <- data.frame(
        x = c(0, 1, 1, 0, 0,  2, 3, 3, 2, 2),
        y = c(0, 0, 1, 1, 0,  2, 2, 3, 3, 2),
        i = c(1, 1, 1, 1, 1,  2, 2, 2, 2, 2)
    )
    expect_silent(s2 <- ShapeFrame(df_poly))
    expect_equal(nrow(s2), 2)
    expect_equal(geom_type(s2), "POLYGON")
    expect_setequal(rownames(as.data.frame(s2)), c("1", "2"))
})
