require(duckspatial, quietly=TRUE)
require(arrow, quietly=TRUE)

# seed
set.seed(1)

example_points <- function(){
  data.frame(x = runif(100), y = runif(100))
}

example_circles <- function(){
  duckspatial::as_duckspatial_df(
    st_as_sf(
      arrow::arrow_table(
        geometry = geoarrow::as_geoarrow_vctr(
          c(
            "POINT (36.382774 24.6331748)",
            "POINT (32.378292 46.4148383)",
            "POINT (24.3715883 25.5517166)",
            "POINT (18.7407733 23.5779362)"
          )
        ),
        radius = c(4,4,4,4)
      ) 
    ),
    conn = duckspatial::ddbs_create_conn(dbdir = "memory"),
    wkt = "wkt",
    geom_col = "geometry",
    remove = TRUE
  )
}

example_polygons <- function(){
  duckspatial::as_duckspatial_df(
    st_as_sf(
      arrow::arrow_table(
        geometry = geoarrow::as_geoarrow_vctr(
          c(
            "POLYGON ((4.53 2.11, 5.55 1.43, 5.78 1.33, 6.89 9.10, 4.30 4.15, 3.06 4.29, 4.53 2.11))",
            "POLYGON ((4.71 3.73, 7.62 2.48, 9.43 1.09, 9.33 4.99, 6.04 9.35, 4.60 4.85, 4.71 3.73))",
            "POLYGON ((1.65 1.09, 5.24 0.64, 7.02 0.62, 7.88 1.70, 3.17 7.55, 2.78 6.20, 1.65 1.09))",
            "POLYGON ((1.81 3.73, 2.99 0.28, 3.82 4.77, 2.57 8.80, 1.69 7.71, 1.92 5.27, 1.81 3.73))"
          )
        )
      ) 
    ),
    conn = duckspatial::ddbs_create_conn(dbdir = "memory"),
    wkt = "wkt",
    geom_col = "geometry",
    remove = TRUE
  )
}