readPoints = function(path, ...) {

  dirs = list.files(path = path, full.names = TRUE, recursive = TRUE)
  parquet_file = grep("*.parquet", dirs, value = TRUE)

  return(arrow::open_dataset(parquet_file))

}
