readShapes = function(path, ...) {

  files = list.files(path = path, full.names = TRUE, recursive = TRUE)

  coords_path = NULL
  index_path = NULL
  radius_path = NULL
  offset0_path = NULL
  offset1_path = NULL

  coords_pattern = "coords/0"
  index_pattern = "Index/0"
  radius_pattern = "radius/0"
  offset0_pattern = "offset0/0"
  offset1_pattern = "offset1/0"

  if (any(grepl(coords_pattern, files))) {
    coords_path = gsub("/0/0", "", files[grepl(coords_pattern, files)])
    coords = Rarr::read_zarr_array(coords_path)
  }

  if (any(grepl(index_pattern, files))) {
    index_path = dirname(files[grepl(index_pattern, files)])
    index = Rarr::read_zarr_array(index_path)
  }

  if (any(grepl(radius_pattern, files))) {
    radius_path = dirname(files[grepl(radius_pattern, files)])
    radius = Rarr::read_zarr_array(radius_path)
  }

  if (any(grepl(offset0_pattern, files))) {
    offset0_path = dirname(files[grepl(offset0_pattern, files)])
    offset0 = Rarr::read_zarr_array(offset0_path)
  }

  if (any(grepl(offset1_pattern, files))) {
    offset1_path = dirname(files[grepl(offset1_pattern, files)])
    offset1 = Rarr::read_zarr_array(offset1_path)
  }

  if (!is.null(radius_path)) {
    shape_type = "circle"
  } else {
    shape_type = "polygon"
  }

  if (shape_type == "polygon") {

    matrices = list()
    for (i in 1:(length(offset0)-1)) {

      matrices[[i]] = coords[offset0[[i]]:offset0[[i+1]], ]

    }

    result = S4Vectors::DataFrame(
      data = I(matrices),
      index = index,
      type = rep(shape_type, length(index))
    )

  } else {

    result = S4Vectors::DataFrame(
      data = I(apply(coords, 1, function(x) list(x[1], x[2]))),
      index = index,
      radius = radius,
      type = rep(shape_type, length(index))
    )

  }

  result

}
