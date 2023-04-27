readShapes = function(path, ...) {
  parts <- list.dirs(path, recursive=FALSE)
  coords <- if ("coords" %in% basename(parts)) {
    Rarr::read_zarr_array(file.path(path, "coords"))
  } else {
    NULL
  }
  index <- if ("Index" %in% basename(parts)) {
    Rarr::read_zarr_array(file.path(path, "index"))
  } else {
    NULL
  }

  radius <- if ("radius" %in% basename(parts)) {
    Rarr::read_zarr_array(file.path(path, "radius"))
  } else {
    NULL
  }
  offset0 <- if ("offset0" %in% basename(parts)) {
    Rarr::read_zarr_array(file.path(path, "offset0"))
  } else {
    NULL
  }
  offset1 <- if ("offset1" %in% basename(parts)) {
    Rarr::read_zarr_array(file.path(path, "offset1"))
  } else {
    NULL
  }

  if (!is.null(radius)) {
    shape_type <- "circle"
  } else {
    shape_type <- "polygon"
  }

  if (shape_type == "polygon") {

    matrices <- lapply(seq_along(index), function(i) NULL)
    for (i in 1:(length(offset0)-1)) {
      matrices[[i]] <- coords[(offset0[[i]]+1):offset0[[i+1]], ,drop=FALSE]
    }
    S4Vectors::DataFrame(
      data = I(matrices),
      index = index,
      type = rep(shape_type, length(index))
    )
  } else {
    S4Vectors::DataFrame(
      data = I(asplit(coords, 1)),
      index = index,
      radius = radius,
      type = rep(shape_type, length(index))
    )

  }
}
