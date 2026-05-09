#' create_zarr_group
#'
#' create zarr groups
#' 
#' @param store the location of (zarr) store
#' @param name name of the group
#' @param version zarr version
#' @export
create_zarr_group <- function(store, name, version = 2){
  split.name <- strsplit(name, split = "\\/")[[1]]
  if(length(split.name) > 1){
    split.name <- vapply(seq_along(split.name),
                         function(x) paste(split.name[seq_len(x)], collapse = "/"),
                         FUN.VALUE = character(1))
    split.name <- rev(tail(split.name,2))
    if(!dir.exists(file.path(store,split.name[2])))
      create_zarr_group(store = store, name = split.name[2], version = version)
  }
  dir.create(file.path(store, split.name[1]), showWarnings = FALSE)
  switch(as.character(version),
         "2" = {
           write("{\"zarr_format\":2}", file = file.path(store, split.name[1], ".zgroup"))},
         "3" = {
           write(
             "{\"zarr_format\":3,\"node_type\":\"group\",\"attributes\":{}}",
             file = file.path(store, split.name[1], "zarr.json"))
         },
         stop("version must be '2' or '3'")
  )

}

#' create_zarr
#'
#' Create Zarr store
#'
#' @param store The location of the Zarr store
#' @param version Zarr version
#'
#' @return `NULL`
#'
#' @examples
#' store <- tempfile(fileext = ".zarr")
#' create_zarr(store = store)
#' dir.exists(store)
#'
#' @export
create_zarr <- function(store, version = 2) {
  prefix <- basename(store)
  dir <- gsub(paste0(prefix, "$"), "", store)
  create_zarr_group(store = dir, name = prefix, version = version)
}

.replace_zarr <- function(zarr.path, replace, version = 2)
{
  if (dir.exists(zarr.path) && !replace)
    stop("zarr store with name ", zarr.path ," doesnt exist")
  if (!replace)
    stop("Directory \"", zarr.path, "\" already exists. ",
         "Use 'replace=TRUE' to replace it. ",
         "Its content will be lost!")
  if (unlink(zarr.path, recursive=TRUE) != 0L)
    stop("failed to delete directory \"", dir, "\"")
  create_zarr(zarr.path, version = version)
  return(zarr.path)
}

.make_zarr_group <- function(x, name, path, replace, version){
  
  # create element parent dir
  if(!dir.exists(path))
    dir.create(path)
  
  # check element dir
  ng <- file.path(path, name)
  if(replace){
    unlink(ng, recursive = TRUE)
  } else {
    nms <- list.dirs(file.path(path), full.names = FALSE)
    if(name %in% nms)
      stop("Directory \"", ng, "\" already exists. ",
           "Use 'replace=TRUE' to replace it. ",
           "Its content will be lost!")
  }
  
  # create group
  create_zarr_group(path, name, version)
  
  return(ng)
}


# For zarr v3, OME-NGFF content (multiscales, omero, image-label) must be
# nested under an "ome" key inside "attributes"; spatialdata_attrs stays at top.
# If the metadata was read from a v3 store it already has "ome", so skip wrapping.
.wrap_ome_for_v3 <- function(zattrs, version) {
  if (version != "v3" || "ome" %in% names(zattrs)) return(as.list(zattrs))
  ome_keys <- setdiff(names(zattrs), "spatialdata_attrs")
  ome_content <- as.list(zattrs)[ome_keys]
  # Strip v2-only fields from each multiscales entry
  if (!is.null(ome_content$multiscales)) {
    ome_content$multiscales <- lapply(ome_content$multiscales, function(ms) {
      ms[setdiff(names(ms), c("version", "metadata"))]
    })
  }
  list(
    ome = c(list(version = "0.5-dev-spatialdata"), ome_content),
    spatialdata_attrs = zattrs[["spatialdata_attrs"]]
  )
}

# .get_multiscale_axes <- function(zattrs) {
#   multiscales <- zattrs[["multiscales"]]
#   if (is.null(multiscales) && !is.null(zattrs[["ome"]]))
#     multiscales <- zattrs[["ome"]][["multiscales"]]
#   if (is.null(multiscales) || length(multiscales) == 0L) return(NULL)
#   axes <- multiscales[[1]][["axes"]]
#   if (is.null(axes) || length(axes) == 0L) return(NULL)
#   vapply(axes, `[[`, character(1), "name")
# }

# # Post-processes Rarr-written v3 array zarr.json:
# #   1. Sorts codecs to required order [array-array → array-bytes → bytes-bytes].
# #      Rarr currently serialises them as [transpose, zstd, bytes] which Python rejects.
# #   2. Adds "attributes": {} and "storage_transformers": [] which Python zarr expects
# #      but Rarr does not emit.
# # dimension_names are handled upstream by setting names(dimnames()) before write_zarr_array.
# .normalize_v3_array_metadata <- function(zarr_array_path) {
#   metadata_path <- file.path(zarr_array_path, "zarr.json")
#   if (!file.exists(metadata_path)) return(invisible(FALSE))
# 
#   metadata <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
#   codecs <- metadata[["codecs"]]
#   if (!is.null(codecs) && length(codecs) > 1L) {
#     codec_names <- vapply(codecs, `[[`, character(1), "name")
#     codec_stage <- ifelse(
#       codec_names %in% "transpose", 1L,
#       ifelse(codec_names %in% c("bytes", "vlen-utf8", "vlen_utf8"), 2L, 3L)
#     )
#     metadata[["codecs"]] <- codecs[order(codec_stage)]
#   }
# 
#   if (is.null(metadata[["attributes"]])) metadata[["attributes"]] <- list()
#   if (is.null(metadata[["storage_transformers"]])) metadata[["storage_transformers"]] <- list()
# 
#   jsonlite::write_json(
#     metadata,
#     path = metadata_path,
#     auto_unbox = TRUE,
#     pretty = 4,
#     null = "null"
#   )
#   invisible(TRUE)
# }
