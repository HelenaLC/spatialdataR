# https://spatialdata.scverse.org/en/latest/design_doc.html#table-table-of-annotations-for-regions
#' @importFrom SingleCellExperiment int_metadata int_colData
.validateTables <- \(object) {
    msg <- c()
    sce <- \(.) is(., "SingleCellExperiment")
    for (i in seq_along(tables(object))) {
        ok <- sce(se <- SpatialData::table(object, i))
        if (!ok) msg <- c(msg, paste0(
            i, "-th table is not a 'SingleCellExperiment'"))
        if (!ok) next
        md <- int_metadata(se)$spatialdata_attrs
        nm <- c("region", "region_key", "instance_key")
        .nm <- sprintf("'%s'", paste(nm, collapse="/"))
        if (any(ok <- nm %in% names(md))) {
            if (!all(ok)) msg <- c(msg, paste0(
                i, "-th table missing ", .nm, "; must set all if any"))
            ok <- all(vapply(md, is.character, logical(1)))
            if (!ok) msg <- c(msg, paste0(
                i, "-th table's ", .nm, " is not of type character"))
            ok <- all(lengths(intersect(md, nm[-1])) == 1)
            if (!ok) msg <- c(msg, paste0(
                i, "-th table's 'region/instance_key' is not length 1"))
            ok <- !is.null(int_colData(se)[[md$instance_key]])
            if (!ok) msg <- c(msg, paste0(
                i, "-th table missing 'instance_key' column in 'int_colData'"))
            ok <- !is.null(rs <- int_colData(se)[[rk <- md$region_key]])
            if (!ok) msg <- c(msg, paste0(
                i, "-th table missing 'region_key' column in 'int_colData'"))
            ok <- all(md[[rk]] %in% rs)
            if (!ok) msg <- c(msg, paste0)(
                i, "-th table's 'region_key' values not found in 'int_colData'")
        }
    }
    na <- setdiff(
        unlist(lapply(tables(object), \(.) if (sce(.)) region(.))),
        unlist(colnames(object)[setdiff(.LAYERS, "tables")])) # don't flip!
    if (length(na))
        msg <- c(msg, paste(
            "table region(s) not found in any layer:",
            paste(sprintf("'%s'", na), collapse=", ")))
    return(msg)
}

.validateImageArray <- \(object) {
    msg <- c()
    res <- length(object)
    for (k in seq_len(res)) {
        x <- data(object, k)
        if (length(dim(x)) != 3) msg <- c(msg, paste(
            "'ImageArray' resolution", k, "is not 3D"))
        if (!type(x) %in% c("double", "integer")) msg <- c(msg, paste(
            "'ImageArray' resolution", k, "is not of type double or integer"))
    }
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("ImageArray", .validateImageArray)

#' @importFrom ZarrArray type
.validateLabelArray <- \(object) {
    msg <- c()
    res <- length(object)
    for (k in seq_len(res)) {
        x <- data(object, k)
        if (length(dim(x)) != 2) msg <- c(msg, paste(
            "'LabelArray' resolution", k, "is not 2D"))
        if (type(x) != "integer") msg <- c(msg, paste(
            "'LabelArray' resolution", k, "is not of type integer"))
    }
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("LabelArray", .validateLabelArray)

.validatePointFrame <- \(object) {
    msg <- c()
    if (!length(object)) return(msg)
    # if (!"x" %in% names(object)) msg <- c(msg, "'PointFrame' missing 'x'.")
    # if (!"y" %in% names(object)) msg <- c(msg, "'PointFrame' missing 'y'.")
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("PointFrame", .validatePointFrame)

.validateShapeFrame <- \(object) {
    msg <- c()
    #if (!nrow(object)) return(msg)
    #if (!"geometry" %in% names(object)) msg <- c(msg, "'ShapeFrame' missing 'geometry'.")
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("ShapeFrame", .validateShapeFrame)

#' @importFrom methods is
.validateSpatialData <- \(x) {
    msg <- c()
    typ <- c(
        images="ImageArray",
        labels="LabelArray",
        points="PointFrame",
        shapes="ShapeFrame",
        tables="SingleCellExperiment")
    for (. in names(typ)) if (length(x[[.]]))
        if (!all(vapply(x[[.]], \(y) is(y, typ[.]), logical(1))))
            msg <- c(msg, sprintf("'%s' should be a list of '%s'", ., typ[.]))
    # TODO: validate .zattrs across all layers
    for (y in labels(x)) msg <- c(msg, .validateLabelArray(y))
    for (y in images(x)) msg <- c(msg, .validateImageArray(y))
    for (y in points(x)) msg <- c(msg, .validatePointFrame(y))
    for (y in shapes(x)) msg <- c(msg, .validateShapeFrame(y))
    msg <- c(msg, .validateTables(x))
    return(msg)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)

# TODO: version-specific .zattrs validation for all layers

.validateZattrs_multiscales <- \(x, msg) {
    if (is.null(ms <- x$multiscales[[1]]))
        msg <- c(msg, "missing 'multiscales'")
    # MUST contain
    for (. in c("axes", "datasets"))
        if (is.null(ms[[.]]))
            msg <- c(msg, sprintf("missing 'multiscales$%s'", .))
    return(msg)
}
.validateZattrs_axes <- \(x, msg) {
    if (!is.list(ax <- x$axes))
        msg <- c(msg, "missing or non-list 'axes'")
    ax <- ax[[1]]
    if (is.null(ax$name))
        msg <- c(msg, "missing 'axes$name'")
    if (!is.null(ts <- ax$type))
        if (!all(ts %in% c("space", "time", "channel")))
            msg <- c(msg, "'axes$type' should be 'space/time/channel'")
    return(msg)
}
.validateZattrs_coordTrans <- \(x, msg) {
    if (!is.list(ct <- x$coordinateTransformations))
        msg <- c(msg, "missing or non-list 'coordTrans'")
    for (i in seq_along(ct))
        for (j in c("input", "output", "type"))
            if (is.null(ct[[i]][[j]]))
                msg <- c(msg, sprintf("'coordTrans' %s missing '%s'", i, j))
    return(msg)
}
.validateZattrsLabelArray <- \(x) {
    msg <- c()
    za <- meta(x)
    msg <- .validateZattrs_multiscales(za, msg)
    ms <- za$multiscales[[1]]
    msg <- .validateZattrs_axes(ms, msg)
    msg <- .validateZattrs_coordTrans(ms, msg)
    return(msg)
}
