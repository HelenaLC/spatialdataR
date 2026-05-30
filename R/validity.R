# https://spatialdata.scverse.org/en/latest/design_doc.html#table-table-of-annotations-for-regions
#' @importFrom SingleCellExperiment int_metadata int_colData
.validateTables <- \(object) {
    msg <- c()
    for (i in seq_along(tables(object))) {
        se <- table(object, i)
        md <- int_metadata(se)$spatialdata_attrs
        nm <- c("region", "region_key", "instance_key")
        .nm <- sprintf("'%s'", paste(nm, collapse="/"))
        if (any(ok <- nm %in% names(md))) {
            if (!all(ok)) msg <- c(msg, paste0(
                i, "-th table missing ", .nm, "; must set all if any"))
            ok <- all(vapply(md, is.character, logical(1)))
            if (!ok) msg <- c(msg, paste0(
                i, "-th table's ", .nm, " is not of type character"))
            ks <- intersect(names(md), nm[-1])
            ok <- all(lengths(md[ks]) == 1)
            if (!ok) {
                msg <- c(msg, paste0(i, "-th table's 'region/instance_key' is not length 1"))
            } else {
                ok <- length(int_colData(se)[[md$instance_key]])
                if (!ok) msg <- c(msg, paste0(
                    i, "-th table missing 'instance_key' column in 'int_colData'"))
                ok <- length(rs <- int_colData(se)[[rk <- md$region_key]])
                if (!ok) {
                    msg <- c(msg, paste0(i, "-th table missing 'region_key' column in 'int_colData'"))
                } else {
                    ok <- all(md$region %in% rs)
                    if (!ok) msg <- c(msg, paste0(
                        i, "-th table's 'region_key' values not found in 'int_colData'"))
                }
            }
        }
    }
    na <- setdiff(
        unlist(lapply(tables(object), region)),
        unlist(colnames(object)[setdiff(.LAYERS, "tables")])) # don't flip!
    if (length(na))
        msg <- c(msg, paste(
            "table region(s) not found in any layer:",
            paste(sprintf("'%s'", na), collapse=", ")))
    return(msg)
}

.validateImage <- \(object) {
    msg <- c()
    res <- length(object)
    axs <- axes(object)
    typ <- vapply(axs, \(.) .$type, character(1))
    d <- sum(typ != "time")
    for (k in seq_len(res)) {
        x <- data(object, k)
        if (length(dim(x)) != d) msg <- c(msg, paste(
            "'SpatialDataImage' resolution", k, "is not ", d, "D"))
        if (!type(x) %in% c("double", "integer")) msg <- c(msg, paste(
            "'SpatialDataImage' resolution", k, "is not of type double or integer"))
    }
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("SpatialDataImage", .validateImage)

#' @importFrom ZarrArray type
.validateLabel <- \(object) {
    msg <- c()
    res <- length(object)
    axs <- axes(object)
    typ <- vapply(axs, \(.) .$type, character(1))
    d <- sum(typ == "space")
    for (k in seq_len(res)) {
        x <- data(object, k)
        if (length(dim(x)) != d) msg <- c(msg, paste(
            "'SpatialDataLabel' resolution", k, "is not ", d, "D"))
        if (type(x) != "integer") msg <- c(msg, paste(
            "'SpatialDataLabel' resolution", k, "is not of type integer"))
    }
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("SpatialDataLabel", .validateLabel)

#' @importFrom dplyr count pull
.validatePoint <- \(object) {
    msg <- c()
    cnt <- tryCatch(error=\(.) 0, as.integer(
        pull(count(spatialdataR::data(object)), "n")))
    if (!cnt) return(msg)
    if (!"geometry" %in% names(object))
        msg <- c(msg, "'SpatialDataPoint' missing 'geometry'.")
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("SpatialDataPoint", .validatePoint)

.validateShape <- \(object) {
    msg <- c()
    if (!"geometry" %in% names(object))
        msg <- c(msg, "'SpatialDataShape' missing 'geometry'.")
    return(msg)
}
#' @importFrom S4Vectors setValidity2
setValidity2("SpatialDataShape", .validateShape)

#' @importFrom methods is
.validateSpatialData <- \(x) {
    msg <- c()
    typ <- c(
        images="SpatialDataImage",
        labels="SpatialDataLabel",
        points="SpatialDataPoint",
        shapes="SpatialDataShape",
        tables="SingleCellExperiment")
    for (. in names(typ)) if (length(x[[.]]))
        if (!all(vapply(x[[.]], \(y) is(y, typ[.]), logical(1))))
            msg <- c(msg, sprintf("'%s' should be a list of '%s'", ., typ[.]))
    # TODO: validate .zattrs across all layers
    for (y in as.list(labels(x))) msg <- c(msg, .validateLabel(y))
    for (y in as.list(images(x))) msg <- c(msg, .validateImage(y))
    for (y in as.list(points(x))) msg <- c(msg, .validatePoint(y))
    for (y in as.list(shapes(x))) msg <- c(msg, .validateShape(y))
    msg <- c(msg, .validateTables(x))
    return(msg)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)

# TODO: version-specific .zattrs validation for all layers

.validateAttrs_multiscales <- \(x, msg) {
    if (is.null(ms <- x$multiscales[[1]]))
        msg <- c(msg, "missing 'multiscales'")
    else {
        # MUST contain
        for (. in c("axes", "datasets"))
            if (is.null(ms[[.]]))
                msg <- c(msg, sprintf("missing 'multiscales$%s'", .))
    }
    return(msg)
}
.validateAttrs_axes <- \(x, msg) {
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
.validateAttrs_coordTrans <- \(x, msg) {
    if (!is.list(ct <- x$coordinateTransformations))
        msg <- c(msg, "missing or non-list 'coordTrans'")
    for (i in seq_along(ct))
        for (j in c("input", "output", "type"))
            if (is.null(ct[[i]][[j]]))
                msg <- c(msg, sprintf("'coordTrans' %s missing '%s'", i, j))
    return(msg)
}
.validateAttrsLabel <- \(x) {
    msg <- c()
    za <- meta(x)
    msg <- .validateAttrs_multiscales(za, msg)
    ms <- za$multiscales[[1]]
    msg <- .validateAttrs_axes(ms, msg)
    msg <- .validateAttrs_coordTrans(ms, msg)
    return(msg)
}
