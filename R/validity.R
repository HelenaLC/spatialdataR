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
    axs <- axes(object)
    typ <- vapply(axs, \(.) .$type, character(1))
    d <- sum(typ != "time")
    for (k in seq_along(object)) {
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
    axs <- axes(object)
    typ <- vapply(axs, \(.) .$type, character(1))
    d <- sum(typ == "space")
    for (k in seq_along(object)) {
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
    f <- \() pull(count(spatialdataR::data(object)), "n")
    n <- tryCatch(error=\(.) 0, as.integer(f()))
    if (!n) return(NULL)
    if (!"geometry" %in% names(object)) 
        return("'SpatialDataPoint' missing 'geometry'.")
    return(NULL)
}
#' @importFrom S4Vectors setValidity2
setValidity2("SpatialDataPoint", .validatePoint)

.validateShape <- \(object) {
    if (!"geometry" %in% names(object)) 
        return("'SpatialDataShape' missing 'geometry'.")
    return(NULL)
}
#' @importFrom S4Vectors setValidity2
setValidity2("SpatialDataShape", .validateShape)

.nm <- \(x, l) {
    msg <- c()
    lys <- get(l)(x)
    nms <- names(lys)
    typ <- class(lys)[[1]]
    if (is.null(nms)) return(paste(typ, "missing names"))
    na <- nchar(nms) == 0
    if (any(na)) {
        na <- paste(which(na), collapse=",")
        return(paste(typ, "elements", na, "missing names"))
    }
    return(NULL)
}

#' @importFrom methods is
.validateSpatialData <- \(x) {
    msg <- c()
    for (l in .LAYERS) msg <- c(msg, .nm(x, l))
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

.ms <- \(x) x$multiscales[[1]] %||% x$ome$multiscales[[1]]

.validateAttrs_multiscales <- \(x, msg) {
    if (is.null(ms <- .ms(x))) {
        c(msg, "missing 'multiscales'")
        return(msg)
    }
    na <- setdiff(c("axes", "datasets"), names(ms))
    msg <- c(msg, sprintf("missing 'multiscales$%s'", na))
    return(msg)
}

# https://ngff.openmicroscopy.org/0.5/#axes-md
.validateAttrs_axes <- \(x, msg) {
    msg <- c()
    if (!is.list(ax <- x$axes))
        msg <- c(msg, "missing or invalid 'multiscales$axes'; should be a list")
    nm <- lapply(ax, names)
    ns <- lengths(nm)
    if (!all(ns == ns[1])) 
        msg <- c(msg, "'multiscales$axes' list elements of unequal length")

    # MUST contain 'name'
    # - character string
    # - unique across axiis
    nms <- lapply(ax, \(.) .$name)
    for (. in seq_along(ax)) {
        nm <- ax[[.]]$name
        ok <- length(nm) == 1 && is.character(nm) && nchar(nm) > 0
        if (!ok) {
            msg <- c(msg, paste0(
                "missing or invalid multiscales$axes[[", ., "]]$name; ",
                "should be a character string"))
            nms <- nms[-.]
        }
    }
    if (any(duplicated(unlist(nms)))) 
        msg <- c(msg, paste0(
            "found duplicated multiscales$axes[[", ., "]]$name; ",
            "should be unique across axiis"))
    
    # MAY contain 'type'
    ok <- c("space", "time", "channel")
    for (. in seq_along(ax)) {
        typ <- ax[[.]]$type
        if (is.null(typ)) next
        bad <- !isTRUE(typ %in% ok)
        if (bad) msg <- c(msg, paste0(
            "invalid multiscales$axes[[", ., "]]$type; ",
            "should be one of: ", paste(ok, collapse=", ")))
    }
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
    x <- label(sd)
    msg <- c()
    za <- meta(x)
    msg <- .validateAttrs_multiscales(za, msg)
    if (is.null(ms <- .ms(za))) return(msg)
    msg <- .validateAttrs_axes(ms, msg)
    msg <- .validateAttrs_coordTrans(ms, msg)
    return(msg)
}
