# get/make DuckDB connection
#' @importFrom DBI dbIsValid
#' @importFrom duckspatial ddbs_create_conn
.conn <- \() {
    nm <- ".SpatialData_DuckDB_conn"
    if (!exists(nm, envir=.GlobalEnv) ||
            !dbIsValid(.GlobalEnv[[nm]])) {
        .GlobalEnv[[nm]] <- ddbs_create_conn()
    }
    .GlobalEnv[[nm]]
}

# internal helper to resolve name/index to integer index
.resolve_id <- \(i, ok, nm=deparse1(substitute(i))) {
    nm <- sprintf("'%s'", nm)
    if (is.character(i)) {
        i <- match.arg(i, ok)
        return(match(i, ok))
    }
    if (is.numeric(i) && i == round(i) && length(i) == 1) {
        if (i < 1 || i > length(ok)) {
            stop(sprintf("invalid %s index: %d (max: %d)", nm, i, length(ok)))
        }
        return(as.integer(i))
    }
    stop(sprintf("invalid %s; expected character or integer index", nm))
}

# internal helper for null-coalescing
`%||%` <- \(a, b) if (is.null(a)) b else a

# internal helpers for object-wide iteration 
# across spatial elements (excluding tables)

.ls <- .LAYERS[.LAYERS != "tables"]

.lapplyLayer <- \(x, FUN, ...) {
    lapply(.ls, \(l) lapply(x[[l]], FUN, ...))
}

.lapplyElement <- \(x, FUN, ...) {
    for (l in .ls) {
        for (e in names(x[[l]])) {
            x[[l]][[e]] <- FUN(x[[l]][[e]], ...)
        }
    }
    return(x)
}

#' @importFrom methods slot<-
.sync_tables_sdattrs <- \(x, old, new) {
    if (!length(ts <- tables(x))) return(x)
    for (i in seq_along(ts)) {
        t <- ts[[i]]
        # check for overlap
        if (!any(region(t) %in% old)) next
        # update 'regions' colData
        # (automatically syncs 'region' metadata)
        rs <- regions(t)
        if (all(rs %in% old)) {
            j <- match(rs, old)
            regions(t) <- new[j]
        } else {
            # partial overlap (multi-region table)
            ok <- rs %in% old
            j <- match(rs[ok], old)
            rs[ok] <- new[j]
            regions(t) <- rs
        }
        ts[[i]] <- t
    }
    slot(x, "tables") <- ts
    return(x)
}

.sync_shapes_on_drop <- \(x, i) {
    # skip when there aren't any shapes
    if (!length(shapes(x))) return(x)
    t <- table(x, i)
    for (j in region(t)) {
        # skip non-shape elements
        if (layer(x, j) != "shapes") next
        # get element 'y' annotated by table 't'
        y <- element(x, j)
        # match instances between them
        y <- y[match(instances(t), instances(y), nomatch=0)] 
        # return matching shape instances
        shape(x, j) <- y
    }
    return(x)
}

#' @importFrom methods slot<-
.sync_tables_on_drop <- \(x) {
    if (!length(ts <- tables(x))) return(x)
    all_nms <- unlist(colnames(x)[.ls])
    drop <- logical(length(ts))
    for (i in seq_along(ts)) {
        t <- ts[[i]]
        # check which regions still exist
        regs <- region(t)
        keep <- regs %in% all_nms
        if (!any(keep)) {
            drop[i] <- TRUE
            message(sprintf("dropping table '%s' because all its annotated regions were removed", names(ts)[i]))
        } else if (!all(keep)) {
            # partial drop: filter table
            keep_regs <- regs[keep]
            t <- t[, regions(t) %in% keep_regs]
            # sync 'region' metadata
            region(t) <- keep_regs
            ts[[i]] <- t
            message(sprintf("filtering table '%s' to remaining regions: %s", names(ts)[i], paste(keep_regs, collapse=", ")))
        }
    }
    if (any(drop)) {
        ts <- ts[!drop]
    }
    slot(x, "tables") <- ts
    return(x)
}

# internal helper to resolve spatial (XY) axis indices
.get_xy_axes <- \(x) {
    nm <- axes(x, "name")
    ix <- match("x", nm)
    iy <- match("y", nm)
    # fallback: OME-NGFF usually places spatial dimensions at the end (YX)
    if (is.na(ix) || is.na(iy)) {
        n <- length(nm)
        ix <- n
        iy <- n-1
    }
    return(list(x=ix, y=iy))
}

# validation ----

# validate OME version
.val_ome_ver <- \(v) {
    ok <- length(v) == 1 && is.character(v) && (v <- gsub("-.*", "", v)) %in% sprintf("0.%d", seq_len(6))
    if (!ok) stop("invalid OME 'version'; expected '0.x' where x is 1-6")
    return(v)
}

# multiscales ----

# internal helper to get the 'active' metadata level
# (drills into 'multiscales' if present, else returns the list itself)
.get_ms <- \(x) {
    # if 'x' is an element, get its attributes first
    if (is(x, "SpatialDataElement")) x <- meta(x)
    # check for 'multiscales' (handles OME version via 'multiscales()'
    ms <- multiscales(x)
    if (is.null(ms)) return(x)
    # return the first (usually only) multiscale level's metadata;
    # this contains the 'axes' & 'coordinateTransformations' we need
    return(ms[[1]])
}

# get scale factors between 'multiscales' levels
# (returns numeric vector, one value per dimension)
.get_ms_scale <- \(x) {
    ms <- .get_ms(x)
    ds <- ms$datasets[[1]]
    ct <- ds$coordinateTransformations[[1]]
    return(unlist(ct$scale))
}

# find indices with equal spatial extents
# (returns array: rows = matches, cols = x/y indices)
.get_ms_match <- \(x, y) {
    ax <- axes(x, "type") == "space"
    ay <- axes(y, "type") == "space"
    dx <- lapply(data(x, NULL), \(d) dim(d)[ax])
    dy <- lapply(data(y, NULL), \(d) dim(d)[ay])
    ks <- outer(dx, dy, Vectorize(identical))
    ks <- which(ks, arr.ind=TRUE)
    if (nrow(ks) == 0)
        stop("couldn't find shared multiscales level; need at",
            " least one data() pair with identical dimensions")
    return(ks)
}
