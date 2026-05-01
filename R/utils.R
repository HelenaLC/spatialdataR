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

.sync_tables <- \(x, old, new) {
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
    tables(x) <- ts
    return(x)
}

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
    tables(x) <- ts
    return(x)
}
