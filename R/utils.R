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
