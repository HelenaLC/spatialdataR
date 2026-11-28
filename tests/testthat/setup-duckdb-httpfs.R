httpfs_available <- tryCatch({
    conn <- duckdb::dbConnect(duckdb::duckdb())
    on.exit(duckdb::dbDisconnect(conn))
    duckspatial::ddbs_install(conn, extension = "httpfs")
}, error = function(e) FALSE)
