# A spy on the SQL that reaches the driver.
#
# `local_query_log()` traces `DBI::dbSendQuery()` for the rest of the calling
# test and returns an environment whose `$statements` accumulates every
# statement sent, in order. `DBI::dbGetQuery()` is implemented on top of
# `dbSendQuery()`, so the dbplyr column probe
# (`SELECT * FROM <table> AS q01 WHERE (0 = 1)`) shows up here too.
#
# `probe_count(log)` counts those column probes.

local_query_log <- function(env = parent.frame()) {
    log <- new.env(parent = emptyenv())
    log$statements <- character()
    suppressMessages(invisible(utils::capture.output(
        trace(
            "dbSendQuery",
            exit = bquote(assign(
                "statements",
                c(get("statements", .(log)), as.character(statement)),
                .(log)
            )),
            print = FALSE,
            where = asNamespace("DBI")
        )
    )))
    withr::defer(
        suppressMessages(invisible(utils::capture.output(
            untrace("dbSendQuery", where = asNamespace("DBI"))
        ))),
        envir = env
    )
    log
}

probe_count <- function(log) {
    sum(grepl("WHERE \\(0 = 1\\)", log$statements))
}
