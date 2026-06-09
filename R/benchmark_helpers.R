## Small helpers for the benchmark scripts.

#' Safely run an expression and capture success / error / elapsed time.
#'
#' @param expr a quoted expression or thunk-style expression. The function
#'   uses non-standard evaluation: pass the expression directly, not
#'   quoted.
#' @return list with elements:
#'   - success  : logical
#'   - error    : character (NA if success)
#'   - elapsed  : numeric, seconds
#'   - value    : the returned value (NULL on error)
safe_run <- function(expr) {
    expr_q <- substitute(expr)
    env <- parent.frame()
    t0 <- proc.time()[["elapsed"]]
    value <- NULL
    err <- NA_character_
    success <- TRUE
    tryCatch({
        value <- eval(expr_q, envir = env)
    }, error = function(e) {
        success <<- FALSE
        err <<- conditionMessage(e)
    })
    t1 <- proc.time()[["elapsed"]]
    list(
        success = success,
        error   = err,
        elapsed = t1 - t0,
        value   = value
    )
}

#' Thin wrapper around utils::write.csv with our preferred defaults.
#'
#' Ensures the parent directory exists and writes without row names.
save_result_csv <- function(x, path) {
    stopifnot(is.data.frame(x))
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(x, path, row.names = FALSE)
    message("[bench] wrote ", path, " (", nrow(x), " rows)")
    invisible(path)
}
