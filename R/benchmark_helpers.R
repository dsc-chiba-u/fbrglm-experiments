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

#' Time `thunk()` `n_iter` times and return per-iteration elapsed seconds.
#'
#' Uses `bench::mark` when available for higher-resolution timings; falls
#' back to `proc.time()` per call. On thunk-level error the function
#' returns NA timings and records the error message.
#'
#' @return list(times = numeric(n_iter), success = logical(1),
#'              error = character(1))
time_iters <- function(thunk, n_iter) {
    if (requireNamespace("bench", quietly = TRUE)) {
        res <- tryCatch(
            bench::mark(thunk(), iterations = n_iter, check = FALSE,
                        filter_gc = FALSE),
            error = function(e) e
        )
        if (inherits(res, "error")) {
            return(list(times = rep(NA_real_, n_iter),
                        success = FALSE,
                        error = conditionMessage(res)))
        }
        if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
            return(list(times = rep(NA_real_, n_iter),
                        success = FALSE,
                        error = "bench::mark returned no rows"))
        }
        times <- as.numeric(res$time[[1]])
        if (length(times) < n_iter) {
            times <- c(times, rep(NA_real_, n_iter - length(times)))
        }
        return(list(times = times, success = TRUE, error = NA_character_))
    }
    times <- numeric(n_iter)
    success <- TRUE
    err <- NA_character_
    for (i in seq_len(n_iter)) {
        r <- safe_run(thunk())
        times[i] <- r$elapsed
        if (!r$success) { success <- FALSE; err <- r$error; break }
    }
    list(times = times, success = success, error = err)
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
