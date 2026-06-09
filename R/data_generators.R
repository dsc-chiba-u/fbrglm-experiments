## Data generators for fbrglm comparison benchmarks.
## All generators return either a single data.frame or a named list of
## data.frames; they never modify global state and always honour `seed`.

#' Generate a small binomial dataset with numeric predictors.
#'
#' @param n integer, number of rows
#' @param seed integer
#' @return data.frame with columns y, x1, ..., x5
make_basic_binomial_data <- function(n = 200, seed = 1) {
    stopifnot(n >= 20)
    old <- .Random.seed_safe(seed)
    on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * 5), n, 5)
    colnames(X) <- paste0("x", 1:5)
    eta <- 0.6 * X[, 1] - 0.4 * X[, 2] + 0.2 * X[, 3]
    y <- stats::rbinom(n, 1, stats::plogis(eta))
    data.frame(y = y, X)
}

#' Generate train/test with a deliberate factor-level mismatch.
#'
#' Train has factor `g` with levels A/B/C/D and observations across all
#' four. Test's `g` is constructed with **levels = A/B only** (not just
#' values — the factor itself has a narrower level set), so that calling
#' `model.matrix(~ ... + g, data = test)` produces fewer columns than the
#' corresponding call on `train`. This is the canonical glmnet failure
#' mode: a naive caller who builds the test design matrix separately gets
#' a width mismatch at predict() time.
#'
#' @return list with $train and $test data.frames
make_factor_mismatch_data <- function(n_train = 200,
                                      n_test = 50,
                                      seed = 1) {
    stopifnot(n_train >= 40, n_test >= 10)
    old <- .Random.seed_safe(seed)
    on.exit(.Random.seed_restore(old))

    train_levels <- c("A", "B", "C", "D")
    test_levels  <- c("A", "B")

    ## Force every train level to appear at least once so the mismatch
    ## is observable regardless of seed.
    g_train_raw <- c(train_levels,
                     sample(train_levels, n_train - length(train_levels),
                            replace = TRUE))
    g_train_raw <- sample(g_train_raw)

    train <- data.frame(
        y  = stats::rbinom(n_train, 1, 0.5),
        x1 = stats::rnorm(n_train),
        x2 = stats::rnorm(n_train),
        g  = factor(g_train_raw, levels = train_levels)
    )
    test <- data.frame(
        x1 = stats::rnorm(n_test),
        x2 = stats::rnorm(n_test),
        ## Narrow levels on purpose: the factor only knows about A/B.
        g  = factor(sample(test_levels, n_test, replace = TRUE),
                    levels = test_levels)
    )
    list(train = train, test = test)
}

#' Generate gaussian data with several wide-ish factors.
#'
#' @param n integer, rows
#' @param n_factor number of factor predictors
#' @param n_levels levels per factor
#' @param seed integer
make_sparse_factor_data <- function(n = 500,
                                    n_factor = 5,
                                    n_levels = 10,
                                    seed = 1) {
    stopifnot(n >= 50, n_factor >= 1, n_levels >= 2)
    old <- .Random.seed_safe(seed)
    on.exit(.Random.seed_restore(old))

    lvl_pool <- LETTERS
    if (n_levels > length(lvl_pool)) {
        lvl_pool <- as.character(seq_len(n_levels))
    }

    f_cols <- lapply(seq_len(n_factor), function(j) {
        lvls <- lvl_pool[seq_len(n_levels)]
        factor(sample(lvls, n, replace = TRUE), levels = lvls)
    })
    names(f_cols) <- paste0("g", seq_len(n_factor))

    num <- data.frame(
        x1 = stats::rnorm(n),
        x2 = stats::rnorm(n)
    )
    df <- cbind(num, as.data.frame(f_cols))

    eta <- 0.5 * df$x1 - 0.3 * df$x2
    df$y <- stats::rnorm(n, mean = eta)
    df[, c("y", setdiff(names(df), "y"))]
}

## --- internal helpers ------------------------------------------------

## Save/restore .Random.seed so generators are reproducible without
## clobbering the caller's seed state.
.Random.seed_safe <- function(seed) {
    old <- if (exists(".Random.seed", envir = globalenv())) {
        get(".Random.seed", envir = globalenv())
    } else {
        NULL
    }
    set.seed(seed)
    old
}

.Random.seed_restore <- function(old) {
    if (is.null(old)) {
        if (exists(".Random.seed", envir = globalenv())) {
            rm(".Random.seed", envir = globalenv())
        }
    } else {
        assign(".Random.seed", old, envir = globalenv())
    }
    invisible(NULL)
}
