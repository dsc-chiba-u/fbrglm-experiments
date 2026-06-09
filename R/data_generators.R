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

#' Generate a binomial dataset for runtime benchmarking.
#'
#' Numeric predictors `x1..x{p_numeric}` plus optionally `n_factor` factor
#' columns `g1..g{n_factor}`, each with `n_levels` levels drawn uniformly.
#' The linear predictor uses `x1` and `x2` only, regardless of `p_numeric`;
#' the rest are noise. The response column `y` is moved to the front.
#'
#' @return data.frame with `y` first, then `x*`, then `g*`.
make_runtime_binomial_data <- function(n,
                                       p_numeric,
                                       n_factor = 0L,
                                       n_levels = 0L,
                                       seed = 1) {
    stopifnot(n >= 20, p_numeric >= 2)
    old <- .Random.seed_safe(seed)
    on.exit(.Random.seed_restore(old))

    X_num <- matrix(stats::rnorm(n * p_numeric), n, p_numeric)
    colnames(X_num) <- paste0("x", seq_len(p_numeric))
    df <- as.data.frame(X_num)

    if (n_factor > 0L) {
        stopifnot(n_levels >= 2L)
        lvl_pool <- if (n_levels <= length(LETTERS)) {
            LETTERS[seq_len(n_levels)]
        } else {
            sprintf("L%02d", seq_len(n_levels))
        }
        for (j in seq_len(n_factor)) {
            df[[paste0("g", j)]] <- factor(
                sample(lvl_pool, n, replace = TRUE),
                levels = lvl_pool
            )
        }
    }

    eta <- 0.4 * df$x1 - 0.3 * df$x2
    df$y <- stats::rbinom(n, 1, stats::plogis(eta))
    df[, c("y", setdiff(names(df), "y"))]
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

## --- per-family generators for the family benchmark (scripts/12) ----
##
## All of these return data frames (Cox returns one, too, with separate
## time / status columns rather than a Surv object so that we can pass
## either form to the benchmark methods). The seeded helpers below give
## every run the same dataset for a given family/seed pair.

make_family_gaussian_data <- function(n = 200, p = 4, seed = 1) {
    old <- .Random.seed_safe(seed); on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("x", seq_len(p))
    eta <- 0.4 * X[, 1] - 0.3 * X[, 2]
    df <- as.data.frame(X); df$y <- stats::rnorm(n, eta, 0.5)
    df[, c("y", setdiff(names(df), "y"))]
}

make_family_binomial_data <- function(n = 200, p = 4, seed = 1) {
    old <- .Random.seed_safe(seed); on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("x", seq_len(p))
    eta <- 0.4 * X[, 1] - 0.3 * X[, 2]
    df <- as.data.frame(X)
    df$y <- stats::rbinom(n, 1, stats::plogis(eta))
    df[, c("y", setdiff(names(df), "y"))]
}

make_family_poisson_data <- function(n = 200, p = 4, seed = 1) {
    old <- .Random.seed_safe(seed); on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("x", seq_len(p))
    eta <- 0.4 * X[, 1] - 0.3 * X[, 2]
    df <- as.data.frame(X); df$y <- stats::rpois(n, exp(eta))
    df[, c("y", setdiff(names(df), "y"))]
}

make_family_gamma_data <- function(n = 200, p = 4, seed = 1) {
    old <- .Random.seed_safe(seed); on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("x", seq_len(p))
    eta <- 0.4 * X[, 1] - 0.3 * X[, 2]
    df <- as.data.frame(X)
    df$y <- stats::rgamma(n, shape = 2, rate = exp(-eta))
    df[, c("y", setdiff(names(df), "y"))]
}

make_family_negbin_data <- function(n = 200, p = 4, theta = 2, seed = 1) {
    old <- .Random.seed_safe(seed); on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("x", seq_len(p))
    eta <- 0.4 * X[, 1] - 0.3 * X[, 2]
    df <- as.data.frame(X)
    df$y <- stats::rnbinom(n, mu = exp(eta), size = theta)
    df[, c("y", setdiff(names(df), "y"))]
}

make_family_cox_data <- function(n = 200, p = 4, seed = 1) {
    old <- .Random.seed_safe(seed); on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("x", seq_len(p))
    eta <- 0.4 * X[, 1] - 0.3 * X[, 2]
    t_event <- stats::rexp(n, exp(eta))
    t_obs   <- pmin(t_event, 3)
    delta   <- as.integer(t_event <= 3)
    df <- as.data.frame(X)
    df$t_obs <- t_obs; df$delta <- delta
    df[, c("t_obs", "delta", setdiff(names(df), c("t_obs", "delta")))]
}

make_family_multinomial_data <- function(n = 200, p = 4, seed = 1) {
    old <- .Random.seed_safe(seed); on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("x", seq_len(p))
    df <- as.data.frame(X)
    df$y <- factor(sample(c("a", "b", "c"), n, replace = TRUE),
                   levels = c("a", "b", "c"))
    df[, c("y", setdiff(names(df), "y"))]
}

make_family_mgaussian_data <- function(n = 200, p = 4, seed = 1) {
    old <- .Random.seed_safe(seed); on.exit(.Random.seed_restore(old))
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("x", seq_len(p))
    eta1 <- 0.4 * X[, 1] - 0.3 * X[, 2]
    eta2 <- 0.3 * X[, 1] + 0.2 * X[, 3]
    df <- as.data.frame(X)
    df$y1 <- stats::rnorm(n, eta1, 0.5)
    df$y2 <- stats::rnorm(n, eta2, 0.5)
    df[, c("y1", "y2", setdiff(names(df), c("y1", "y2")))]
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
