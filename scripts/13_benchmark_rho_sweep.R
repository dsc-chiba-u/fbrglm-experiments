## 13_benchmark_rho_sweep.R
## Correlation-strength sweep: for rho in {0, 0.5, 0.9, 0.99}, verify
## that fbrglm reproduces raw glmnet exactly on correlated designs
## (Reviewer 1, Comment 6), and record the per-method runtime.
##
## Predictors are drawn from an AR(1) multivariate normal with
## Cov(x_i, x_j) = rho^|i - j|; the linear predictor uses the first
## two columns only. For each rho, both fbrglm and raw glmnet are
## fit at a fixed lambda; the maximum absolute difference between
## their response-scale predictions is recorded.
##
## Writes:
##   results/raw/rho_sweep_raw.csv          — per-iteration timings
##   results/summary/rho_sweep.csv          — median runtime + parity
##                                             per (rho, method)

suppressPackageStartupMessages({
    library(fbrglm)
    library(glmnet)
})

here_root <- (function() {
    e <- Sys.getenv("FBRGLM_EXP_ROOT")
    if (nzchar(e))                                          return(e)
    if (file.exists("environment.yml") && dir.exists("R")) return(getwd())
    cur <- normalizePath(getwd(), mustWork = FALSE)
    for (i in seq_len(8)) {
        if (file.exists(file.path(cur, "environment.yml")) &&
            dir.exists(file.path(cur, "R"))) return(cur)
        parent <- dirname(cur); if (parent == cur) break
        cur <- parent
    }
    stop("Cannot locate the fbrglm-experiments repository root. ",
         "Run from the repo root, or set FBRGLM_EXP_ROOT.",
         call. = FALSE)
})()
source(file.path(here_root, "R", "data_generators.R"), local = TRUE)
source(file.path(here_root, "R", "benchmark_helpers.R"), local = TRUE)

rho_grid <- c(0.0, 0.5, 0.9, 0.99)
n_rows   <- 400L
p_cols   <- 10L
lam      <- 0.05
n_iter   <- 5L

methods_order <- c("fbrglm", "glmnet_raw")

raw_rows <- list()

for (k in seq_along(rho_grid)) {
    rho <- rho_grid[k]
    scenario_name <- sprintf("rho_%s", format(rho, nsmall = 2))
    message(sprintf(
        "[13] scenario %-10s (n=%d, p=%d, rho=%g)",
        scenario_name, n_rows, p_cols, rho))

    df <- make_correlated_binomial_data(
        n = n_rows, p = p_cols, rho = rho, seed = 1300L + k
    )
    pred_vars <- setdiff(names(df), "y")
    fml <- stats::as.formula(
        paste("y ~", paste(pred_vars, collapse = " + ")))

    X_mat <- stats::model.matrix(fml, data = df)[, -1, drop = FALSE]
    y_vec <- df$y

    ## --- parity: compute both predictions once, outside the timing loop ---
    fit_fbr <- fbrglm(fml, data = df, family = "binomial",
                      lambda = "fix", lambda_value = lam)
    pred_fbr <- as.numeric(predict(fit_fbr, type = "response"))

    fit_glm <- glmnet::glmnet(X_mat, y_vec, family = "binomial",
                              alpha = 1, lambda = lam)
    pred_glm <- as.numeric(predict(fit_glm, newx = X_mat, s = lam,
                                   type = "response"))

    max_abs_diff <- max(abs(pred_fbr - pred_glm))
    emp_rho12 <- stats::cor(df$x1, df$x2)

    message(sprintf(
        "       empirical cor(x1,x2) = %.4f, max|fbrglm - glmnet| = %.3g",
        emp_rho12, max_abs_diff))

    ## --- runtime: 5 iterations per method ---
    thunks <- list(
        fbrglm = function() {
            fit <- fbrglm(fml, data = df, family = "binomial",
                          lambda = "fix", lambda_value = lam)
            predict(fit, type = "response")
        },
        glmnet_raw = function() {
            fit <- glmnet::glmnet(X_mat, y_vec, family = "binomial",
                                  alpha = 1, lambda = lam)
            as.vector(predict(fit, newx = X_mat, s = lam,
                              type = "response"))
        }
    )

    for (m in methods_order) {
        r <- time_iters(thunks[[m]], n_iter)
        for (i in seq_len(n_iter)) {
            this_t  <- r$times[i]
            this_ok <- r$success && !is.na(this_t)
            raw_rows[[length(raw_rows) + 1]] <- data.frame(
                scenario         = scenario_name,
                rho              = rho,
                n                = n_rows,
                p                = p_cols,
                method           = m,
                iteration        = i,
                runtime_sec      = this_t,
                success          = this_ok,
                max_abs_diff     = if (m == "fbrglm") max_abs_diff else 0,
                empirical_cor_x1x2 = emp_rho12,
                error_message    = if (!r$success) r$error else NA_character_,
                stringsAsFactors = FALSE
            )
        }
    }
}

raw_df <- do.call(rbind, raw_rows)
rownames(raw_df) <- NULL

raw_path <- file.path(here_root, "results", "raw", "rho_sweep_raw.csv")
save_result_csv(raw_df, raw_path)

## --- summarise to medians per (rho, method) --------------------------
keys <- unique(raw_df[, c("scenario", "rho", "n", "p", "method")])
summary_rows <- vector("list", nrow(keys))
for (i in seq_len(nrow(keys))) {
    sub <- raw_df[
        raw_df$scenario == keys$scenario[i] &
        raw_df$method   == keys$method[i],
    ]
    all_ok <- all(sub$success)
    summary_rows[[i]] <- data.frame(
        scenario         = keys$scenario[i],
        rho              = keys$rho[i],
        n                = keys$n[i],
        p                = keys$p[i],
        method           = keys$method[i],
        runtime_sec      = if (all_ok) stats::median(sub$runtime_sec) else NA_real_,
        max_abs_diff     = sub$max_abs_diff[1L],
        empirical_cor_x1x2 = sub$empirical_cor_x1x2[1L],
        success          = all_ok,
        stringsAsFactors = FALSE
    )
}
summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL

summary_df$method <- factor(summary_df$method, levels = methods_order)
summary_df <- summary_df[order(summary_df$rho, summary_df$method), ]
summary_df$method <- as.character(summary_df$method)
rownames(summary_df) <- NULL

summary_path <- file.path(here_root, "results", "summary", "rho_sweep.csv")
save_result_csv(summary_df, summary_path)

print(summary_df)
message("[13] rho_sweep: done")
