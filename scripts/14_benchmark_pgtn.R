## 14_benchmark_pgtn.R
## High-dimensional (p > n) parity check: for n = 200, p = 500, verify
## that fbrglm reproduces raw glmnet in the regime the reviewer asked
## about (Reviewer 1, Comment 7).
##
## Two rank-policy paths are measured:
##   - rank_policy = "keep": the full design (all p = 500 columns) is
##     passed to glmnet; fbrglm's prediction should match raw glmnet
##     on the full X to within 1e-10 absolute (penalty disambiguates).
##   - rank_policy = "drop": the pre-fit QR pivot drops linearly
##     dependent columns before the glmnet call; predictions can
##     differ from the "keep" / raw-glmnet path because the kept
##     column set differs. We record the empirical difference.
##
## Writes:
##   results/raw/pgtn_raw.csv               — per-iteration timings
##   results/summary/pgtn.csv               — median runtime + parity

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

n_rows <- 200L
p_cols <- 500L
s_true <- 5L
lam    <- 0.05
n_iter <- 5L

methods_order <- c("fbrglm_keep", "fbrglm_drop", "glmnet_raw")

df <- make_pgtn_binomial_data(
    n = n_rows, p = p_cols, s = s_true, beta_scale = 0.5, seed = 1400L
)
pred_vars <- setdiff(names(df), "y")
fml <- stats::as.formula(
    paste("y ~", paste(pred_vars, collapse = " + ")))

X_mat <- stats::model.matrix(fml, data = df)[, -1, drop = FALSE]
y_vec <- df$y

message(sprintf(
    "[14] p > n scenario: n = %d, p = %d, s_true = %d, lambda = %g",
    n_rows, p_cols, s_true, lam))

## --- parity + rank-policy accounting (outside the timing loop) -------
fit_keep <- fbrglm(fml, data = df, family = "binomial",
                   lambda = "fix", lambda_value = lam,
                   rank_policy = "keep")
pred_keep <- as.numeric(predict(fit_keep, type = "response"))

fit_drop <- fbrglm(fml, data = df, family = "binomial",
                   lambda = "fix", lambda_value = lam,
                   rank_policy = "drop")
pred_drop <- as.numeric(predict(fit_drop, type = "response"))

fit_glm <- glmnet::glmnet(X_mat, y_vec, family = "binomial",
                          alpha = 1, lambda = lam)
pred_glm <- as.numeric(predict(fit_glm, newx = X_mat, s = lam,
                               type = "response"))

keep_vs_glmnet <- max(abs(pred_keep - pred_glm))
drop_vs_glmnet <- max(abs(pred_drop - pred_glm))

rank_info <- fit_drop$rank_info
n_dropped <- if (!is.null(rank_info) && !is.null(rank_info$dropped_cols))
    length(rank_info$dropped_cols) else NA_integer_

message(sprintf(
    "       keep   vs raw glmnet: max |diff| = %.3g", keep_vs_glmnet))
message(sprintf(
    "       drop   vs raw glmnet: max |diff| = %.3g  (dropped cols = %s)",
    drop_vs_glmnet, format(n_dropped)))

parity_lookup <- c(
    fbrglm_keep = keep_vs_glmnet,
    fbrglm_drop = drop_vs_glmnet,
    glmnet_raw  = 0
)

## --- runtime: 5 iterations per method ---
thunks <- list(
    fbrglm_keep = function() {
        fit <- fbrglm(fml, data = df, family = "binomial",
                      lambda = "fix", lambda_value = lam,
                      rank_policy = "keep")
        predict(fit, type = "response")
    },
    fbrglm_drop = function() {
        fit <- fbrglm(fml, data = df, family = "binomial",
                      lambda = "fix", lambda_value = lam,
                      rank_policy = "drop")
        predict(fit, type = "response")
    },
    glmnet_raw = function() {
        fit <- glmnet::glmnet(X_mat, y_vec, family = "binomial",
                              alpha = 1, lambda = lam)
        as.vector(predict(fit, newx = X_mat, s = lam,
                          type = "response"))
    }
)

raw_rows <- list()
for (m in methods_order) {
    r <- time_iters(thunks[[m]], n_iter)
    for (i in seq_len(n_iter)) {
        this_t  <- r$times[i]
        this_ok <- r$success && !is.na(this_t)
        raw_rows[[length(raw_rows) + 1]] <- data.frame(
            scenario              = "pgtn_n200_p500",
            n                     = n_rows,
            p                     = p_cols,
            s_true                = s_true,
            method                = m,
            iteration             = i,
            runtime_sec           = this_t,
            success               = this_ok,
            max_abs_diff_vs_glmnet = parity_lookup[[m]],
            n_cols_dropped_by_qr  = if (m == "fbrglm_drop") n_dropped else NA_integer_,
            error_message         = if (!r$success) r$error else NA_character_,
            stringsAsFactors = FALSE
        )
    }
}

raw_df <- do.call(rbind, raw_rows)
rownames(raw_df) <- NULL

raw_path <- file.path(here_root, "results", "raw", "pgtn_raw.csv")
save_result_csv(raw_df, raw_path)

## --- summarise to medians per method --------------------------------
keys <- unique(raw_df[, c("scenario", "n", "p", "s_true", "method")])
summary_rows <- vector("list", nrow(keys))
for (i in seq_len(nrow(keys))) {
    sub <- raw_df[raw_df$method == keys$method[i], ]
    all_ok <- all(sub$success)
    summary_rows[[i]] <- data.frame(
        scenario              = keys$scenario[i],
        n                     = keys$n[i],
        p                     = keys$p[i],
        s_true                = keys$s_true[i],
        method                = keys$method[i],
        runtime_sec           = if (all_ok) stats::median(sub$runtime_sec) else NA_real_,
        max_abs_diff_vs_glmnet = sub$max_abs_diff_vs_glmnet[1L],
        n_cols_dropped_by_qr  = sub$n_cols_dropped_by_qr[1L],
        success               = all_ok,
        stringsAsFactors = FALSE
    )
}
summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL

summary_df$method <- factor(summary_df$method, levels = methods_order)
summary_df <- summary_df[order(summary_df$method), ]
summary_df$method <- as.character(summary_df$method)
rownames(summary_df) <- NULL

summary_path <- file.path(here_root, "results", "summary", "pgtn.csv")
save_result_csv(summary_df, summary_path)

print(summary_df)
message("[14] pgtn: done")
