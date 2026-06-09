## 11_benchmark_runtime_small.R
## Wall-clock comparison of fbrglm / glmnet raw / glmnetUtils / parsnip+
## workflows across a small grid of (n, p_numeric, n_factor, n_levels).
##
## Writes:
##   results/raw/runtime_small_raw.csv      — per-iteration timings
##   results/summary/runtime_small.csv      — median per (scenario, method)

suppressPackageStartupMessages({
    library(fbrglm)
    library(glmnet)
    library(glmnetUtils)
    library(parsnip)
    library(workflows)
})

here_root <- "/home/koki/dev/fbrglm-experiments"
source(file.path(here_root, "R", "data_generators.R"), local = TRUE)
source(file.path(here_root, "R", "benchmark_helpers.R"), local = TRUE)

scenarios <- list(
    list(name = "n200_p5",       n = 200,  p_numeric = 5,  n_factor = 0L, n_levels = 0L),
    list(name = "n1000_p20",     n = 1000, p_numeric = 20, n_factor = 0L, n_levels = 0L),
    list(name = "n1000_p20_f10", n = 1000, p_numeric = 20, n_factor = 1L, n_levels = 10L),
    list(name = "n2000_p50_f20", n = 2000, p_numeric = 50, n_factor = 1L, n_levels = 20L)
)

methods_order <- c("fbrglm", "glmnet_raw", "glmnetUtils", "parsnip_workflow")
n_iter <- 5L
lam    <- 0.05

raw_rows <- list()

for (sc_idx in seq_along(scenarios)) {
    sc <- scenarios[[sc_idx]]
    message(sprintf(
        "[11] scenario %-15s (n=%d, p=%d, n_factor=%d, n_levels=%d)",
        sc$name, sc$n, sc$p_numeric, sc$n_factor, sc$n_levels))

    df <- make_runtime_binomial_data(
        n         = sc$n,
        p_numeric = sc$p_numeric,
        n_factor  = sc$n_factor,
        n_levels  = sc$n_levels,
        seed      = 1100L + sc_idx
    )
    pred_vars <- setdiff(names(df), "y")
    fml       <- stats::as.formula(
        paste("y ~", paste(pred_vars, collapse = " + ")))

    X_mat <- stats::model.matrix(fml, data = df)[, -1, drop = FALSE]
    y_vec <- df$y

    df_p <- df
    df_p$y <- factor(df$y)

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
        },
        glmnetUtils = function() {
            fit <- glmnetUtils::glmnet(fml, data = df, family = "binomial",
                                       alpha = 1, lambda = lam)
            as.vector(predict(fit, newdata = df, s = lam,
                              type = "response"))
        },
        parsnip_workflow = function() {
            spec <- parsnip::logistic_reg(penalty = lam, mixture = 1) |>
                parsnip::set_engine("glmnet") |>
                parsnip::set_mode("classification")
            wf <- workflows::workflow() |>
                workflows::add_formula(fml) |>
                workflows::add_model(spec)
            fit_obj <- fit(wf, data = df_p)
            predict(fit_obj, new_data = df_p, type = "prob")
        }
    )

    for (m in methods_order) {
        r <- time_iters(thunks[[m]], n_iter)
        for (i in seq_len(n_iter)) {
            this_t  <- r$times[i]
            this_ok <- r$success && !is.na(this_t)
            raw_rows[[length(raw_rows) + 1]] <- data.frame(
                scenario      = sc$name,
                n             = sc$n,
                p_numeric     = sc$p_numeric,
                n_factor      = sc$n_factor,
                n_levels      = sc$n_levels,
                method        = m,
                iteration     = i,
                runtime_sec   = this_t,
                success       = this_ok,
                error_message = if (!r$success) r$error else NA_character_,
                stringsAsFactors = FALSE
            )
        }
    }
}

raw_df <- do.call(rbind, raw_rows)
rownames(raw_df) <- NULL

raw_path <- file.path(here_root, "results", "raw",
                     "runtime_small_raw.csv")
save_result_csv(raw_df, raw_path)

## --- summarise to medians per (scenario, method) --------------------
keys <- unique(raw_df[, c("scenario", "n", "p_numeric",
                          "n_factor", "n_levels", "method")])
summary_rows <- vector("list", nrow(keys))
for (i in seq_len(nrow(keys))) {
    sub <- raw_df[
        raw_df$scenario == keys$scenario[i] &
        raw_df$method   == keys$method[i],
    ]
    all_ok <- all(sub$success)
    summary_rows[[i]] <- data.frame(
        scenario      = keys$scenario[i],
        n             = keys$n[i],
        p_numeric     = keys$p_numeric[i],
        n_factor      = keys$n_factor[i],
        n_levels      = keys$n_levels[i],
        method        = keys$method[i],
        runtime_sec   = if (all_ok) stats::median(sub$runtime_sec) else NA_real_,
        success       = all_ok,
        error_message = {
            non_na <- sub$error_message[!is.na(sub$error_message)]
            if (length(non_na)) non_na[1] else NA_character_
        },
        stringsAsFactors = FALSE
    )
}
summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL

## Reorder methods to the canonical display order.
summary_df$method <- factor(summary_df$method, levels = methods_order)
summary_df <- summary_df[order(
    match(summary_df$scenario, vapply(scenarios, `[[`, "", "name")),
    summary_df$method
), ]
summary_df$method <- as.character(summary_df$method)
rownames(summary_df) <- NULL

summary_path <- file.path(here_root, "results", "summary",
                          "runtime_small.csv")
save_result_csv(summary_df, summary_path)

print(summary_df)
message("[11] runtime_small: done")
