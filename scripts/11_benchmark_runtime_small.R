## 11_benchmark_runtime_small.R
## Compare fit+predict wall-clock for fbrglm / glmnet raw / glmnetUtils
## on a small binomial dataset.
## Writes results/summary/runtime_small.csv.

suppressPackageStartupMessages({
    library(fbrglm)
    library(glmnet)
    library(glmnetUtils)
})

here_root <- "/home/koki/dev/fbrglm-experiments"
source(file.path(here_root, "R", "data_generators.R"), local = TRUE)
source(file.path(here_root, "R", "benchmark_helpers.R"), local = TRUE)

set.seed(111)
df    <- make_basic_binomial_data(n = 200, seed = 111)
X_mat <- as.matrix(df[, paste0("x", 1:5)])
y_vec <- df$y
lam   <- 0.05

use_bench <- requireNamespace("bench", quietly = TRUE)
message("[11] use bench::mark = ", use_bench)

run_one <- function(method, thunk) {
    if (use_bench) {
        ok <- TRUE
        emsg <- NA_character_
        res <- tryCatch(
            bench::mark(thunk(), iterations = 5, check = FALSE,
                        filter_gc = FALSE),
            error = function(e) { ok <<- FALSE; emsg <<- conditionMessage(e); NULL }
        )
        if (ok) {
            t_med <- as.numeric(res$median)
        } else {
            t_med <- NA_real_
        }
        data.frame(method = method,
                   runtime_sec = t_med,
                   success     = ok,
                   error       = emsg,
                   stringsAsFactors = FALSE)
    } else {
        r <- safe_run(thunk())
        data.frame(method = method,
                   runtime_sec = r$elapsed,
                   success     = r$success,
                   error       = r$error,
                   stringsAsFactors = FALSE)
    }
}

results <- list()

## --- fbrglm ----------------------------------------------------------
results$fbrglm <- run_one("fbrglm", function() {
    fit <- fbrglm(y ~ x1 + x2 + x3 + x4 + x5, data = df,
                  family = "binomial",
                  lambda = "fix", lambda_value = lam)
    predict(fit, type = "response")
})

## --- glmnet raw ------------------------------------------------------
results$glmnet_raw <- run_one("glmnet_raw", function() {
    fit <- glmnet::glmnet(X_mat, y_vec, family = "binomial",
                          alpha = 1, lambda = lam)
    as.vector(predict(fit, newx = X_mat, s = lam, type = "response"))
})

## --- glmnetUtils -----------------------------------------------------
results$glmnetUtils <- run_one("glmnetUtils", function() {
    fit <- glmnetUtils::glmnet(y ~ x1 + x2 + x3 + x4 + x5, data = df,
                               family = "binomial",
                               alpha = 1, lambda = lam)
    as.vector(predict(fit, newdata = df, s = lam, type = "response"))
})

df_out <- do.call(rbind, results)
rownames(df_out) <- NULL

out_path <- file.path(here_root, "results", "summary", "runtime_small.csv")
save_result_csv(df_out, out_path)

print(df_out)
message("[11] runtime_small: done")
