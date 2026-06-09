## 10_benchmark_prediction_failures.R
## Compare how fbrglm / glmnet (naive vs safe) / glmnetUtils handle a
## factor-level mismatch where test$g has narrower levels than train$g.
##
## Expected outcome:
##   fbrglm            -> success (xlevels carried on the fit object)
##   glmnet_raw_naive  -> FAIL    (test model.matrix has fewer columns)
##   glmnet_raw_safe   -> success (test factor manually relevelled)
##   glmnetUtils       -> recorded as observed (its own xlevels logic)

suppressPackageStartupMessages({
    library(fbrglm)
    library(glmnet)
    library(glmnetUtils)
})

here_root <- "/home/koki/dev/fbrglm-experiments"
source(file.path(here_root, "R", "data_generators.R"), local = TRUE)
source(file.path(here_root, "R", "benchmark_helpers.R"), local = TRUE)

set.seed(110)
d <- make_factor_mismatch_data(n_train = 200, n_test = 50, seed = 110)
train <- d$train
test  <- d$test
stopifnot(setequal(levels(train$g), c("A", "B", "C", "D")))
stopifnot(setequal(levels(test$g),  c("A", "B")))
stopifnot(all(c("A", "B", "C", "D") %in% as.character(train$g)))

lam <- 0.05

row <- function(method, success, err, pred_len, train_ncol, test_ncol, note) {
    data.frame(
        method        = method,
        success       = success,
        error_message = if (is.na(err) || nzchar(err)) err else NA_character_,
        pred_length   = pred_len,
        train_ncol    = train_ncol,
        test_ncol     = test_ncol,
        note          = note,
        stringsAsFactors = FALSE
    )
}

results <- list()

## --- fbrglm ----------------------------------------------------------
fbr_fit <- safe_run(
    fbrglm(y ~ x1 + x2 + g, data = train, family = "binomial",
           lambda = "fix", lambda_value = lam)
)
fbr_train_ncol <- if (fbr_fit$success) {
    length(fbr_fit$value$x_colnames)
} else NA_integer_

if (fbr_fit$success) {
    fbr_pred <- safe_run(
        predict(fbr_fit$value, newdata = test, type = "response")
    )
} else {
    fbr_pred <- list(success = FALSE, error = "fit failed",
                     elapsed = NA_real_, value = NULL)
}
fbr_success <- fbr_fit$success && fbr_pred$success
fbr_err <- {
    if (!fbr_fit$success) fbr_fit$error
    else if (!fbr_pred$success) fbr_pred$error
    else NA_character_
}
results$fbrglm <- row(
    "fbrglm", fbr_success, fbr_err,
    pred_len  = if (fbr_pred$success) length(fbr_pred$value) else NA_integer_,
    train_ncol = fbr_train_ncol,
    ## fbrglm's predict reshapes test X to train_ncol via xlevels.
    test_ncol  = if (fbr_pred$success) fbr_train_ncol else NA_integer_,
    note = "auto: xlevels stored on fit object"
)

## --- glmnet_raw_naive ------------------------------------------------
## Build train and test design matrices independently. Predict will see
## a width mismatch.
naive_X_tr <- stats::model.matrix(y ~ x1 + x2 + g, data = train)[, -1,
                                                                 drop = FALSE]
naive_X_te <- stats::model.matrix(~ x1 + x2 + g, data = test)[, -1,
                                                              drop = FALSE]
naive_train_ncol <- ncol(naive_X_tr)
naive_test_ncol  <- ncol(naive_X_te)

naive_fit <- safe_run(
    glmnet::glmnet(naive_X_tr, train$y, family = "binomial",
                   alpha = 1, lambda = lam)
)
naive_pred <- if (naive_fit$success) {
    safe_run(as.vector(predict(naive_fit$value, newx = naive_X_te,
                               s = lam, type = "response")))
} else {
    list(success = FALSE, error = "fit failed",
         elapsed = NA_real_, value = NULL)
}
naive_success <- naive_fit$success && naive_pred$success
naive_err <- {
    if (!naive_fit$success) naive_fit$error
    else if (!naive_pred$success) naive_pred$error
    else NA_character_
}
results$glmnet_raw_naive <- row(
    "glmnet_raw_naive", naive_success, naive_err,
    pred_len  = if (naive_pred$success) length(naive_pred$value)
                else NA_integer_,
    train_ncol = naive_train_ncol,
    test_ncol  = naive_test_ncol,
    note = "manual: model.matrix(train) vs model.matrix(test) separately"
)

## --- glmnet_raw_safe -------------------------------------------------
## User manually carries train's levels onto test before model.matrix.
safe_xlevels <- levels(train$g)
safe_X_tr <- stats::model.matrix(y ~ x1 + x2 + g, data = train)[, -1,
                                                                drop = FALSE]
safe_test <- test
safe_test$g <- factor(test$g, levels = safe_xlevels)
safe_X_te <- stats::model.matrix(~ x1 + x2 + g, data = safe_test)[, -1,
                                                                  drop = FALSE]
safe_train_ncol <- ncol(safe_X_tr)
safe_test_ncol  <- ncol(safe_X_te)

safe_fit <- safe_run(
    glmnet::glmnet(safe_X_tr, train$y, family = "binomial",
                   alpha = 1, lambda = lam)
)
safe_pred <- if (safe_fit$success) {
    safe_run(as.vector(predict(safe_fit$value, newx = safe_X_te,
                               s = lam, type = "response")))
} else {
    list(success = FALSE, error = "fit failed",
         elapsed = NA_real_, value = NULL)
}
safe_success <- safe_fit$success && safe_pred$success
safe_err <- {
    if (!safe_fit$success) safe_fit$error
    else if (!safe_pred$success) safe_pred$error
    else NA_character_
}
results$glmnet_raw_safe <- row(
    "glmnet_raw_safe", safe_success, safe_err,
    pred_len  = if (safe_pred$success) length(safe_pred$value)
                else NA_integer_,
    train_ncol = safe_train_ncol,
    test_ncol  = safe_test_ncol,
    note = "manual: relevel(test$g, levels = levels(train$g)) before model.matrix"
)

## --- glmnetUtils -----------------------------------------------------
gu_fit <- safe_run(
    glmnetUtils::glmnet(y ~ x1 + x2 + g, data = train, family = "binomial",
                        alpha = 1, lambda = lam)
)
gu_train_ncol <- if (gu_fit$success) {
    nrow(coef(gu_fit$value, s = lam)) - 1L   # drop intercept row
} else NA_integer_

if (gu_fit$success) {
    gu_pred <- safe_run(
        as.vector(predict(gu_fit$value, newdata = test, s = lam,
                          type = "response"))
    )
} else {
    gu_pred <- list(success = FALSE, error = "fit failed",
                    elapsed = NA_real_, value = NULL)
}
gu_success <- gu_fit$success && gu_pred$success
gu_err <- {
    if (!gu_fit$success) gu_fit$error
    else if (!gu_pred$success) gu_pred$error
    else NA_character_
}
results$glmnetUtils <- row(
    "glmnetUtils", gu_success, gu_err,
    pred_len  = if (gu_pred$success) length(gu_pred$value) else NA_integer_,
    train_ncol = gu_train_ncol,
    test_ncol  = if (gu_pred$success) gu_train_ncol else NA_integer_,
    note = "observed: formula interface, but failed under narrowed test factor levels"
)

## --- assemble & save -------------------------------------------------
df <- do.call(rbind, results)
rownames(df) <- NULL

out_path <- file.path(here_root, "results", "summary",
                      "prediction_failures_small.csv")
save_result_csv(df, out_path)

print(df)
message("[10] prediction_failures: done")
