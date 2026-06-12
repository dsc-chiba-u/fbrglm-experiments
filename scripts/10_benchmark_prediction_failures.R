## 10_benchmark_prediction_failures.R
##
## Compare how fbrglm, raw glmnet, glmnetUtils (two paths), parsnip /
## workflows, and base::glm() handle the two canonical factor-level
## scenarios at predict time:
##
##   Scenario 1 — narrowed_test:
##     train$g has levels A/B/C/D; test$g has narrower levels A/B.
##     The "missing" levels (C, D) just do not occur in test;
##     model.matrix(~ ... + g, test) is narrower than the train design.
##
##   Scenario 2 — novel_level_test:
##     train$g has levels A/B; test$g has wider levels A/B/C/D.
##     Test rows carry factor values the trained model never saw.
##     stats::glm() / predict.glm() would error here — this is the
##     glm-strict semantics fbrglm follows by default.
##
## The script writes two CSV files. Both have the same column layout so
## downstream tooling can read them with the same parser.
##
##   prediction_failures_small.csv        — Scenario 1 (kept for back-compat)
##   prediction_failures_novel_small.csv  — Scenario 2 (new)

suppressPackageStartupMessages({
    library(fbrglm)
    library(glmnet)
    library(glmnetUtils)
    library(parsnip)
    library(workflows)
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
source(file.path(here_root, "R", "data_generators.R"),   local = TRUE)
source(file.path(here_root, "R", "benchmark_helpers.R"), local = TRUE)

lam <- 0.05

row <- function(method, success, err, pred_len, train_ncol, test_ncol, note) {
    data.frame(
        method        = method,
        success       = success,
        error_message = if (is.na(err) || !nzchar(err)) NA_character_ else err,
        pred_length   = pred_len,
        train_ncol    = train_ncol,
        test_ncol     = test_ncol,
        note          = note,
        stringsAsFactors = FALSE
    )
}

## --- Per-method runners ------------------------------------------------
## Each runner returns a one-row data.frame via row(). They take a
## (train, test, mode) triple, where `mode` is one of "narrowed" /
## "novel" so a runner can swap behaviour when relevant
## (e.g. fbrglm_na is only sensible under "novel").

run_fbrglm <- function(train, test, mode, on_new_levels = "error") {
    label <- if (on_new_levels == "error") "fbrglm" else "fbrglm_na"
    fit <- safe_run(
        suppressMessages(
            fbrglm(y ~ x1 + x2 + g, data = train, family = "binomial",
                   lambda = "fix", lambda_value = lam)
        )
    )
    train_ncol <- if (fit$success) length(fit$value$x_colnames) else NA_integer_
    if (fit$success) {
        pred <- safe_run(suppressWarnings(
            predict(fit$value, newdata = test, type = "response",
                    on_new_levels = on_new_levels)
        ))
    } else {
        pred <- list(success = FALSE, error = "fit failed",
                     elapsed = NA_real_, value = NULL)
    }
    success <- fit$success && pred$success
    err <- if (!fit$success) fit$error
           else if (!pred$success) pred$error
           else NA_character_
    note <- if (on_new_levels == "error") {
        "default: glm-strict (error on novel levels, auto-align on narrowed)"
    } else {
        "opt-in: novel-level rows -> NA + warning, safe rows score normally"
    }
    row(label, success, err,
        pred_len   = if (pred$success) length(as.vector(pred$value))
                     else NA_integer_,
        train_ncol = train_ncol,
        test_ncol  = if (pred$success) train_ncol else NA_integer_,
        note       = note)
}

run_glmnet_raw_naive <- function(train, test, mode) {
    naive_X_tr <- stats::model.matrix(y ~ x1 + x2 + g, data = train)[, -1,
                                                                     drop = FALSE]
    naive_X_te <- stats::model.matrix(~ x1 + x2 + g, data = test)[, -1,
                                                                  drop = FALSE]
    fit <- safe_run(glmnet::glmnet(naive_X_tr, train$y, family = "binomial",
                                    alpha = 1, lambda = lam))
    pred <- if (fit$success) {
        safe_run(as.vector(predict(fit$value, newx = naive_X_te,
                                    s = lam, type = "response")))
    } else {
        list(success = FALSE, error = "fit failed",
             elapsed = NA_real_, value = NULL)
    }
    success <- fit$success && pred$success
    err <- if (!fit$success) fit$error
           else if (!pred$success) pred$error
           else NA_character_
    row("glmnet_raw_naive", success, err,
        pred_len   = if (pred$success) length(pred$value) else NA_integer_,
        train_ncol = ncol(naive_X_tr),
        test_ncol  = ncol(naive_X_te),
        note       = "manual: model.matrix(train) vs model.matrix(test) separately")
}

run_glmnet_raw_safe <- function(train, test, mode) {
    safe_xlevels <- levels(train$g)
    safe_X_tr <- stats::model.matrix(y ~ x1 + x2 + g, data = train)[, -1,
                                                                    drop = FALSE]
    safe_test <- test
    safe_test$g <- factor(test$g, levels = safe_xlevels)
    safe_X_te <- stats::model.matrix(~ x1 + x2 + g, data = safe_test)[, -1,
                                                                      drop = FALSE]
    fit <- safe_run(glmnet::glmnet(safe_X_tr, train$y, family = "binomial",
                                    alpha = 1, lambda = lam))
    pred <- if (fit$success) {
        safe_run(as.vector(predict(fit$value, newx = safe_X_te,
                                    s = lam, type = "response")))
    } else {
        list(success = FALSE, error = "fit failed",
             elapsed = NA_real_, value = NULL)
    }
    success <- fit$success && pred$success
    err <- if (!fit$success) fit$error
           else if (!pred$success) pred$error
           else NA_character_
    row("glmnet_raw_safe", success, err,
        pred_len   = if (pred$success) length(pred$value) else NA_integer_,
        train_ncol = ncol(safe_X_tr),
        test_ncol  = ncol(safe_X_te),
        note       = "manual: relevel(test$g, levels(train$g)) before model.matrix")
}

run_glmnetUtils <- function(train, test, mode, use_model_frame = FALSE) {
    label <- if (use_model_frame) "glmnetUtils_mf" else "glmnetUtils"
    fit_args <- list(formula = y ~ x1 + x2 + g, data = train,
                     family = "binomial", alpha = 1, lambda = lam)
    if (use_model_frame) fit_args$use.model.frame <- TRUE
    fit <- safe_run(do.call(glmnetUtils::glmnet, fit_args))
    train_ncol <- if (fit$success) {
        nrow(coef(fit$value, s = lam)) - 1L
    } else NA_integer_
    pred <- if (fit$success) {
        safe_run(as.vector(predict(fit$value, newdata = test, s = lam,
                                    type = "response")))
    } else {
        list(success = FALSE, error = "fit failed",
             elapsed = NA_real_, value = NULL)
    }
    success <- fit$success && pred$success
    err <- if (!fit$success) fit$error
           else if (!pred$success) pred$error
           else NA_character_
    note <- if (use_model_frame) {
        "glm-equivalent path: xlevels carried via model.frame()"
    } else {
        "default fast path: no xlevels carried; mirrors glmnet raw newx"
    }
    row(label, success, err,
        pred_len   = if (pred$success) length(pred$value) else NA_integer_,
        train_ncol = train_ncol,
        test_ncol  = if (pred$success) train_ncol else NA_integer_,
        note       = note)
}

run_parsnip <- function(train, test, mode) {
    train_p <- train
    train_p$y <- factor(train$y)
    fit <- safe_run({
        spec <- parsnip::logistic_reg(penalty = lam, mixture = 1) |>
            parsnip::set_engine("glmnet") |>
            parsnip::set_mode("classification")
        wf <- workflows::workflow() |>
            workflows::add_formula(y ~ x1 + x2 + g) |>
            workflows::add_model(spec)
        fit(wf, data = train_p)
    })
    pred <- if (fit$success) {
        safe_run(suppressWarnings({
            pr <- predict(fit$value, new_data = test, type = "prob")
            as.numeric(pr[[".pred_1"]])
        }))
    } else {
        list(success = FALSE, error = "fit failed",
             elapsed = NA_real_, value = NULL)
    }
    success <- fit$success && pred$success
    err <- if (!fit$success) fit$error
           else if (!pred$success) pred$error
           else NA_character_
    note <- if (mode == "novel") {
        "workflow: hardhat warns and silently coerces novel-level cells to NA (reference)"
    } else {
        "workflow: hardhat preprocessor carries xlevels"
    }
    row("parsnip_workflow", success, err,
        pred_len   = if (pred$success) length(pred$value) else NA_integer_,
        train_ncol = NA_integer_,
        test_ncol  = NA_integer_,
        note       = note)
}

run_base_glm <- function(train, test, mode) {
    fit <- safe_run(suppressWarnings(
        stats::glm(y ~ x1 + x2 + g, data = train, family = stats::binomial())
    ))
    pred <- if (fit$success) {
        safe_run(suppressWarnings(
            as.vector(stats::predict(fit$value, newdata = test, type = "response"))
        ))
    } else {
        list(success = FALSE, error = "fit failed",
             elapsed = NA_real_, value = NULL)
    }
    success <- fit$success && pred$success
    err <- if (!fit$success) fit$error
           else if (!pred$success) pred$error
           else NA_character_
    row("base_glm", success, err,
        pred_len   = if (pred$success) length(pred$value) else NA_integer_,
        train_ncol = NA_integer_,
        test_ncol  = NA_integer_,
        note       = "reference: stats::glm + predict.glm (glm-strict)")
}

## --- Scenario 1: narrowed_test (existing behaviour kept) -------------
set.seed(110)
d1 <- make_factor_mismatch_data(n_train = 200, n_test = 50, seed = 110)
stopifnot(setequal(levels(d1$train$g), c("A", "B", "C", "D")))
stopifnot(setequal(levels(d1$test$g),  c("A", "B")))

s1 <- list(
    run_fbrglm(d1$train, d1$test, mode = "narrowed", on_new_levels = "error"),
    run_glmnet_raw_naive(d1$train, d1$test, mode = "narrowed"),
    run_glmnet_raw_safe (d1$train, d1$test, mode = "narrowed"),
    run_glmnetUtils     (d1$train, d1$test, mode = "narrowed",
                          use_model_frame = FALSE),
    run_glmnetUtils     (d1$train, d1$test, mode = "narrowed",
                          use_model_frame = TRUE),
    run_parsnip         (d1$train, d1$test, mode = "narrowed")
)
df1 <- do.call(rbind, s1); rownames(df1) <- NULL
out1 <- file.path(here_root, "results", "summary",
                  "prediction_failures_small.csv")
save_result_csv(df1, out1)
message("[10] narrowed_test scenario:")
print(df1)

## --- Scenario 2: novel_level_test (new) -------------------------------
set.seed(210)
d2 <- make_factor_novel_level_data(n_train = 200, n_test = 50, seed = 210)
stopifnot(setequal(levels(d2$train$g), c("A", "B")))
stopifnot(setequal(levels(d2$test$g),  c("A", "B", "C", "D")))
stopifnot(any(as.character(d2$test$g) %in% c("C", "D")))

s2 <- list(
    run_fbrglm(d2$train, d2$test, mode = "novel", on_new_levels = "error"),
    run_fbrglm(d2$train, d2$test, mode = "novel", on_new_levels = "na"),
    run_glmnet_raw_naive(d2$train, d2$test, mode = "novel"),
    run_glmnetUtils     (d2$train, d2$test, mode = "novel",
                          use_model_frame = FALSE),
    run_glmnetUtils     (d2$train, d2$test, mode = "novel",
                          use_model_frame = TRUE),
    run_parsnip         (d2$train, d2$test, mode = "novel"),
    run_base_glm        (d2$train, d2$test, mode = "novel")
)
df2 <- do.call(rbind, s2); rownames(df2) <- NULL
out2 <- file.path(here_root, "results", "summary",
                  "prediction_failures_novel_small.csv")
save_result_csv(df2, out2)
message("[10] novel_level_test scenario:")
print(df2)

message("[10] prediction_failures: done")
