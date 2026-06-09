## 12_benchmark_families_small.R
## Family-wide comparison of fbrglm against raw glmnet (the reference),
## plus glmnetUtils and a parsnip / workflows pipeline when those have
## an equivalent for the family in question.
##
## Per (family, method) row, the script records:
##   supported_attempted, success, runtime_sec, max_abs_diff_vs_glmnet,
##   prediction_shape, error_message, note
##
## Writes:
##   results/raw/families_small_raw.csv     -- the long row form (same data)
##   results/summary/families_small.csv     -- the canonical CSV

suppressPackageStartupMessages({
    library(fbrglm)
    library(glmnet)
    library(glmnetUtils)
    library(parsnip)
    library(workflows)
    if (requireNamespace("survival", quietly = TRUE)) library(survival)
    if (requireNamespace("MASS",     quietly = TRUE)) library(MASS)
})

here_root <- (function() {
    e <- Sys.getenv("FBRGLM_EXP_ROOT")
    if (nzchar(e))                                          return(e)
    if (file.exists("environment.yml") && dir.exists("R")) return(getwd())
    "/home/koki/dev/fbrglm-experiments"
})()
source(file.path(here_root, "R", "data_generators.R"), local = TRUE)
source(file.path(here_root, "R", "benchmark_helpers.R"), local = TRUE)

lam       <- 0.05
n_default <- 200

shape_str <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.null(dim(x))) return(paste0("vec(", length(x), ")"))
    paste0(class(x)[1], "(", paste(dim(x), collapse = ","), ")")
}
diff_max <- function(a, b) {
    if (is.null(a) || is.null(b)) return(NA_real_)
    av <- tryCatch(as.numeric(a), error = function(e) NA_real_)
    bv <- tryCatch(as.numeric(b), error = function(e) NA_real_)
    if (anyNA(av) || anyNA(bv) || length(av) != length(bv)) return(NA_real_)
    max(abs(av - bv))
}

## --- scenarios -------------------------------------------------------
build_scenarios <- function() {
    has_MASS     <- requireNamespace("MASS",     quietly = TRUE)
    has_survival <- requireNamespace("survival", quietly = TRUE)
    list(
        list(
            id = "gaussian",
            data = make_family_gaussian_data(n = n_default, seed = 11),
            x_formula = ~ x1 + x2 + x3 + x4,
            formula   = y ~ x1 + x2 + x3 + x4,
            y_for_glmnet = function(d) d$y,
            family = "gaussian",
            predict_type = "response",
            parsnip = list(
                available = TRUE,
                spec = parsnip::linear_reg(penalty = lam, mixture = 1) |>
                    parsnip::set_engine("glmnet") |> parsnip::set_mode("regression"),
                data_prep = identity,
                formula   = y ~ x1 + x2 + x3 + x4,
                predict_type = "numeric",
                extract = function(pr) as.numeric(pr$.pred)
            )
        ),
        list(
            id = "binomial",
            data = make_family_binomial_data(n = n_default, seed = 12),
            x_formula = ~ x1 + x2 + x3 + x4,
            formula   = y ~ x1 + x2 + x3 + x4,
            y_for_glmnet = function(d) d$y,
            family = "binomial",
            predict_type = "response",
            parsnip = list(
                available = TRUE,
                spec = parsnip::logistic_reg(penalty = lam, mixture = 1) |>
                    parsnip::set_engine("glmnet") |> parsnip::set_mode("classification"),
                data_prep = function(d) { d$y <- factor(d$y); d },
                formula   = y ~ x1 + x2 + x3 + x4,
                predict_type = "prob",
                extract = function(pr) as.numeric(pr$.pred_1)
            )
        ),
        list(
            id = "poisson",
            data = make_family_poisson_data(n = n_default, seed = 13),
            x_formula = ~ x1 + x2 + x3 + x4,
            formula   = y ~ x1 + x2 + x3 + x4,
            y_for_glmnet = function(d) d$y,
            family = "poisson",
            predict_type = "response",
            parsnip = list(available = FALSE,
                           reason = "no parsnip path tried: requires the poissonreg extension")
        ),
        list(
            id = "gamma",
            data = make_family_gamma_data(n = n_default, seed = 14),
            x_formula = ~ x1 + x2 + x3 + x4,
            formula   = y ~ x1 + x2 + x3 + x4,
            y_for_glmnet = function(d) d$y,
            family = stats::Gamma(link = "log"),
            predict_type = "response",
            parsnip = list(available = FALSE,
                           reason = "no parsnip equivalent for Gamma + glmnet engine")
        ),
        list(
            id = "negbin_fixed_theta",
            data = make_family_negbin_data(n = n_default, seed = 15),
            x_formula = ~ x1 + x2 + x3 + x4,
            formula   = y ~ x1 + x2 + x3 + x4,
            y_for_glmnet = function(d) d$y,
            family = if (has_MASS) MASS::negative.binomial(theta = 2) else NULL,
            predict_type = "response",
            parsnip = list(available = FALSE,
                           reason = "no parsnip equivalent for fixed-theta negative binomial + glmnet")
        ),
        list(
            id = "cox",
            data = if (has_survival)
                make_family_cox_data(n = n_default, seed = 16) else NULL,
            x_formula = ~ x1 + x2 + x3 + x4,
            formula   = if (has_survival)
                survival::Surv(t_obs, delta) ~ x1 + x2 + x3 + x4 else NULL,
            y_for_glmnet = if (has_survival)
                function(d) survival::Surv(d$t_obs, d$delta) else NULL,
            family = "cox",
            predict_type = "link",
            parsnip = list(available = FALSE,
                           reason = "Cox in parsnip needs the censored extension; not used here")
        ),
        list(
            id = "multinomial",
            data = make_family_multinomial_data(n = n_default, seed = 17),
            x_formula = ~ x1 + x2 + x3 + x4,
            formula   = y ~ x1 + x2 + x3 + x4,
            y_for_glmnet = function(d) d$y,
            family = "multinomial",
            predict_type = "response",
            parsnip = list(
                available = TRUE,
                spec = parsnip::multinom_reg(penalty = lam, mixture = 1) |>
                    parsnip::set_engine("glmnet") |> parsnip::set_mode("classification"),
                data_prep = identity,
                formula   = y ~ x1 + x2 + x3 + x4,
                predict_type = "prob",
                extract = function(pr) {
                    cols <- grep("^\\.pred_", names(pr), value = TRUE)
                    as.numeric(as.matrix(pr[, cols]))
                }
            )
        ),
        list(
            id = "mgaussian",
            data = make_family_mgaussian_data(n = n_default, seed = 18),
            x_formula = ~ x1 + x2 + x3 + x4,
            formula   = cbind(y1, y2) ~ x1 + x2 + x3 + x4,
            y_for_glmnet = function(d) cbind(d$y1, d$y2),
            family = "mgaussian",
            predict_type = "response",
            parsnip = list(available = FALSE,
                           reason = "no parsnip equivalent for multi-response gaussian")
        )
    )
}

## --- per-method runners ---------------------------------------------
run_fbrglm <- function(sc) {
    safe_run({
        fit <- fbrglm(sc$formula, data = sc$data, family = sc$family,
                      lambda = "fix", lambda_value = lam)
        suppressWarnings(predict(fit, type = sc$predict_type))
    })
}
run_glmnet_raw <- function(sc) {
    safe_run({
        X <- stats::model.matrix(sc$x_formula, data = sc$data)[, -1, drop = FALSE]
        y <- sc$y_for_glmnet(sc$data)
        fit <- suppressWarnings(
            glmnet::glmnet(X, y, family = sc$family, alpha = 1, lambda = lam))
        pr  <- suppressWarnings(
            predict(fit, newx = X, s = lam, type = sc$predict_type))
        if (length(dim(pr)) == 3L && dim(pr)[3L] == 1L) pr <- pr[, , 1]
        if (is.matrix(pr) && ncol(pr) == 1L) pr <- as.vector(pr)
        pr
    })
}
run_glmnetUtils <- function(sc) {
    safe_run({
        fit <- suppressWarnings(
            glmnetUtils::glmnet(sc$formula, data = sc$data, family = sc$family,
                                alpha = 1, lambda = lam))
        pr <- suppressWarnings(
            predict(fit, newdata = sc$data, s = lam, type = sc$predict_type))
        if (length(dim(pr)) == 3L && dim(pr)[3L] == 1L) pr <- pr[, , 1]
        if (is.matrix(pr) && ncol(pr) == 1L) pr <- as.vector(pr)
        pr
    })
}
run_parsnip <- function(sc) {
    if (isFALSE(sc$parsnip$available)) {
        return(list(success = FALSE, elapsed = NA_real_,
                    error = sc$parsnip$reason, value = NULL,
                    attempted = FALSE))
    }
    res <- safe_run({
        data_p <- sc$parsnip$data_prep(sc$data)
        wf <- workflows::workflow() |>
            workflows::add_formula(sc$parsnip$formula) |>
            workflows::add_model(sc$parsnip$spec)
        fit_obj <- fit(wf, data = data_p)
        pr <- predict(fit_obj, new_data = data_p,
                      type = sc$parsnip$predict_type)
        sc$parsnip$extract(pr)
    })
    res$attempted <- TRUE
    res
}

method_runners <- list(
    fbrglm           = run_fbrglm,
    glmnet_raw       = run_glmnet_raw,
    glmnetUtils      = run_glmnetUtils,
    parsnip_workflow = run_parsnip
)
method_notes <- c(
    fbrglm           = "fbrglm formula API",
    glmnet_raw       = "matrix API; reference",
    glmnetUtils      = "glmnetUtils formula API",
    parsnip_workflow = "tidymodels workflow"
)

## --- main loop ------------------------------------------------------
scenarios <- build_scenarios()
raw_rows  <- list()

for (sc in scenarios) {
    if (is.null(sc$family) || is.null(sc$data)) {
        message(sprintf(
            "[12] %-22s SKIP (family not constructible -- support package missing)",
            sc$id))
        next
    }
    message(sprintf("[12] family %s", sc$id))

    sc_results <- lapply(method_runners, function(fn) fn(sc))
    ref_pred <- if (isTRUE(sc_results$glmnet_raw$success))
        sc_results$glmnet_raw$value else NULL

    for (mname in names(method_runners)) {
        r <- sc_results[[mname]]
        attempted <- if (mname == "parsnip_workflow")
            isTRUE(r$attempted) else TRUE
        success <- if (!attempted) FALSE else isTRUE(r$success)
        diff <- if (mname == "glmnet_raw") {
            if (success) 0 else NA_real_
        } else if (success && !is.null(ref_pred)) {
            diff_max(r$value, ref_pred)
        } else NA_real_
        note <- if (mname == "parsnip_workflow" && !attempted)
            sc$parsnip$reason else method_notes[[mname]]
        raw_rows[[length(raw_rows) + 1]] <- data.frame(
            family_id              = sc$id,
            method                 = mname,
            supported_attempted    = attempted,
            success                = success,
            runtime_sec            = r$elapsed,
            max_abs_diff_vs_glmnet = diff,
            prediction_shape       = shape_str(r$value),
            error_message          = if (success) NA_character_ else r$error,
            note                   = note,
            stringsAsFactors = FALSE
        )
    }
}

out_df <- do.call(rbind, raw_rows)
rownames(out_df) <- NULL

save_result_csv(out_df, file.path(here_root, "results", "raw",
                                  "families_small_raw.csv"))
save_result_csv(out_df, file.path(here_root, "results", "summary",
                                  "families_small.csv"))

print(out_df[, c("family_id", "method", "supported_attempted", "success",
                  "max_abs_diff_vs_glmnet", "prediction_shape")])
message("[12] families_small: done")
