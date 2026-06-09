## run_all_smoke.R
## Source 00–05 in order. Print which script failed, if any.

scripts <- c(
    "scripts/00_install_local_fbrglm.R",
    "scripts/01_smoke_basic.R",
    "scripts/02_against_glmnet_fixed_lambda.R",
    "scripts/03_predict_factor_newdata.R",
    "scripts/04_missing_report.R",
    "scripts/05_lambda_cv_equivalence.R"
)

for (s in scripts) {
    message("\n=== running ", s, " ===")
    res <- tryCatch(
        {
            source(s, local = TRUE, echo = FALSE)
            TRUE
        },
        error = function(e) {
            message("!!! FAILED at ", s)
            message("    error: ", conditionMessage(e))
            FALSE
        }
    )
    if (!isTRUE(res)) {
        stop("Smoke run aborted at ", s, call. = FALSE)
    }
}

message("\n=== All smoke scripts passed ===")
