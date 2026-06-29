## 30_make_manuscript_tables.R
## Build manuscript-/README-ready summary tables off the small benchmark
## CSVs. Produces both CSV (machine-friendly) and Markdown pipe tables
## (paste-friendly).
##
## Inputs:
##   results/summary/prediction_failures_small.csv        (narrowed test)
##   results/summary/prediction_failures_novel_small.csv  (novel-level test)
##   results/summary/runtime_small.csv
##
## Outputs:
##   results/summary/manuscript_prediction_table.csv          (Table 3a)
##   results/summary/manuscript_prediction_table.md           (Table 3a)
##   results/summary/manuscript_prediction_table_novel.csv    (Table 3b)
##   results/summary/manuscript_prediction_table_novel.md     (Table 3b)
##   results/summary/manuscript_runtime_table.csv
##   results/summary/manuscript_runtime_table.md

suppressPackageStartupMessages({
    library(knitr)
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
source(file.path(here_root, "R", "benchmark_helpers.R"), local = TRUE)

csv_pred       <- file.path(here_root, "results", "summary",
                            "prediction_failures_small.csv")
csv_pred_novel <- file.path(here_root, "results", "summary",
                            "prediction_failures_novel_small.csv")
csv_rt         <- file.path(here_root, "results", "summary",
                            "runtime_small.csv")

if (!file.exists(csv_pred) || !file.exists(csv_rt)) {
    message("[30] Required input CSV(s) missing; run first:")
    message("        Rscript scripts/run_all_benchmarks_small.R")
    stop("missing benchmark CSVs", call. = FALSE)
}

out_dir <- file.path(here_root, "results", "summary")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## --- prediction tables (Table 3a + Table 3b) -------------------------
## Two scenarios share the same downstream layout; we just key the
## meta-text by scenario + method.
pred_meta_narrowed <- list(
    fbrglm = list(
        user_burden    = "automatic",
        interpretation = "default glm-strict path; auto-aligns the narrowed test"
    ),
    glmnet_raw_naive = list(
        user_burden    = "manual matrix alignment required",
        interpretation = "naive separately-built model matrices mismatch in width"
    ),
    glmnet_raw_safe = list(
        user_burden    = "manual matrix alignment required",
        interpretation = "succeeds when user re-levels test$g to match train"
    ),
    glmnetUtils = list(
        user_burden    = "automatic",
        interpretation = "default fast path: no xlevels carried; fails like raw glmnet"
    ),
    glmnetUtils_mf = list(
        user_burden    = "user must set use.model.frame = TRUE",
        interpretation = "model.frame path carries xlevels; succeeds (matches glm())"
    ),
    parsnip_workflow = list(
        user_burden    = "workflow framework",
        interpretation = "hardhat preprocessor carries xlevels"
    )
)

pred_meta_novel <- list(
    fbrglm = list(
        user_burden    = "automatic",
        interpretation = "default glm-strict path: errors on novel levels (like predict.glm)"
    ),
    fbrglm_na = list(
        user_burden    = "user sets on_new_levels = 'na'",
        interpretation = "opt-in production path: novel-level rows return NA with a warning"
    ),
    glmnet_raw_naive = list(
        user_burden    = "manual matrix alignment required",
        interpretation = "fails with raw column-width error (not the glm error message)"
    ),
    glmnetUtils = list(
        user_burden    = "automatic",
        interpretation = "default fast path: fails with raw column-width error"
    ),
    glmnetUtils_mf = list(
        user_burden    = "user must set use.model.frame = TRUE",
        interpretation = "model.frame path errors with the glm() new-levels message"
    ),
    parsnip_workflow = list(
        user_burden    = "workflow framework",
        interpretation = "warns, silently substitutes the reference level, returns finite predictions"
    ),
    base_glm = list(
        user_burden    = "reference",
        interpretation = "stats::glm + predict.glm: errors on novel levels (the gold standard)"
    )
)

build_pred_table <- function(csv_path, meta, result_col_label) {
    df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
    rows <- vector("list", nrow(df))
    for (i in seq_len(nrow(df))) {
        m  <- df$method[i]
        md <- meta[[m]]
        if (is.null(md)) {
            md <- list(user_burden = NA_character_,
                       interpretation = NA_character_)
        }
        rows[[i]] <- data.frame(
            method = m,
            result = if (isTRUE(as.logical(df$success[i]))) "Success" else "Failure",
            user_burden    = md$user_burden,
            interpretation = md$interpretation,
            stringsAsFactors = FALSE
        )
    }
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    names(out)[2L] <- result_col_label
    out
}

pred_out <- build_pred_table(csv_pred, pred_meta_narrowed,
                              "prediction_under_narrowed_factor_levels")
pred_csv <- file.path(out_dir, "manuscript_prediction_table.csv")
save_result_csv(pred_out, pred_csv)

pred_md_path <- file.path(out_dir, "manuscript_prediction_table.md")
pred_md <- knitr::kable(
    pred_out, format = "pipe",
    col.names = c("method",
                  "prediction under narrowed factor levels",
                  "user burden",
                  "interpretation"),
    align = c("l", "l", "l", "l")
)
writeLines(c("<!-- generated by scripts/30_make_manuscript_tables.R -->",
             "", as.character(pred_md), ""), pred_md_path)
message("[30] wrote ", pred_md_path)

if (file.exists(csv_pred_novel)) {
    pred_novel_out <- build_pred_table(
        csv_pred_novel, pred_meta_novel,
        "prediction_under_novel_factor_levels"
    )
    pred_novel_csv <- file.path(out_dir,
                                "manuscript_prediction_table_novel.csv")
    save_result_csv(pred_novel_out, pred_novel_csv)

    pred_novel_md_path <- file.path(out_dir,
                                    "manuscript_prediction_table_novel.md")
    pred_novel_md <- knitr::kable(
        pred_novel_out, format = "pipe",
        col.names = c("method",
                      "prediction under novel test factor levels",
                      "user burden",
                      "interpretation"),
        align = c("l", "l", "l", "l")
    )
    writeLines(c("<!-- generated by scripts/30_make_manuscript_tables.R -->",
                 "", as.character(pred_novel_md), ""), pred_novel_md_path)
    message("[30] wrote ", pred_novel_md_path)
} else {
    message("[30] prediction_failures_novel_small.csv not found; ",
            "skipping novel-level table.")
    pred_novel_out <- NULL
}

## --- runtime table --------------------------------------------------
rt_in <- utils::read.csv(csv_rt, stringsAsFactors = FALSE)

rt_meta <- list(
    fbrglm           = "lightweight formula interface",
    glmnet_raw       = "fastest matrix API",
    glmnetUtils      = "formula interface",
    parsnip_workflow = "full workflow framework"
)

## Per-scenario reference time: glmnet_raw row.
ref_by_scn <- tapply(
    rt_in$runtime_sec[rt_in$method == "glmnet_raw"],
    rt_in$scenario[rt_in$method == "glmnet_raw"],
    identity
)

scn_order    <- unique(rt_in$scenario)
method_order <- c("fbrglm", "glmnet_raw", "glmnetUtils", "parsnip_workflow")

rt_rows <- vector("list", nrow(rt_in))
for (i in seq_len(nrow(rt_in))) {
    m   <- rt_in$method[i]
    scn <- rt_in$scenario[i]
    ref <- ref_by_scn[[scn]]
    ratio <- if (!is.null(ref) && !is.na(ref) && ref > 0 &&
                 !is.na(rt_in$runtime_sec[i])) {
        rt_in$runtime_sec[i] / ref
    } else NA_real_
    rt_rows[[i]] <- data.frame(
        scenario = scn,
        method   = m,
        median_ms = if (is.na(rt_in$runtime_sec[i])) NA_real_
                    else rt_in$runtime_sec[i] * 1000,
        relative_to_raw_glmnet = ratio,
        interpretation = rt_meta[[m]] %||% NA_character_,
        stringsAsFactors = FALSE
    )
}
rt_out <- do.call(rbind, rt_rows)

## Stable sort: scenario in CSV order, method in canonical order.
rt_out$scenario <- factor(rt_out$scenario, levels = scn_order)
rt_out$method   <- factor(rt_out$method,
                          levels = intersect(method_order, unique(rt_out$method)))
rt_out <- rt_out[order(rt_out$scenario, rt_out$method), ]
rt_out$scenario <- as.character(rt_out$scenario)
rt_out$method   <- as.character(rt_out$method)
rownames(rt_out) <- NULL

rt_csv <- file.path(out_dir, "manuscript_runtime_table.csv")
save_result_csv(rt_out, rt_csv)

rt_display <- rt_out
rt_display$median_ms <- sprintf("%.2f", rt_display$median_ms)
rt_display$relative_to_raw_glmnet <- sprintf("%.2f×",
                                             rt_display$relative_to_raw_glmnet)

rt_md_path <- file.path(out_dir, "manuscript_runtime_table.md")
rt_md <- knitr::kable(
    rt_display,
    format    = "pipe",
    col.names = c("scenario", "method", "median (ms)",
                  "relative to raw glmnet", "interpretation"),
    align = c("l", "l", "r", "r", "l")
)
writeLines(c("<!-- generated by scripts/30_make_manuscript_tables.R -->",
             "", as.character(rt_md), ""),
           rt_md_path)
message("[30] wrote ", rt_md_path)

cat("\n=== Prediction table (narrowed test) ===\n")
print(pred_out)
if (!is.null(pred_novel_out)) {
    cat("\n=== Prediction table (novel-level test) ===\n")
    print(pred_novel_out)
}
cat("\n=== Runtime table ===\n")
print(rt_out)

## --- family parity table --------------------------------------------
csv_fam <- file.path(here_root, "results", "summary",
                     "families_small.csv")
if (file.exists(csv_fam)) {
    fam_in <- utils::read.csv(csv_fam, stringsAsFactors = FALSE)

    family_order <- c("gaussian", "binomial", "poisson", "gamma",
                      "negbin_fixed_theta", "cox", "multinomial",
                      "mgaussian")
    family_display <- c(
        gaussian           = "Gaussian",
        binomial           = "Binomial",
        poisson            = "Poisson",
        gamma              = "Gamma",
        negbin_fixed_theta = "Negative binomial",
        cox                = "Cox",
        multinomial        = "Multinomial",
        mgaussian          = "Multi-response Gaussian"
    )
    response_label <- c(
        gaussian           = "continuous",
        binomial           = "binary (0/1)",
        poisson            = "count",
        gamma              = "positive continuous",
        negbin_fixed_theta = "overdispersed count",
        cox                = "Surv(time, status)",
        multinomial        = "multiclass factor",
        mgaussian          = "numeric matrix"
    )
    note_label <- c(
        gaussian           = "",
        binomial           = "",
        poisson            = "",
        gamma              = "Gamma(link = 'log') family object",
        negbin_fixed_theta = "fixed theta only",
        cox                = "native glmnet Cox; relative-risk scale",
        multinomial        = "(n x k) matrix output",
        mgaussian          = "(n x q) matrix output"
    )

    fmt_diff <- function(d) {
        if (is.na(d)) return("n/a")
        if (d == 0) return("0")
        sprintf("%.1e", d)
    }
    fmt_ms <- function(s) if (is.na(s)) "n/a" else sprintf("%.1f", s * 1000)

    fam_rows <- lapply(family_order, function(fid) {
        sub <- fam_in[fam_in$family_id == fid, ]
        if (!nrow(sub)) return(NULL)
        pick <- function(m, col) sub[[col]][sub$method == m]
        ps_attempted <- isTRUE(as.logical(pick("parsnip_workflow",
                                               "supported_attempted")))
        ps_ok        <- isTRUE(as.logical(pick("parsnip_workflow", "success")))
        gu_ok        <- isTRUE(as.logical(pick("glmnetUtils",      "success")))
        formula_iface <- {
            parts <- c("fbrglm")
            if (gu_ok)               parts <- c(parts, "glmnetUtils")
            if (ps_attempted && ps_ok) parts <- c(parts, "parsnip")
            s <- paste(parts, collapse = " + ")
            if (!ps_attempted) s <- paste0(s, " (parsnip not attempted)")
            s
        }
        data.frame(
            family                = unname(family_display[fid]),
            response              = unname(response_label[fid]),
            fbrglm_vs_glmnet      = fmt_diff(pick("fbrglm", "max_abs_diff_vs_glmnet")),
            fbrglm_runtime_ms     = fmt_ms(pick("fbrglm",     "runtime_sec")),
            raw_glmnet_runtime_ms = fmt_ms(pick("glmnet_raw", "runtime_sec")),
            formula_interfaces    = formula_iface,
            note                  = unname(note_label[fid]),
            stringsAsFactors = FALSE
        )
    })
    fam_out <- do.call(rbind, fam_rows[!vapply(fam_rows, is.null, logical(1))])
    rownames(fam_out) <- NULL

    fam_csv <- file.path(out_dir, "manuscript_family_table.csv")
    save_result_csv(fam_out, fam_csv)

    fam_md_path <- file.path(out_dir, "manuscript_family_table.md")
    fam_md <- knitr::kable(
        fam_out, format = "pipe",
        col.names = c("family", "response", "fbrglm vs raw glmnet",
                      "fbrglm (ms)", "raw glmnet (ms)",
                      "formula APIs that succeeded", "note"),
        align = c("l", "l", "r", "r", "r", "l", "l")
    )
    writeLines(c("<!-- generated by scripts/30_make_manuscript_tables.R -->",
                 "", as.character(fam_md), ""),
               fam_md_path)
    message("[30] wrote ", fam_md_path)
} else {
    message("[30] families_small.csv not found; skipping family table.")
}

message("[30] manuscript_tables: done")
