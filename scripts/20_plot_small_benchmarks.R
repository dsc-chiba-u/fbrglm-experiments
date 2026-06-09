## 20_plot_small_benchmarks.R
## Build the first two figures off of the small-benchmark CSVs.

suppressPackageStartupMessages({
    library(ggplot2)
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

csv_failures <- file.path(here_root, "results", "summary",
                          "prediction_failures_small.csv")
csv_runtime  <- file.path(here_root, "results", "summary",
                          "runtime_small.csv")

if (!file.exists(csv_failures) || !file.exists(csv_runtime)) {
    message("[20] Required CSV(s) missing; run first:")
    message("        Rscript scripts/run_all_benchmarks_small.R")
    stop("missing benchmark CSVs", call. = FALSE)
}

fig_dir <- file.path(here_root, "results", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

## --- prediction_failures plot ----------------------------------------
fail_df <- utils::read.csv(csv_failures, stringsAsFactors = FALSE)
## Preserve the order from the CSV (which is the order methods were
## evaluated) so the bars don't get alphabetically shuffled.
fail_df$method <- factor(fail_df$method, levels = fail_df$method)
fail_df$success <- as.logical(fail_df$success)
fail_df$success_label <- ifelse(fail_df$success, "success", "failure")
fail_df$bar_value <- 1L

## Build an annotation: success → "success", failure → trimmed error.
short_err <- function(msg, n = 50) {
    if (is.na(msg) || !nzchar(msg)) return("")
    if (nchar(msg) > n) paste0(substr(msg, 1, n - 1), "…") else msg
}
fail_df$annotation <- ifelse(
    fail_df$success,
    sprintf("OK\nncol: train=%d, test=%d",
            fail_df$train_ncol, fail_df$test_ncol),
    sprintf("FAIL\nncol: train=%d, test=%s\n%s",
            fail_df$train_ncol,
            ifelse(is.na(fail_df$test_ncol), "NA",
                   as.character(fail_df$test_ncol)),
            vapply(fail_df$error_message, short_err, character(1)))
)

p_fail <- ggplot(fail_df,
                 aes(x = method, y = bar_value, fill = success_label)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = annotation),
              vjust = 1.1, size = 3, lineheight = 0.95) +
    scale_fill_manual(values = c(success = "#2C7BB6",
                                 failure = "#D7191C"),
                      name = NULL) +
    labs(
        title    = "Factor-level mismatch: who handles it",
        subtitle = "train$g has 4 levels (A/B/C/D); test$g has 2 levels (A/B)",
        x = NULL,
        y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position  = "bottom"
    )

fail_png <- file.path(fig_dir, "prediction_failures_small.png")
ggsave(fail_png, p_fail, width = 7, height = 4, dpi = 300)
message("[20] wrote ", fail_png)

## --- runtime plot ----------------------------------------------------
rt_df <- utils::read.csv(csv_runtime, stringsAsFactors = FALSE)
rt_df$success    <- as.logical(rt_df$success)
rt_df$runtime_ms <- rt_df$runtime_sec * 1000

## Canonical method order; preserve CSV scenario order.
method_levels <- c("fbrglm", "glmnet_raw", "glmnetUtils", "parsnip_workflow")
rt_df$method   <- factor(rt_df$method,
                         levels = intersect(method_levels, unique(rt_df$method)))
rt_df$scenario <- factor(rt_df$scenario, levels = unique(rt_df$scenario))

p_rt <- ggplot(rt_df,
               aes(x = method, y = runtime_ms, fill = method)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = ifelse(is.na(runtime_ms), "fail",
                                  sprintf("%.1f", runtime_ms))),
              vjust = -0.3, size = 2.8) +
    facet_wrap(~ scenario, scales = "free_y") +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    labs(
        title    = "fit + predict wall-clock across scenarios",
        subtitle = "binomial, lambda=0.05; median of 5 iterations via bench::mark()",
        x = NULL,
        y = "milliseconds"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

rt_png <- file.path(fig_dir, "runtime_small.png")
ggsave(rt_png, p_rt, width = 8, height = 5, dpi = 300)
message("[20] wrote ", rt_png)

message("[20] plot_small_benchmarks: done")
