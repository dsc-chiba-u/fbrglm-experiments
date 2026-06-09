# fbrglm-experiments

Reproducible experiments and benchmarks for
[fbrglm](https://github.com/dsc-chiba-u/fbrglm) — a formula-based
regularized GLM that wraps `glmnet` with a `glm()`-style interface.

The fbrglm package itself lives in a separate repository. This
repository hosts:

- **smoke tests** — minimal correctness checks that verify every fbrglm
  release against `glmnet`.
- **small benchmarks** — first-cut comparisons against `glmnet` and
  `glmnetUtils`, intended to be runnable in seconds and to stay
  pinned in git.
- **conda environment** — `environment.yml` defining the
  `fbrglm-dev` conda env used throughout.

Larger benchmarks (high-dimensional, slow CV, inference variants) will
land here later; the small benchmarks are sized so they can be re-run
on every fbrglm change without friction.

## Setup

```sh
conda env create -f environment.yml          # first time only
conda activate fbrglm-dev
```

`scripts/00_install_local_fbrglm.R` installs the local fbrglm from
`/home/koki/dev/fbrglm`. It prefers `pak`, falls back to `devtools`,
and falls back again to base `install.packages(repos = NULL, type = "source")`
when the others can't satisfy a binary dependency.

## Smoke tests

`scripts/run_all_smoke.R` sources, in order:

| # | script                                           | what it checks |
|---|--------------------------------------------------|----------------|
| 0 | `00_install_local_fbrglm.R`                      | install local fbrglm |
| 1 | `01_smoke_basic.R`                               | gaussian / binomial / poisson basics |
| 2 | `02_against_glmnet_fixed_lambda.R`               | `lambda = "fix"` matches direct `glmnet` (max abs diff < 1e-6) |
| 3 | `03_predict_factor_newdata.R`                    | `predict()` survives missing factor levels in newdata |
| 4 | `04_missing_report.R`                            | `nobs_info$n_total / n_dropped_missing / n_used` are right |
| 5 | `05_lambda_cv_equivalence.R`                     | `lambda = "cv_min" / "cv_1se"` match `cv.glmnet` with fixed `foldid` |

```sh
Rscript scripts/run_all_smoke.R
```

## Small benchmarks

`scripts/run_all_benchmarks_small.R` runs:

- `scripts/10_benchmark_prediction_failures.R` — predict path resilience
  to a train/test factor-level mismatch.
- `scripts/11_benchmark_runtime_small.R` — wall-clock for fit+predict on
  a small binomial dataset.

```sh
Rscript scripts/run_all_benchmarks_small.R
Rscript scripts/20_plot_small_benchmarks.R
```

Outputs land under:

- `results/summary/prediction_failures_small.csv`
- `results/summary/runtime_small.csv`
- `results/figures/prediction_failures_small.png`
- `results/figures/runtime_small.png`

### Prediction-failure benchmark

Train has factor `g` with levels `A/B/C/D`; test has `g` with **levels
narrowed to `A/B`** (the factor object itself has only two levels, not
just two observed values). This is the canonical setup where a naive
`glmnet` caller gets bitten.

| method               | success | train_ncol | test_ncol | note |
|----------------------|---------|-----------:|----------:|------|
| `fbrglm`             | ✅      | 5          | 5         | auto: `xlevels` stored on the fit object |
| `glmnet_raw_naive`   | ❌      | 5          | 3         | manual: `model.matrix(train)` vs `model.matrix(test)` built separately |
| `glmnet_raw_safe`    | ✅      | 5          | 5         | manual: `relevel(test$g, levels = levels(train$g))` before `model.matrix` |
| `glmnetUtils`        | ❌      | 6          | NA        | formula interface, but failed under narrowed test factor levels |
| `parsnip_workflow`   | ✅      | NA         | NA        | tidymodels workflow with the `glmnet` engine; hardhat/recipes restore xlevels |

Failure rows produce a `glmnet` runtime error of the form
`The number of variables in newx must be N`.

![prediction-failure figure](results/figures/prediction_failures_small.png)

The takeaway: fbrglm's `predict()` and parsnip's workflow path both
handle narrowed test factors automatically because each stores the
training-time xlevels on the fit. `glmnet_raw_safe` works as well, but
moves the bookkeeping to the caller. `glmnet_raw_naive` and
`glmnetUtils` fail — the friction fbrglm is meant to remove.

### Runtime benchmark

Small binomial dataset (n = 200, p = 5, single λ = 0.05), median of 5
iterations from `bench::mark()`.

| method             | median runtime |
|--------------------|----------------|
| `fbrglm`           | ~ 4.9 ms       |
| `glmnet_raw`       | ~ 2.7 ms       |
| `glmnetUtils`      | ~ 4.1 ms       |
| `parsnip_workflow` | ~ 15.6 ms      |

fbrglm pays a small constant cost over raw `glmnet` for its formula /
factor / xlevels bookkeeping and lands in the same neighbourhood as
`glmnetUtils`. The tidymodels workflow path is the slowest of the four
— ~3× fbrglm — because every fit goes through the hardhat /
workflows abstraction layer. The overhead is what buys you the
prediction-path robustness above.

![runtime figure](results/figures/runtime_small.png)

Numbers are illustrative — the dataset is intentionally tiny, and a
larger benchmark suite will follow.

## Layout

```
R/                          # shared helpers
  data_generators.R         #   data factories
  benchmark_helpers.R       #   safe_run(), save_result_csv()
scripts/
  00_install_local_fbrglm.R
  01_smoke_basic.R
  02_against_glmnet_fixed_lambda.R
  03_predict_factor_newdata.R
  04_missing_report.R
  05_lambda_cv_equivalence.R
  10_benchmark_prediction_failures.R
  11_benchmark_runtime_small.R
  20_plot_small_benchmarks.R
  run_all_smoke.R
  run_all_benchmarks_small.R
results/
  summary/                  # CSVs (tracked in git)
  figures/                  # PNGs   (tracked in git)
  raw/                      # raw per-run artifacts (currently empty)
environment.yml             # conda env spec for fbrglm-dev
```
