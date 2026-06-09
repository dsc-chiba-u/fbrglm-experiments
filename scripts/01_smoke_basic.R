## 01_smoke_basic.R
## fbrglm() works for gaussian / binomial / poisson on small data.

suppressPackageStartupMessages({
    library(fbrglm)
})

set.seed(101)
n <- 80

## gaussian -------------------------------------------------------------
df_g <- data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n))
fit_g <- fbrglm(y ~ x1 + x2, data = df_g, family = "gaussian",
                lambda = "fix", lambda_value = 0.05)
pr_g <- predict(fit_g, type = "response")
stopifnot(length(pr_g) == n, all(is.finite(pr_g)))

## binomial -------------------------------------------------------------
df_b <- data.frame(y = rbinom(n, 1, 0.5),
                   x1 = rnorm(n), x2 = rnorm(n))
fit_b <- fbrglm(y ~ x1 + x2, data = df_b, family = "binomial",
                lambda = "fix", lambda_value = 0.05)
pr_b <- predict(fit_b, type = "response")
stopifnot(length(pr_b) == n, all(is.finite(pr_b)))
stopifnot(all(pr_b >= 0 & pr_b <= 1))

cls_b <- predict(fit_b, type = "class")
stopifnot(length(cls_b) == n, all(cls_b %in% c(0, 1)))

## poisson --------------------------------------------------------------
df_p <- data.frame(y = rpois(n, 2),
                   x1 = rnorm(n), x2 = rnorm(n))
fit_p <- fbrglm(y ~ x1 + x2, data = df_p, family = "poisson",
                lambda = "fix", lambda_value = 0.05)
pr_p <- predict(fit_p, type = "response")
stopifnot(length(pr_p) == n, all(is.finite(pr_p)), all(pr_p > 0))

message("[01] smoke_basic: OK")
