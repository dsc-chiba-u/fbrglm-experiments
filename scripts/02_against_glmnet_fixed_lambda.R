## 02_against_glmnet_fixed_lambda.R
## At a fixed lambda, fbrglm should reproduce glmnet's predictions exactly.

suppressPackageStartupMessages({
    library(fbrglm)
    library(glmnet)
})

set.seed(202)
n <- 120
p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", seq_len(p))
eta <- 0.5 * X[, 1] - 0.3 * X[, 2]
y <- rbinom(n, 1, plogis(eta))

df <- data.frame(y = y, X)
lam <- 0.05

fit_fbr <- fbrglm(y ~ x1 + x2 + x3 + x4 + x5, data = df,
                  family = "binomial",
                  lambda = "fix", lambda_value = lam)
fit_gln <- glmnet(X, y, family = "binomial",
                  alpha = 1, lambda = lam)

pr_fbr <- predict(fit_fbr, type = "response")
pr_gln <- as.vector(predict(fit_gln, newx = X, s = lam,
                            type = "response"))

max_abs_diff <- max(abs(pr_fbr - pr_gln))
message(sprintf("[02] max abs diff (response) = %.3e", max_abs_diff))
stopifnot(max_abs_diff < 1e-6)

## Sanity-check link too.
lk_fbr <- predict(fit_fbr, type = "link")
lk_gln <- as.vector(predict(fit_gln, newx = X, s = lam, type = "link"))
stopifnot(max(abs(lk_fbr - lk_gln)) < 1e-6)

message("[02] against_glmnet_fixed_lambda: OK")
