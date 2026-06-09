## 05_lambda_cv_equivalence.R
## With foldid fixed, cv.glmnet and fbrglm(lambda="cv_min"/"cv_1se") must
## land on the same lambda.

suppressPackageStartupMessages({
    library(fbrglm)
    library(glmnet)
})

set.seed(505)
n <- 200
p <- 6
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", seq_len(p))
eta <- 0.5 * X[, 1] - 0.3 * X[, 3]
y   <- rnorm(n, mean = eta)

## Fix the fold assignment so both CV runs see identical splits.
set.seed(606)
foldid <- sample(rep(seq_len(5), length.out = n))

df <- data.frame(y = y, X)
fml <- y ~ x1 + x2 + x3 + x4 + x5 + x6

fit_min <- fbrglm(fml, data = df, family = "gaussian",
                  lambda = "cv_min", foldid = foldid)
fit_1se <- fbrglm(fml, data = df, family = "gaussian",
                  lambda = "cv_1se", foldid = foldid)
cv <- cv.glmnet(X, y, family = "gaussian", alpha = 1, foldid = foldid)

message(sprintf("[05] cv_min  fbrglm=%.6g  glmnet=%.6g",
                fit_min$lambda_value, cv$lambda.min))
message(sprintf("[05] cv_1se  fbrglm=%.6g  glmnet=%.6g",
                fit_1se$lambda_value, cv$lambda.1se))

stopifnot(isTRUE(all.equal(fit_min$lambda_value, cv$lambda.min)))
stopifnot(isTRUE(all.equal(fit_1se$lambda_value, cv$lambda.1se)))

message("[05] lambda_cv_equivalence: OK")
