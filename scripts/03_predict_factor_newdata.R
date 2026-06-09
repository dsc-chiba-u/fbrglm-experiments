## 03_predict_factor_newdata.R
## predict() must accept newdata whose factor is missing some training levels
## (e.g. train has A/B/C, test has only A/B).

suppressPackageStartupMessages({
    library(fbrglm)
})

set.seed(303)

n_train <- 120
train <- data.frame(
    y  = rbinom(n_train, 1, 0.5),
    x1 = rnorm(n_train),
    g  = factor(sample(c("A", "B", "C"), n_train, replace = TRUE),
                levels = c("A", "B", "C"))
)
stopifnot(all(c("A", "B", "C") %in% levels(train$g)))

fit <- fbrglm(y ~ x1 + g, data = train, family = "binomial",
              lambda = "fix", lambda_value = 0.05)

n_test <- 10
test <- data.frame(
    x1 = rnorm(n_test),
    g  = factor(rep(c("A", "B"), length.out = n_test),
                levels = c("A", "B", "C"))
)
stopifnot(!"C" %in% as.character(test$g))

pr <- predict(fit, newdata = test, type = "response")
stopifnot(length(pr) == nrow(test))
stopifnot(all(is.finite(pr)))
stopifnot(all(pr >= 0 & pr <= 1))

message("[03] predict_factor_newdata: OK")
