## 04_missing_report.R
## Complete-case filtering surfaces through object$nobs_info.

suppressPackageStartupMessages({
    library(fbrglm)
})

set.seed(404)
n <- 50
df <- data.frame(y = rnorm(n),
                 x1 = rnorm(n),
                 x2 = rnorm(n))
df$y[1:5]      <- NA
df$x1[c(8, 9)] <- NA

n_total_expect   <- n
n_dropped_expect <- sum(!stats::complete.cases(df))
n_used_expect    <- n_total_expect - n_dropped_expect

fit <- suppressMessages(
    fbrglm(y ~ x1 + x2, data = df, family = "gaussian",
           lambda = "fix", lambda_value = 0.05)
)

stopifnot(is.list(fit$nobs_info))
stopifnot(fit$nobs_info$n_total           == n_total_expect)
stopifnot(fit$nobs_info$n_dropped_missing == n_dropped_expect)
stopifnot(fit$nobs_info$n_used            == n_used_expect)
stopifnot(nobs(fit)                       == n_used_expect)

message(sprintf("[04] n_total=%d  dropped=%d  used=%d",
                fit$nobs_info$n_total,
                fit$nobs_info$n_dropped_missing,
                fit$nobs_info$n_used))
message("[04] missing_report: OK")
