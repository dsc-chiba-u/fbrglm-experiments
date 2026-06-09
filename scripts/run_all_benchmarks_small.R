## run_all_benchmarks_small.R
## Run the small-scale benchmarks in order. Each script is responsible
## for its own outputs under results/.

scripts <- c(
    "scripts/10_benchmark_prediction_failures.R",
    "scripts/11_benchmark_runtime_small.R"
)

for (s in scripts) {
    message("\n=== running ", s, " ===")
    tryCatch(
        source(s, local = TRUE, echo = FALSE),
        error = function(e) {
            message("!!! FAILED at ", s)
            message("    error: ", conditionMessage(e))
            stop(e)
        }
    )
}

message("\n=== All small benchmarks completed ===")
