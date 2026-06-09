## Install the local fbrglm package from /home/koki/dev/fbrglm.
##
## Strategy: try pak first; fall back to devtools::install; fall back again
## to base install.packages(repos = NULL, type = "source"). pak/devtools
## sometimes pull a binary 'cli' that is incompatible with the system glibc
## on this machine, so the final fallback uses no extra tooling.

local_pkg <- "/home/koki/dev/fbrglm"

if (!dir.exists(local_pkg)) {
    stop("Local fbrglm package directory not found at ", local_pkg)
}

try_pak <- function() {
    if (!requireNamespace("pak", quietly = TRUE)) return(FALSE)
    message("[00] Trying pak::pkg_install")
    tryCatch({
        pak::pkg_install(local_pkg, ask = FALSE)
        TRUE
    }, error = function(e) {
        message("[00] pak install failed: ", conditionMessage(e))
        FALSE
    })
}

try_devtools <- function() {
    if (!requireNamespace("devtools", quietly = TRUE)) return(FALSE)
    message("[00] Trying devtools::install")
    tryCatch({
        devtools::install(local_pkg, upgrade = FALSE, quiet = TRUE)
        TRUE
    }, error = function(e) {
        message("[00] devtools install failed: ", conditionMessage(e))
        FALSE
    })
}

try_base_install <- function() {
    message("[00] Trying base install.packages(repos = NULL, type = 'source')")
    tryCatch({
        utils::install.packages(local_pkg, repos = NULL, type = "source",
                                quiet = TRUE)
        TRUE
    }, error = function(e) {
        message("[00] base install failed: ", conditionMessage(e))
        FALSE
    })
}

installed <- try_pak() || try_devtools() || try_base_install()
if (!installed) {
    stop("All install paths failed for ", local_pkg)
}

suppressPackageStartupMessages(library(fbrglm))
message("[00] fbrglm version: ", as.character(utils::packageVersion("fbrglm")))
message("[00] OK")
