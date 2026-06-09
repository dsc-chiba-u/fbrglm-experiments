## Install fbrglm for the experiments pipeline.
##
## Resolution order:
##   1. If FBRGLM_PKG_ROOT is set and points at a checkout with a
##      DESCRIPTION file, install from that local source.
##   2. Otherwise install the released GitHub version
##      `dsc-chiba-u/fbrglm`. pak is tried first, then remotes, then
##      devtools::install_github(). Each fallback is used only when the
##      one above is unavailable or errors out (e.g. when the package
##      cache contains a binary `cli` incompatible with the system
##      glibc).
##   3. Last resort: base `install.packages(repos = NULL,
##      type = "source")` against the local checkout, when one is
##      provided.
##
## Reviewers reproducing the manuscript do not need to clone fbrglm
## separately: option 2 fetches it from GitHub.

github_slug <- "dsc-chiba-u/fbrglm"
pkg_root    <- Sys.getenv("FBRGLM_PKG_ROOT")
has_local   <- nzchar(pkg_root) &&
               file.exists(file.path(pkg_root, "DESCRIPTION"))

try_pak_local <- function() {
    if (!has_local || !requireNamespace("pak", quietly = TRUE)) return(FALSE)
    message("[00] pak::pkg_install local from ", pkg_root)
    tryCatch({
        pak::pkg_install(pkg_root, ask = FALSE); TRUE
    }, error = function(e) {
        message("[00] pak (local) failed: ", conditionMessage(e)); FALSE
    })
}
try_pak_github <- function() {
    if (!requireNamespace("pak", quietly = TRUE)) return(FALSE)
    message("[00] pak::pkg_install github::", github_slug)
    tryCatch({
        pak::pkg_install(paste0("github::", github_slug), ask = FALSE); TRUE
    }, error = function(e) {
        message("[00] pak (github) failed: ", conditionMessage(e)); FALSE
    })
}
try_remotes_github <- function() {
    if (!requireNamespace("remotes", quietly = TRUE)) return(FALSE)
    message("[00] remotes::install_github(\"", github_slug, "\")")
    tryCatch({
        remotes::install_github(github_slug, upgrade = "never",
                                quiet = TRUE); TRUE
    }, error = function(e) {
        message("[00] remotes failed: ", conditionMessage(e)); FALSE
    })
}
try_devtools_github <- function() {
    if (!requireNamespace("devtools", quietly = TRUE)) return(FALSE)
    message("[00] devtools::install_github(\"", github_slug, "\")")
    tryCatch({
        devtools::install_github(github_slug, upgrade = FALSE,
                                 quiet = TRUE); TRUE
    }, error = function(e) {
        message("[00] devtools (github) failed: ", conditionMessage(e)); FALSE
    })
}
try_devtools_local <- function() {
    if (!has_local || !requireNamespace("devtools", quietly = TRUE)) return(FALSE)
    message("[00] devtools::install local from ", pkg_root)
    tryCatch({
        devtools::install(pkg_root, upgrade = FALSE, quiet = TRUE); TRUE
    }, error = function(e) {
        message("[00] devtools (local) failed: ", conditionMessage(e)); FALSE
    })
}
try_base_install_local <- function() {
    if (!has_local) return(FALSE)
    message("[00] install.packages(repos = NULL, type = 'source') from ", pkg_root)
    tryCatch({
        utils::install.packages(pkg_root, repos = NULL, type = "source",
                                quiet = TRUE); TRUE
    }, error = function(e) {
        message("[00] base install failed: ", conditionMessage(e)); FALSE
    })
}

installed <- (has_local && try_pak_local()) ||
             try_pak_github() ||
             try_remotes_github() ||
             try_devtools_github() ||
             (has_local && try_devtools_local()) ||
             (has_local && try_base_install_local())

if (!installed) {
    stop("All install paths failed for ", github_slug,
         ". Try setting FBRGLM_PKG_ROOT to a local checkout, or ",
         "install one of `pak`, `remotes`, or `devtools` first.",
         call. = FALSE)
}

suppressPackageStartupMessages(library(fbrglm))
message("[00] fbrglm version: ", as.character(utils::packageVersion("fbrglm")))
message("[00] OK")
