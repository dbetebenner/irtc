#' {{rPackage.name}}
#'
#' R package supporting a dissertation provisioned via dissertation.ai.
#' Currently contains scaffolding only. Add your methodological R functions
#' to the `R/` directory and roxygen-document them; tests go in
#' `tests/testthat/`.
#'
#' The Quarto book at `ui/www/` is the thesis source. The
#' `build-thesis.yml` GitHub Action renders it to PDF on every push.
#'
#' @keywords internal
"_PACKAGE"

#' Package metadata (stub function)
#'
#' Replaced at provision time + extended by `dataimago::ai()` based on your
#' `dataimago-spec.yaml`.
#'
#' @return A list with package name + version.
#' @export
#' @examples
#' package_info()
package_info <- function() {
  list(
    name = utils::packageName(),
    version = as.character(utils::packageVersion(utils::packageName()))
  )
}
