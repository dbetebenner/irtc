#' Fit a Level-0 baseline IRT model via mirt
#'
#' Wraps `mirt::mirt()` for the charter's Level-0 baselines (Rasch, 2PL,
#' GRM) and returns the standardized [new_model_result()] object with full
#' provenance, convergence status, captured warnings, and timing.
#'
#' @param data An `irtc_simulation` (from [simulate_2pl()] /
#'   [simulate_grm()]) or a raw response matrix. Binary models (`"rasch"`,
#'   `"2pl"`) require 0/1 responses; `"grm"` requires ordinal responses with
#'   3+ observed categories.
#' @param model One of `"rasch"`, `"2pl"`, `"grm"`.
#' @return An `irtc_model_result`. Item-parameter `estimates` use the IRT
#'   parameterization (`a`, `b` for binary models; `a`, `b1...` thresholds
#'   for the GRM).
#' @family model fitting
#' @export
fit_baseline <- function(data, model = c("rasch", "2pl", "grm")) {
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop("Package `mirt` is required for baseline fits.", call. = FALSE)
  }
  model <- match.arg(model)

  if (inherits(data, "irtc_simulation")) {
    responses <- data$responses
    data_description <- list(source = "simulation", generator = data$generator)
    seed <- data$generator$seed
  } else if (is.matrix(data)) {
    responses <- data
    data_description <- list(
      source = "matrix",
      n_persons = nrow(data),
      n_items = ncol(data)
    )
    seed <- NA_real_
  } else {
    stop("`data` must be an irtc_simulation or a response matrix.", call. = FALSE)
  }

  n_observed_categories <- length(unique(stats::na.omit(as.vector(responses))))
  if (model %in% c("rasch", "2pl") && n_observed_categories > 2) {
    stop(sprintf("`%s` requires binary responses; the data look ordinal.", model), call. = FALSE)
  }
  if (model == "grm" && n_observed_categories <= 2) {
    stop("`grm` requires ordinal responses with 3+ categories; the data look binary.", call. = FALSE)
  }

  itemtype <- switch(model, rasch = "Rasch", `2pl` = "2PL", grm = "graded")

  captured_warnings <- character(0)
  started <- proc.time()[["elapsed"]]
  fit <- withCallingHandlers(
    mirt::mirt(as.data.frame(responses), 1, itemtype = itemtype, verbose = FALSE),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  runtime <- proc.time()[["elapsed"]] - started

  items <- mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  keep <- intersect(c("a", grep("^b[0-9]*$", colnames(items), value = TRUE), "b"),
                    colnames(items))
  estimates <- data.frame(item = rownames(items), items[, keep, drop = FALSE],
                          row.names = NULL)

  new_model_result(
    model = model,
    engine = "mirt",
    engine_version = as.character(utils::packageVersion("mirt")),
    data_description = data_description,
    seed = seed,
    converged = isTRUE(mirt::extract.mirt(fit, "converged")),
    log_likelihood = mirt::extract.mirt(fit, "logLik"),
    n_parameters = mirt::extract.mirt(fit, "nest"),
    estimates = estimates,
    warnings = captured_warnings,
    runtime_seconds = runtime
  )
}
