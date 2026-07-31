#' Construct a standardized model-fit result
#'
#' The model-result schema is the project's WP3 contract: every fit --
#' baseline or copula -- records its engine, versions, seed, convergence
#' status, warnings, and timing, so that no nonconverged or unprovenanced
#' fit can slip silently into a model comparison.
#'
#' @param model Model identifier (e.g. `"rasch"`, `"2pl"`, `"grm"`).
#' @param engine Fitting engine (e.g. `"mirt"`).
#' @param engine_version Engine package version, as a string.
#' @param data_description List describing the data: at minimum `source`
#'   (`"simulation"` or `"matrix"`), plus generator metadata when simulated.
#' @param seed The generating/fitting seed (`NA` when unknown, e.g. for a
#'   raw matrix of unknown provenance).
#' @param converged Logical scalar from the engine's convergence diagnostic.
#' @param log_likelihood Fitted log-likelihood. Must be negative -- item
#'   response likelihoods are products of probabilities.
#' @param n_parameters Number of estimated parameters.
#' @param estimates Data frame of item-parameter estimates.
#' @param warnings Character vector of warnings emitted during fitting.
#' @param runtime_seconds Wall-clock fitting time in seconds.
#' @return An object of class `irtc_model_result` (schema version 1), with
#'   `fitted_at` stamped in UTC.
#' @export
new_model_result <- function(model,
                             engine,
                             engine_version,
                             data_description,
                             seed,
                             converged,
                             log_likelihood,
                             n_parameters,
                             estimates,
                             warnings = character(0),
                             runtime_seconds) {
  check_string(model, "model")
  check_string(engine, "engine")
  check_string(engine_version, "engine_version")
  if (!is.list(data_description)) {
    stop("`data_description` must be a list.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1) {
    stop("`seed` must be a single number (NA allowed for unknown provenance).", call. = FALSE)
  }
  if (!is.logical(converged) || length(converged) != 1 || is.na(converged)) {
    stop("`converged` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(log_likelihood) || length(log_likelihood) != 1 ||
      is.na(log_likelihood) || log_likelihood >= 0) {
    stop("`log_likelihood` must be a single negative number.", call. = FALSE)
  }
  if (!is.numeric(n_parameters) || length(n_parameters) != 1 ||
      is.na(n_parameters) || n_parameters < 1) {
    stop("`n_parameters` must be a positive count.", call. = FALSE)
  }
  if (!is.data.frame(estimates)) {
    stop("`estimates` must be a data frame.", call. = FALSE)
  }
  if (!is.character(warnings)) {
    stop("`warnings` must be a character vector.", call. = FALSE)
  }
  if (!is.numeric(runtime_seconds) || length(runtime_seconds) != 1 ||
      is.na(runtime_seconds) || runtime_seconds < 0) {
    stop("`runtime_seconds` must be a single non-negative number.", call. = FALSE)
  }

  structure(
    list(
      schema_version = 1L,
      model = model,
      engine = engine,
      engine_version = engine_version,
      data_description = data_description,
      seed = as.numeric(seed),
      converged = converged,
      log_likelihood = as.numeric(log_likelihood),
      n_parameters = as.integer(n_parameters),
      estimates = estimates,
      warnings = warnings,
      runtime_seconds = as.numeric(runtime_seconds),
      fitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    class = "irtc_model_result"
  )
}

#' Serialize a model result to JSON
#'
#' @param result An `irtc_model_result`.
#' @return A JSON string.
#' @export
model_result_to_json <- function(result) {
  if (!inherits(result, "irtc_model_result")) {
    stop("`result` must be an irtc_model_result.", call. = FALSE)
  }
  jsonlite::toJSON(unclass(result), auto_unbox = TRUE, digits = NA, na = "null")
}

#' Rebuild a model result from its JSON serialization
#'
#' @param json A JSON string produced by [model_result_to_json()].
#' @return An `irtc_model_result`.
#' @export
model_result_from_json <- function(json) {
  x <- jsonlite::fromJSON(json)
  new_model_result(
    model = x$model,
    engine = x$engine,
    engine_version = x$engine_version,
    data_description = x$data_description,
    seed = if (is.null(x$seed)) NA_real_ else x$seed,
    converged = x$converged,
    log_likelihood = x$log_likelihood,
    n_parameters = x$n_parameters,
    estimates = as.data.frame(x$estimates),
    warnings = as.character(unlist(x$warnings)),
    runtime_seconds = x$runtime_seconds
  )
}

#' @export
print.irtc_model_result <- function(x, ...) {
  status <- if (isTRUE(x$converged)) "converged" else "NOT CONVERGED"
  cat(sprintf(
    "<irtc_model_result v%d> %s via %s %s -- %s\n",
    x$schema_version, x$model, x$engine, x$engine_version, status
  ))
  cat(sprintf(
    "  logLik %.2f | %d parameters | %d items | %.2fs | fitted %s\n",
    x$log_likelihood, x$n_parameters, nrow(x$estimates), x$runtime_seconds, x$fitted_at
  ))
  if (length(x$warnings)) {
    cat("  warnings:", paste(x$warnings, collapse = " | "), "\n")
  }
  invisible(x)
}

check_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a non-empty string.", name), call. = FALSE)
  }
  invisible(TRUE)
}
