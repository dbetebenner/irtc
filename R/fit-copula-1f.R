#' Fit a one-factor copula IRT model via FactorCopula
#'
#' Level-2 of the charter's model ladder: wraps
#' `FactorCopula::mle1factor()` for ordinal item-response data and returns
#' the standardized [new_model_result()] object. Estimation is the
#' package's two-stage procedure (empirical cutpoints, then copula
#' parameters by quadrature MLE).
#'
#' `mle1factor()` exposes no explicit convergence diagnostic, so
#' `converged` reports whether the log-likelihood and every dependence
#' parameter came back finite -- the strongest check available from the
#' engine. `n_parameters` counts the second-stage dependence parameters
#' only (first-stage cutpoints are profiled empirically).
#'
#' @param data An ordinal response matrix (integer categories, 3+ observed
#'   levels) or an `irtc_simulation` from [simulate_grm()].
#' @param family A single copula family name recycled across items, or one
#'   per item (FactorCopula names, e.g. `"gum"`, `"frk"`, `"joe"`,
#'   `"rjoe"`). Single-parameter families only.
#' @param nq Number of Gauss-Legendre quadrature points (default 25).
#' @return An `irtc_model_result` with `estimates` columns `item`,
#'   `family`, `theta` (dependence parameter), and `tau` (Kendall's tau).
#' @export
fit_copula_1f <- function(data, family, nq = 25) {
  if (!requireNamespace("FactorCopula", quietly = TRUE) ||
      !requireNamespace("statmod", quietly = TRUE)) {
    stop("Packages `FactorCopula` and `statmod` are required for copula fits.", call. = FALSE)
  }

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
    stop("`data` must be an irtc_simulation or an ordinal response matrix.", call. = FALSE)
  }

  # Binary AND ordinal categorical responses are supported: for dichotomous
  # items the factor copula uses a single cutpoint per item (the BVN family
  # recovers normal-ogive-style behavior). Verified on IRW binary data
  # (much_tte_2025_matrixreasoning, M3). Only degenerate data are refused.
  n_observed_categories <- length(unique(stats::na.omit(as.vector(responses))))
  if (n_observed_categories < 2) {
    stop("One-factor copula fits require categorical responses with 2+ observed categories.", call. = FALSE)
  }

  n_items <- ncol(responses)
  if (length(family) == 1) {
    family <- rep(family, n_items)
  }
  if (length(family) != n_items) {
    stop(sprintf(
      "`family` must be length 1 or one per item (%d), got %d.",
      n_items, length(family)
    ), call. = FALSE)
  }

  # FactorCopula's own simulations code ordinal categories from 0.
  responses <- responses - min(responses, na.rm = TRUE)

  gl <- statmod::gauss.quad.prob(nq)
  captured_warnings <- character(0)
  started <- proc.time()[["elapsed"]]
  fit <- withCallingHandlers(
    FactorCopula::mle1factor(
      continuous = NULL,
      ordinal = as.data.frame(responses),
      count = NULL,
      copF1 = family,
      gl
    ),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  runtime <- proc.time()[["elapsed"]] - started

  theta <- vapply(fit$cpar$f1, function(p) as.numeric(p[[1]]), numeric(1))
  item_names <- colnames(responses)
  if (is.null(item_names)) {
    item_names <- paste0("I", seq_len(n_items))
  }
  estimates <- data.frame(
    item = item_names,
    family = family,
    theta = theta,
    tau = as.numeric(fit$taus)
  )

  model <- if (length(unique(family)) == 1) {
    paste0("copula-1f-", family[[1]])
  } else {
    "copula-1f-mixed"
  }

  new_model_result(
    model = model,
    engine = "FactorCopula",
    engine_version = as.character(utils::packageVersion("FactorCopula")),
    data_description = data_description,
    seed = seed,
    converged = is.finite(fit$loglik) && all(is.finite(theta)),
    log_likelihood = fit$loglik,
    n_parameters = length(theta),
    estimates = estimates,
    warnings = captured_warnings,
    runtime_seconds = runtime
  )
}

#' Load a dataset shipped with FactorCopula without touching the global env
#'
#' @param name Dataset name (e.g. `"PE"`).
#' @return The dataset object.
#' @keywords internal
get_factorcopula_data <- function(name) {
  e <- new.env()
  utils::data(list = name, package = "FactorCopula", envir = e)
  get(name, envir = e)
}
