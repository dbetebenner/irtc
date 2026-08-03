#' Fit a one-factor copula IRT model
#'
#' Level-2 of the charter's model ladder: one-factor copula fits for
#' item-response data, returning the standardized [new_model_result()]
#' object. Estimation is the two-stage procedure in both engines:
#' empirical cutpoints, then dependence parameters by quadrature MLE.
#'
#' Two engines maximize the same likelihood from the same first-stage
#' cutpoints:
#' \itemize{
#'   \item `"FactorCopula"` (default) wraps `FactorCopula::mle1factor()`.
#'     Its likelihood builds a joint contingency table over all response
#'     patterns -- O(2^J) for binary items -- and fails for J >= ~40
#'     (ledger C0006).
#'   \item `"irtc"` is the native code path: the person-wise marginal
#'     likelihood assembled from per-item copula h-functions (the same
#'     functions [heldout_logloss()] uses, verified to 1e-6 against
#'     `mle1factor`), maximized by [stats::nlminb()] with an item-local
#'     finite-difference gradient. No joint table exists at any point, so
#'     it scales to realistic test lengths.
#' }
#'
#' `mle1factor()` exposes no explicit convergence diagnostic, so with the
#' wrapped engine `converged` reports whether the log-likelihood and every
#' dependence parameter came back finite; the native engine reports the
#' optimizer's own convergence code. `n_parameters` counts the
#' second-stage dependence parameters only (first-stage cutpoints are
#' profiled empirically).
#'
#' @param data A response matrix (integer categories, 2+ observed levels)
#'   or an `irtc_simulation` from [simulate_grm()] / [simulate_2pl()].
#' @param family A single copula family name recycled across items, or one
#'   per item (FactorCopula names: `"bvn"`, `"frk"`, `"gum"`, `"rgum"`,
#'   `"joe"`, `"rjoe"`). Single-parameter families only.
#' @param nq Number of Gauss-Legendre quadrature points (default 25).
#' @param engine `"FactorCopula"` (default, wrapped) or `"irtc"` (native).
#' @return An `irtc_model_result` with `estimates` columns `item`,
#'   `family`, `theta` (dependence parameter), `tau` (Kendall's tau), and
#'   the first-stage cutpoints `cut1..cutM`.
#' @family model fitting
#' @export
fit_copula_1f <- function(data, family, nq = 25,
                          engine = c("FactorCopula", "irtc")) {
  engine <- match.arg(engine)
  if (!requireNamespace("statmod", quietly = TRUE) ||
      (engine == "FactorCopula" &&
       !requireNamespace("FactorCopula", quietly = TRUE))) {
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

  if (engine == "irtc") {
    return(fit_copula_1f_native(
      responses, family, nq,
      data_description = data_description, seed = seed
    ))
  }

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

  # Carry the first-stage cutpoints (cumulative category proportions on the
  # uniform scale; mle1factor pads its matrix with 1s) so held-out
  # evaluation can reconstruct the fitted likelihood exactly. Columns
  # cut1..cutM, NA-padded per item.
  cut_mat <- as.matrix(fit$cutpoints)
  cut_list <- lapply(seq_len(n_items), function(j) {
    v <- cut_mat[, j]
    v[v < 1 - 1e-12]
  })
  max_cuts <- max(vapply(cut_list, length, integer(1)))
  for (m in seq_len(max_cuts)) {
    estimates[[paste0("cut", m)]] <- vapply(
      cut_list,
      function(v) if (length(v) >= m) v[[m]] else NA_real_,
      numeric(1)
    )
  }

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
