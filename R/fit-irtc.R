#' Fit the IRTc product-copula model (Level 1: independence copula)
#'
#' The Level-1 rung of the model ladder: IRTc's own code path, structured
#' as item-response marginals joined by a copula. At Level 1 the copula is
#' fixed to independence, so the conditional joint factorizes into the
#' product of 2PL marginals -- and the fit must reproduce the conventional
#' baseline. Estimation is marginal maximum likelihood with a standard
#' normal ability prior and Gauss-Hermite-type quadrature
#' (`statmod::gauss.quad.prob(dist = "normal")`), optimized by `nlminb()`
#' with discriminations log-parameterized for positivity.
#'
#' The acceptance test for this rung (charter, model ladder Level 1) lives
#' in `tests/testthat/test-irtc-level1.R`: parameter estimates,
#' log-likelihood, and predicted probabilities must match the mirt 2PL
#' within numerical tolerance on seeded data. Non-independence copulas are
#' deliberately refused here; they arrive with the Level-2+ likelihoods.
#'
#' @param data An `irtc_simulation` from [simulate_2pl()] or a binary
#'   response matrix.
#' @param copula Copula family. Only `"independence"` is implemented at
#'   Level 1.
#' @param nq Number of quadrature points for the normal ability prior
#'   (default 61).
#' @return An `irtc_model_result` with 2PL `estimates` (`item`, `a`, `b`).
#' @export
fit_irtc <- function(data, copula = "independence", nq = 61) {
  if (!requireNamespace("statmod", quietly = TRUE)) {
    stop("Package `statmod` is required for quadrature.", call. = FALSE)
  }
  if (!identical(copula, "independence")) {
    stop(
      "Only the independence copula is implemented at Level 1; ",
      "structured copulas arrive with the Level-2+ likelihoods.",
      call. = FALSE
    )
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
    stop("`data` must be an irtc_simulation or a binary response matrix.", call. = FALSE)
  }

  values <- unique(stats::na.omit(as.vector(responses)))
  if (!all(values %in% c(0, 1))) {
    stop("Level-1 fit_irtc requires binary (0/1) responses; the data are not binary.", call. = FALSE)
  }

  n_items <- ncol(responses)
  gq <- statmod::gauss.quad.prob(nq, dist = "normal", mu = 0, sigma = 1)
  nodes <- gq$nodes
  weights <- gq$weights

  negloglik <- function(par) {
    a <- exp(par[seq_len(n_items)])
    b <- par[n_items + seq_len(n_items)]
    eta <- outer(nodes, b, `-`) * rep(a, each = length(nodes))
    p <- stats::plogis(eta)
    # Independence copula: the conditional joint is the product of the
    # marginals, so the per-node log-likelihood is a sum over items.
    ll_nq <- responses %*% t(log(p)) + (1 - responses) %*% t(log1p(-p))
    m <- apply(ll_nq, 1, max)
    value <- -sum(m + log(exp(ll_nq - m) %*% weights))
    if (!is.finite(value)) {
      return(.Machine$double.xmax / 2)
    }
    value
  }

  captured_warnings <- character(0)
  started <- proc.time()[["elapsed"]]
  opt <- withCallingHandlers(
    stats::nlminb(
      start = c(rep(0, n_items), rep(0, n_items)),
      objective = negloglik,
      control = list(iter.max = 500, eval.max = 1000)
    ),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  runtime <- proc.time()[["elapsed"]] - started

  item_names <- colnames(responses)
  if (is.null(item_names)) {
    item_names <- paste0("I", seq_len(n_items))
  }
  estimates <- data.frame(
    item = item_names,
    a = exp(opt$par[seq_len(n_items)]),
    b = opt$par[n_items + seq_len(n_items)]
  )

  new_model_result(
    model = "irtc-2pl-independence",
    engine = "irtc",
    engine_version = as.character(utils::packageVersion("irtc")),
    data_description = data_description,
    seed = seed,
    converged = opt$convergence == 0,
    log_likelihood = -opt$objective,
    n_parameters = 2L * n_items,
    estimates = estimates,
    warnings = captured_warnings,
    runtime_seconds = runtime
  )
}
