#' Held-out marginal log-loss at fixed parameters
#'
#' The M4 primary metric: the marginal log-likelihood of unseen persons'
#' response vectors under a fitted model's (training-time) parameters, with
#' the latent variable integrated out by quadrature, reported per response.
#'
#' Two paths, one per model class:
#' \itemize{
#'   \item Binary marginal-IRT results (engines `mirt` -- models `2pl`,
#'     `rasch` -- and `irtc`): standard-normal ability prior, item response
#'     `plogis(a (theta - b))`.
#'   \item One-factor copula results (engine `FactorCopula`): uniform latent
#'     factor, category probabilities reconstructed from the stored
#'     first-stage cutpoints and per-item copula h-functions
#'     (`bvn`, `frk`, `gum`, `rgum`, `joe`, `rjoe`).
#' }
#'
#' Correctness contract (enforced in tests): evaluated on the training data
#' at the fitted parameters, the result reproduces the engine's own
#' log-likelihood. Missing responses contribute nothing (skipped, never
#' imputed). GRM results are not yet supported.
#'
#' @param result An `irtc_model_result` from [fit_baseline()],
#'   [fit_irtc()], or [fit_copula_1f()].
#' @param newdata A response matrix (or `irtc_simulation`) with the same
#'   items, in the same order and coding, as the training data.
#' @param nq Quadrature points (default 61; use the fitting `nq` to
#'   reproduce a training likelihood exactly).
#' @return A list of class `irtc_heldout`: `total_loglik`, `n_persons`,
#'   `n_responses`, and `logloss_per_response` (negative mean).
#' @family evaluation
#' @export
heldout_logloss <- function(result, newdata, nq = 61) {
  if (!inherits(result, "irtc_model_result")) {
    stop("`result` must be an irtc_model_result.", call. = FALSE)
  }
  responses <- if (inherits(newdata, "irtc_simulation")) newdata$responses else newdata
  if (!is.matrix(responses)) {
    stop("`newdata` must be a response matrix or irtc_simulation.", call. = FALSE)
  }
  if (ncol(responses) != nrow(result$estimates)) {
    stop(sprintf(
      "`newdata` has %d items but the fitted model has %d items.",
      ncol(responses), nrow(result$estimates)
    ), call. = FALSE)
  }

  if (result$engine %in% c("mirt", "irtc") &&
      result$model %in% c("2pl", "rasch", "irtc-2pl-independence")) {
    total <- marginal_loglik_2pl(result$estimates, responses, nq)
  } else if (grepl("^copula-1f-", result$model)) {
    # Both copula engines (FactorCopula wrapped, irtc native) store the same
    # cutpoints + families + thetas; one evaluation path serves both.
    total <- marginal_loglik_copula_1f(result$estimates, responses, nq)
  } else {
    stop(sprintf("Model `%s` (engine %s) is not yet supported by heldout_logloss.",
                 result$model, result$engine), call. = FALSE)
  }

  n_responses <- sum(!is.na(responses))
  structure(
    list(
      total_loglik = total,
      n_persons = nrow(responses),
      n_responses = n_responses,
      logloss_per_response = -total / n_responses
    ),
    class = "irtc_heldout"
  )
}

#' @export
print.irtc_heldout <- function(x, ...) {
  cat(sprintf(
    "<irtc_heldout> logLik %.2f over %d persons / %d responses | log-loss per response %.5f\n",
    x$total_loglik, x$n_persons, x$n_responses, x$logloss_per_response
  ))
  invisible(x)
}

marginal_loglik_2pl <- function(estimates, responses, nq) {
  gq <- statmod::gauss.quad.prob(nq, dist = "normal", mu = 0, sigma = 1)
  a <- estimates$a
  b <- estimates$b
  eta <- outer(gq$nodes, b, `-`) * rep(a, each = nq)
  # Clamp away exact 0/1: extreme item estimates (e.g. a Heywood-ish
  # discrimination) otherwise produce log(0) = -Inf and a NaN metric
  # (found by exp_2026_08_02_001's baseline). Mirrors the copula path's
  # pcat clamp.
  p <- pmin(pmax(stats::plogis(eta), 1e-300), 1 - 1e-16)    # Q x J
  y1 <- responses; y1[is.na(y1)] <- 0L                       # observed successes
  y0 <- 1L - responses; y0[is.na(y0)] <- 0L                  # observed failures
  ll_nq <- y1 %*% t(log(p)) + y0 %*% t(log1p(-p))            # N x Q
  m <- apply(ll_nq, 1, max)
  sum(m + log(exp(ll_nq - m) %*% gq$weights))
}

marginal_loglik_copula_1f <- function(estimates, responses, nq) {
  cut_cols <- grep("^cut[0-9]+$", names(estimates), value = TRUE)
  if (length(cut_cols) == 0) {
    stop("The copula result carries no stored cutpoints; refit with the current fit_copula_1f().",
         call. = FALSE)
  }
  gq <- statmod::gauss.quad.prob(nq)                         # uniform on (0, 1)
  responses <- responses - min(responses, na.rm = TRUE)      # 0-based, as in fitting

  n_items <- nrow(estimates)
  ll_nq <- matrix(0, nrow = nrow(responses), ncol = nq)      # N x Q accumulator
  for (j in seq_len(n_items)) {
    cuts <- as.numeric(estimates[j, cut_cols])
    cuts <- cuts[!is.na(cuts)]
    bounds <- c(0, cuts, 1)
    # h at each bound for every node: (K+1) x Q
    h <- vapply(
      gq$nodes,
      function(v) copula_h(bounds, v, estimates$family[[j]], estimates$theta[[j]]),
      numeric(length(bounds))
    )
    pcat <- pmax(h[-1, , drop = FALSE] - h[-nrow(h), , drop = FALSE], 1e-300)  # K x Q
    y <- responses[, j]
    obs <- !is.na(y)
    if (max(y[obs]) >= nrow(pcat)) {
      stop(sprintf("Item %d has category %d outside the fitted range.",
                   j, max(y[obs])), call. = FALSE)
    }
    ll_nq[obs, ] <- ll_nq[obs, ] + log(pcat[y[obs] + 1L, , drop = FALSE])
  }
  m <- apply(ll_nq, 1, max)
  sum(m + log(exp(ll_nq - m) %*% gq$weights))
}

# Conditional copula h-function C_{U|V}(u | v) = dC(u, v)/dv for the
# single-parameter families FactorCopula's one-factor model supports here.
# Rotated variants (rgum, rjoe) are the 180-degree survival rotations:
# h_180(u | v) = 1 - h(1 - u | 1 - v).
copula_h <- function(u, v, family, theta) {
  out <- switch(
    family,
    bvn = stats::pnorm((stats::qnorm(u) - theta * stats::qnorm(v)) / sqrt(1 - theta^2)),
    frk = if (abs(theta) < 1e-8) {
      # Frank's independence limit: the native optimizer may cross theta = 0,
      # where the closed form is 0/0.
      u
    } else {
      em <- expm1(-theta)
      eu <- expm1(-theta * u)
      exp(-theta * v) * eu / (em + eu * expm1(-theta * v))
    },
    gum = h_gumbel(u, v, theta),
    joe = h_joe(u, v, theta),
    rgum = 1 - h_gumbel(1 - u, 1 - v, theta),
    rjoe = 1 - h_joe(1 - u, 1 - v, theta),
    stop(sprintf("Copula family `%s` is not supported by heldout_logloss.", family),
         call. = FALSE)
  )
  # Exact boundaries regardless of family numerics.
  out[u <= 0] <- 0
  out[u >= 1] <- 1
  out
}

h_gumbel <- function(u, v, theta) {
  lu <- -log(pmin(pmax(u, 1e-300), 1))
  lv <- -log(pmin(pmax(v, 1e-300), 1))
  s <- lu^theta + lv^theta
  exp(-s^(1 / theta)) * s^(1 / theta - 1) * lv^(theta - 1) / v
}

h_joe <- function(u, v, theta) {
  xu <- (1 - u)^theta
  xv <- (1 - v)^theta
  s <- xu + xv - xu * xv
  (1 - v)^(theta - 1) * (1 - xu) * s^(1 / theta - 1)
}
