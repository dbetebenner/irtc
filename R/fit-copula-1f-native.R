# The native Level-2 one-factor copula likelihood (IRTc's own code path).
#
# Model identical to FactorCopula::mle1factor's: uniform latent factor V,
# per-item ordinal margins from first-stage empirical cutpoints, per-item
# single-parameter copulas linking each item to V. The likelihood is the
# person-wise marginal
#
#   L_i(theta) = int_0^1 prod_j [ h_j(b_{j, y_ij + 1} | v) - h_j(b_{j, y_ij} | v) ] dv
#
# evaluated by Gauss-Legendre quadrature -- per-item h-function differences
# only, never a joint contingency table, so memory is O(N x Q) regardless of
# J (the O(2^J) wall is ledger C0006). The h-functions are the ones
# heldout_logloss() verifies against mle1factor to 1e-6 in tests.
#
# Optimization: stats::nlminb over unconstrained transforms of the J
# dependence parameters, with an item-local central-difference gradient --
# perturbing theta_j only changes item j's contribution to the N x Q
# log-likelihood accumulator, so a full gradient costs ~2 objective
# evaluations instead of 2J.

fit_copula_1f_native <- function(responses, family, nq,
                                 data_description, seed) {
  gq <- statmod::gauss.quad.prob(nq)
  n_items <- ncol(responses)

  # First stage: empirical cumulative category proportions per item, on the
  # uniform scale -- the same cutpoints mle1factor uses (asserted to 1e-8 in
  # the reproduction tests).
  cut_list <- lapply(seq_len(n_items), function(j) {
    y <- responses[, j]
    y <- y[!is.na(y)]
    k_max <- max(y)
    cuts <- cumsum(tabulate(y + 1L, nbins = k_max + 1L)) / length(y)
    cuts[-length(cuts)]
  })
  bounds_list <- lapply(cut_list, function(cuts) c(0, cuts, 1))

  # Precomputed per-person observation index for each item.
  obs_list <- lapply(seq_len(n_items), function(j) !is.na(responses[, j]))
  y_list <- lapply(seq_len(n_items), function(j) {
    responses[obs_list[[j]], j] + 1L
  })

  # Item j's N x Q log-probability contribution at dependence value theta.
  item_contrib <- function(j, theta) {
    h <- vapply(
      gq$nodes,
      function(v) copula_h(bounds_list[[j]], v, family[[j]], theta),
      numeric(length(bounds_list[[j]]))
    )
    pcat <- pmax(h[-1, , drop = FALSE] - h[-nrow(h), , drop = FALSE], 1e-300)
    out <- matrix(0, nrow = nrow(responses), ncol = nq)
    out[obs_list[[j]], ] <- log(pcat)[y_list[[j]], , drop = FALSE]
    out
  }

  loglik_from_accum <- function(ll_nq) {
    m <- apply(ll_nq, 1, max)
    sum(m + log(exp(ll_nq - m) %*% gq$weights))
  }

  theta_from_z <- function(z, fam) {
    switch(fam,
      bvn = tanh(z),
      frk = z,
      1 + exp(z)   # gum, rgum, joe, rjoe: theta >= 1
    )
  }
  z_start <- vapply(family, function(fam) {
    switch(fam, bvn = atanh(0.5), frk = 2, 0)   # gum-class: theta = 2
  }, numeric(1))

  contribs_at <- function(z) {
    lapply(seq_len(n_items), function(j) {
      item_contrib(j, theta_from_z(z[[j]], family[[j]]))
    })
  }
  objective <- function(z) {
    -loglik_from_accum(Reduce(`+`, contribs_at(z)))
  }
  gradient <- function(z) {
    contribs <- contribs_at(z)
    total <- Reduce(`+`, contribs)
    h_step <- 1e-5
    vapply(seq_len(n_items), function(j) {
      up <- total - contribs[[j]] +
        item_contrib(j, theta_from_z(z[[j]] + h_step, family[[j]]))
      dn <- total - contribs[[j]] +
        item_contrib(j, theta_from_z(z[[j]] - h_step, family[[j]]))
      (loglik_from_accum(dn) - loglik_from_accum(up)) / (2 * h_step)
    }, numeric(1))
  }

  started <- proc.time()[["elapsed"]]
  opt <- stats::nlminb(
    z_start, objective, gradient = gradient,
    control = list(iter.max = 500, eval.max = 1000)
  )
  runtime <- proc.time()[["elapsed"]] - started

  theta <- vapply(seq_len(n_items), function(j) {
    theta_from_z(opt$par[[j]], family[[j]])
  }, numeric(1))
  loglik <- -opt$objective

  item_names <- colnames(responses)
  if (is.null(item_names)) {
    item_names <- paste0("I", seq_len(n_items))
  }
  estimates <- data.frame(
    item = item_names,
    family = family,
    theta = theta,
    tau = vapply(seq_len(n_items), function(j) {
      copula_tau(family[[j]], theta[[j]])
    }, numeric(1))
  )
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
    engine = "irtc",
    engine_version = as.character(utils::packageVersion("irtc")),
    data_description = data_description,
    seed = seed,
    converged = opt$convergence == 0 && is.finite(loglik) &&
      all(is.finite(theta)),
    log_likelihood = loglik,
    n_parameters = n_items,
    estimates = estimates,
    warnings = if (opt$convergence == 0) character(0) else opt$message,
    runtime_seconds = runtime
  )
}

# Kendall's tau for the supported single-parameter families. The 180-degree
# survival rotations preserve tau. Closed forms where they exist; the Frank
# Debye integral and the Joe series otherwise. Verified against
# FactorCopula's tau output in the reproduction tests.
copula_tau <- function(family, theta) {
  switch(
    family,
    bvn = 2 / pi * asin(theta),
    frk = {
      if (abs(theta) < 1e-8) return(0)
      debye1 <- stats::integrate(
        function(t) t / expm1(t), 0, theta
      )$value / theta
      1 - 4 / theta * (1 - debye1)
    },
    gum = ,
    rgum = 1 - 1 / theta,
    joe = ,
    rjoe = {
      k <- seq_len(20000)
      1 - 4 * sum(1 / (k * (theta * k + 2) * (theta * (k - 1) + 2)))
    },
    stop(sprintf("Copula family `%s` has no tau formula here.", family),
         call. = FALSE)
  )
}
