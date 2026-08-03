# The native Level-2 likelihood (charter model ladder, Level 2 proper):
# fit_copula_1f(engine = "irtc") estimates the one-factor copula model with
# IRTc's own code path -- the same two-stage procedure as
# FactorCopula::mle1factor (empirical cutpoints, then dependence parameters
# by quadrature ML) but built on the person-wise marginal likelihood, which
# needs no O(2^J) joint contingency table (ledger C0006).
#
# The correctness oracle is the wrapped engine itself at small J, where both
# engines maximize the identical likelihood surface from identical
# first-stage cutpoints: log-likelihoods and dependence parameters must
# agree to optimizer tolerance, family by family.

test_that("native engine reproduces mle1factor per family at small J", {
  skip_if_not_installed("FactorCopula")
  skip_if_not_installed("statmod")

  base_theta <- list(bvn = 0.6, frk = 5, gum = 2, rgum = 2, joe = 2, rjoe = 2)
  for (fam in names(base_theta)) {
    set.seed(37)
    dat <- FactorCopula::r1factor(
      n = 400, d1 = 0, d2 = 5, categ = rep(3, 5),
      theta = rep(base_theta[[fam]], 5), copF1 = rep(fam, 5)
    )
    wrapped <- fit_copula_1f(dat, family = fam, nq = 25)
    native <- fit_copula_1f(dat, family = fam, nq = 25, engine = "irtc")

    expect_equal(
      native$log_likelihood, wrapped$log_likelihood, tolerance = 1e-5,
      label = sprintf("family %s native loglik", fam)
    )
    expect_equal(
      native$estimates$theta, wrapped$estimates$theta, tolerance = 1e-2,
      label = sprintf("family %s native theta", fam)
    )
    expect_equal(
      native$estimates$tau, wrapped$estimates$tau, tolerance = 1e-2,
      label = sprintf("family %s native tau", fam)
    )
    # Identical first-stage cutpoints -- both are empirical margins.
    expect_equal(
      native$estimates$cut1, wrapped$estimates$cut1, tolerance = 1e-8,
      label = sprintf("family %s native cutpoints", fam)
    )
  }
})

test_that("native engine returns a standard model result", {
  skip_if_not_installed("statmod")

  sim <- simulate_2pl(n_persons = 400, n_items = 6, seed = 13)
  res <- fit_copula_1f(sim, family = "bvn", nq = 15, engine = "irtc")

  expect_s3_class(res, "irtc_model_result")
  expect_equal(res$model, "copula-1f-bvn")
  expect_equal(res$engine, "irtc")
  expect_equal(res$engine_version, as.character(utils::packageVersion("irtc")))
  expect_true(res$converged)
  expect_true(res$log_likelihood < 0)
  expect_equal(res$n_parameters, 6)
  expect_contains(names(res$estimates), c("item", "family", "theta", "tau", "cut1"))
})

test_that("native engine scales past the FactorCopula joint-table wall", {
  skip_if_not_installed("statmod")

  # J = 45 binary items: FactorCopula's likelihood wants a 2^45-cell joint
  # table here (exp_2026_08_02_002/_003 failure mode); the person-wise
  # likelihood does not.
  sim <- simulate_2pl(n_persons = 300, n_items = 45, seed = 91)
  res <- fit_copula_1f(sim, family = "bvn", nq = 11, engine = "irtc")

  expect_true(res$converged)
  expect_true(is.finite(res$log_likelihood))
  expect_equal(nrow(res$estimates), 45)
  expect_true(all(is.finite(res$estimates$theta)))
})

test_that("native engine results evaluate through heldout_logloss exactly", {
  skip_if_not_installed("statmod")

  sim <- simulate_2pl(n_persons = 400, n_items = 6, seed = 13)
  fit <- fit_copula_1f(sim, family = "rjoe", nq = 25, engine = "irtc")
  ev <- heldout_logloss(fit, sim$responses, nq = 25)

  # Same quadrature, same h-functions: self-consistency at near machine
  # precision, as for the wrapped engine.
  expect_equal(ev$total_loglik, fit$log_likelihood, tolerance = 1e-8)
})

test_that("native engine skips missing responses instead of failing", {
  skip_if_not_installed("statmod")

  sim <- simulate_2pl(n_persons = 300, n_items = 6, seed = 8)
  holed <- sim$responses
  holed[cbind(1:20, rep(1:6, length.out = 20))] <- NA
  res <- fit_copula_1f(holed, family = "bvn", nq = 15, engine = "irtc")

  expect_true(res$converged)
  expect_true(is.finite(res$log_likelihood))
})

test_that("fit_copula_1f validates the engine argument", {
  ordinal <- matrix(sample(0:2, 60, replace = TRUE), ncol = 3)
  expect_error(fit_copula_1f(ordinal, family = "gum", engine = "nope"))
})
