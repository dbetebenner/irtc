# M4 held-out prediction machinery: marginal log-loss for unseen persons at
# fixed (training-time) parameters, for both model classes.
#
# The correctness oracle is self-consistency: evaluated ON THE TRAINING DATA
# at the fitted parameters, heldout_logloss must reproduce the engine's own
# maximized log-likelihood -- mirt's for the 2PL path (same 61-point normal
# quadrature), mle1factor's for each copula family (which verifies our copula
# h-functions against FactorCopula's likelihood exactly).

test_that("2PL path reproduces the training log-likelihood (irtc and mirt engines)", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 500, n_items = 8, seed = 21)

  own <- fit_irtc(sim, nq = 61)
  ev_own <- heldout_logloss(own, sim$responses, nq = 61)
  expect_equal(ev_own$total_loglik, own$log_likelihood, tolerance = 1e-8)

  base <- fit_baseline(sim, model = "2pl")
  ev_base <- heldout_logloss(base, sim$responses, nq = 61)
  expect_equal(ev_base$total_loglik, base$log_likelihood, tolerance = 1e-4)
})

test_that("copula path reproduces mle1factor's log-likelihood per family", {
  skip_if_not_installed("FactorCopula")
  skip_if_not_installed("statmod")

  base_theta <- list(bvn = 0.6, frk = 5, gum = 2, rgum = 2, joe = 2, rjoe = 2)
  for (fam in names(base_theta)) {
    set.seed(31)
    dat <- FactorCopula::r1factor(
      n = 400, d1 = 0, d2 = 5, categ = rep(3, 5),
      theta = rep(base_theta[[fam]], 5), copF1 = rep(fam, 5)
    )
    fit <- fit_copula_1f(dat, family = fam, nq = 25)
    ev <- heldout_logloss(fit, dat, nq = 25)
    expect_equal(
      ev$total_loglik, fit$log_likelihood, tolerance = 1e-6,
      label = sprintf("family %s heldout total_loglik", fam)
    )
  }
})

test_that("copula path handles binary items", {
  skip_if_not_installed("FactorCopula")
  skip_if_not_installed("statmod")

  sim <- simulate_2pl(n_persons = 400, n_items = 6, seed = 13)
  fit <- fit_copula_1f(sim, family = "bvn", nq = 25)
  ev <- heldout_logloss(fit, sim$responses, nq = 25)
  expect_equal(ev$total_loglik, fit$log_likelihood, tolerance = 1e-6)
})

test_that("held-out evaluation on a test fold returns sane per-response loss", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 800, n_items = 10, seed = 5)
  set.seed(99)
  test_idx <- sort(sample.int(800, 160))
  train <- sim$responses[-test_idx, ]
  test <- sim$responses[test_idx, ]

  fit <- fit_baseline(train, model = "2pl")
  ev <- heldout_logloss(fit, test)

  expect_true(is.finite(ev$total_loglik) && ev$total_loglik < 0)
  expect_equal(ev$n_responses, length(test))
  expect_gt(ev$logloss_per_response, 0)
  expect_lt(ev$logloss_per_response, log(2) * 3)
})

test_that("missing responses are skipped, not imputed", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 300, n_items = 6, seed = 8)
  fit <- fit_baseline(sim, model = "2pl")

  holed <- sim$responses
  holed[1, 1] <- NA
  holed[10, 3] <- NA
  ev <- heldout_logloss(fit, holed)

  expect_equal(ev$n_responses, length(holed) - 2)
  expect_true(is.finite(ev$total_loglik))
})

test_that("heldout_logloss validates inputs", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 200, n_items = 6, seed = 2)
  fit <- fit_baseline(sim, model = "2pl")

  expect_error(heldout_logloss(fit, sim$responses[, 1:4]), "items")
  expect_error(heldout_logloss("not a result", sim$responses), "irtc_model_result")

  ord <- simulate_grm(n_persons = 200, n_items = 5, n_categories = 4, seed = 3)
  grm_fit <- fit_baseline(ord, model = "grm")
  expect_error(heldout_logloss(grm_fit, ord$responses), "not yet supported")
})

test_that("extreme item parameters yield finite held-out log-loss (boundary clamp)", {
  # Regression: exp_2026_08_02_001's 2PL baseline returned NaN -- an extreme
  # discrimination pushed plogis to exactly 0/1 and the un-clamped marginal
  # path took log(0). The copula path already clamps; the 2PL path must too.
  est <- data.frame(item = c("I1", "I2"), a = c(500, 1.2), b = c(0, 0.3))
  fake <- structure(
    list(
      schema_version = 1L, model = "2pl", engine = "mirt", engine_version = "x",
      data_description = list(source = "matrix"), seed = NA_real_,
      converged = TRUE, log_likelihood = -1, n_parameters = 4L,
      estimates = est, warnings = character(0), runtime_seconds = 0,
      fitted_at = "now"
    ),
    class = "irtc_model_result"
  )
  y <- matrix(c(0L, 1L, 1L, 0L), nrow = 2)
  ev <- heldout_logloss(fake, y, nq = 21)
  expect_true(is.finite(ev$total_loglik))
  expect_true(is.finite(ev$logloss_per_response))
})
