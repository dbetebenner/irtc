# Level 1 of the model ladder (charter 6): the IRTc product-copula wrapper.
# The acceptance test is equivalence: with the copula fixed to independence,
# parameter estimates, log-likelihoods, and predicted probabilities must
# match the mirt baseline within numerical tolerance. Probe on this exact
# seeded design: loglik diff < 1e-5, max parameter diff < 1e-4, predicted
# probability diff < 1e-5 -- test bounds are set with wide margins.

test_that("fit_irtc returns a well-formed model result", {
  sim <- simulate_2pl(n_persons = 300, n_items = 8, seed = 9)
  res <- fit_irtc(sim)

  expect_s3_class(res, "irtc_model_result")
  expect_equal(res$model, "irtc-2pl-independence")
  expect_equal(res$engine, "irtc")
  expect_true(res$converged)
  expect_true(res$log_likelihood < 0)
  expect_equal(res$n_parameters, 16L)
  expect_equal(nrow(res$estimates), 8)
  expect_named(res$estimates, c("item", "a", "b"), ignore.order = TRUE)
  expect_true(all(res$estimates$a > 0))
  expect_equal(res$data_description$source, "simulation")
})

test_that("Level-1 acceptance: independence-copula IRTc matches the mirt 2PL", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 1000, n_items = 10, seed = 42)

  irtc_fit <- fit_irtc(sim, nq = 61)
  mirt_fit <- fit_baseline(sim, model = "2pl")

  # Log-likelihoods agree.
  expect_equal(irtc_fit$log_likelihood, mirt_fit$log_likelihood, tolerance = 1e-5)

  # Item parameters agree (absolute scale, not just correlation).
  expect_lt(max(abs(irtc_fit$estimates$a - mirt_fit$estimates$a)), 0.01)
  expect_lt(max(abs(irtc_fit$estimates$b - mirt_fit$estimates$b)), 0.01)

  # Predicted response probabilities agree across the ability range.
  theta_grid <- seq(-3, 3, by = 0.5)
  p_from <- function(est) {
    plogis(outer(theta_grid, est$b, "-") * rep(est$a, each = length(theta_grid)))
  }
  expect_lt(
    max(abs(p_from(irtc_fit$estimates) - p_from(mirt_fit$estimates))),
    0.001
  )
})

test_that("fit_irtc is deterministic for the same data", {
  sim <- simulate_2pl(n_persons = 200, n_items = 5, seed = 3)
  a <- fit_irtc(sim)
  b <- fit_irtc(sim)
  expect_equal(a$estimates, b$estimates)
  expect_equal(a$log_likelihood, b$log_likelihood)
})

test_that("non-independence copulas are refused at Level 1", {
  sim <- simulate_2pl(n_persons = 100, n_items = 5, seed = 1)
  expect_error(fit_irtc(sim, copula = "gaussian"), "independence")
})

test_that("fit_irtc validates inputs", {
  expect_error(fit_irtc("not data"), "matrix")

  ordinal_sim <- simulate_grm(n_persons = 100, n_items = 5, n_categories = 3, seed = 1)
  expect_error(fit_irtc(ordinal_sim), "binary")

  sim <- simulate_2pl(n_persons = 100, n_items = 5, seed = 2)
  res <- fit_irtc(sim$responses)
  expect_equal(res$data_description$source, "matrix")
})
