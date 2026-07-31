# Level-0 baseline fits (charter model ladder): mirt-backed Rasch/2PL/GRM
# wrappers returning the standardized model-result object. Recovery
# thresholds are deliberately loose — these are smoke-level recovery tests,
# not the M1 recovery study itself.

test_that("fit_baseline fits a 2PL and recovers generating parameters", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 1000, n_items = 15, seed = 42)
  res <- fit_baseline(sim, model = "2pl")

  expect_s3_class(res, "irtc_model_result")
  expect_equal(res$model, "2pl")
  expect_equal(res$engine, "mirt")
  expect_true(res$converged)
  expect_true(res$log_likelihood < 0)
  expect_equal(nrow(res$estimates), 15)
  expect_named(res$estimates, c("item", "a", "b"), ignore.order = TRUE)

  # Parameter recovery: difficulty tight, discrimination looser.
  expect_gt(cor(res$estimates$b, sim$item_parameters$b), 0.9)
  expect_gt(cor(res$estimates$a, sim$item_parameters$a), 0.6)
})

test_that("fit_baseline fits a Rasch model with constant discrimination", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 500, n_items = 10, seed = 7)
  res <- fit_baseline(sim, model = "rasch")

  expect_equal(res$model, "rasch")
  expect_true(res$converged)
  expect_equal(length(unique(round(res$estimates$a, 6))), 1)
  expect_gt(cor(res$estimates$b, sim$item_parameters$b), 0.9)
})

test_that("fit_baseline fits a GRM on ordinal data", {
  skip_if_not_installed("mirt")

  sim <- simulate_grm(n_persons = 800, n_items = 10, n_categories = 4, seed = 11)
  res <- fit_baseline(sim, model = "grm")

  expect_equal(res$model, "grm")
  expect_true(res$converged)
  expect_equal(nrow(res$estimates), 10)
  threshold_cols <- grep("^b[0-9]+$", names(res$estimates), value = TRUE)
  expect_length(threshold_cols, 3)

  # Recovery on the pooled thresholds.
  true_b <- as.vector(as.matrix(sim$item_parameters[, c("b1", "b2", "b3")]))
  est_b <- as.vector(as.matrix(res$estimates[, threshold_cols]))
  expect_gt(cor(true_b, est_b), 0.9)
})

test_that("fit_baseline records provenance and runtime in the result", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 300, n_items = 8, seed = 3)
  res <- fit_baseline(sim, model = "2pl")

  expect_equal(res$data_description$source, "simulation")
  expect_equal(res$data_description$generator$seed, 3)
  expect_equal(res$engine_version, as.character(utils::packageVersion("mirt")))
  expect_true(is.numeric(res$runtime_seconds) && res$runtime_seconds >= 0)
})

test_that("fit_baseline rejects model/data mismatches and unknown models", {
  skip_if_not_installed("mirt")

  binary_sim <- simulate_2pl(n_persons = 100, n_items = 5, seed = 1)
  ordinal_sim <- simulate_grm(n_persons = 100, n_items = 5, n_categories = 3, seed = 1)

  expect_error(fit_baseline(binary_sim, model = "grm"), "ordinal")
  expect_error(fit_baseline(ordinal_sim, model = "2pl"), "binary")
  expect_error(fit_baseline(binary_sim, model = "not-a-model"))
})

test_that("fit_baseline accepts a raw response matrix", {
  skip_if_not_installed("mirt")

  sim <- simulate_2pl(n_persons = 300, n_items = 8, seed = 9)
  res <- fit_baseline(sim$responses, model = "2pl")

  expect_s3_class(res, "irtc_model_result")
  expect_true(res$converged)
  expect_equal(res$data_description$source, "matrix")
})
