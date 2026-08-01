# M2 (charter 18): FactorCopula smoke tests, reproduction of a documented
# factor-copula example, and simulation-based parameter recovery for one
# copula family. Reference values below were captured from a local run of
# the package's documented PE example (mle1factor man page) on
# FactorCopula, R 4.5 — the reproduction test asserts against them.

test_that("the documented PE one-factor example reproduces", {
  skip_if_not_installed("FactorCopula")
  skip_if_not_installed("statmod")

  # The mle1factor man-page example, verbatim modulo hessian (not needed
  # for the reproduction check).
  gl <- statmod::gauss.quad.prob(25)
  PE <- get_factorcopula_data("PE")
  continuous_pe <- cbind(-PE[, 1], PE[, 2])
  categorical_pe <- PE[, 3:5]
  families <- c("joe", "joe", "rjoe", "joe", "gum")

  est <- FactorCopula::mle1factor(
    continuous_pe, categorical_pe, count = NULL,
    copF1 = families, gl
  )

  expect_equal(est$loglik, -151.9777, tolerance = 1e-3)
  expect_equal(
    unname(est$taus),
    c(0.506, 0.577, 0.801, 0.677, 0.738),
    tolerance = 1e-2
  )
  expect_equal(
    unlist(est$cpar$f1),
    c(2.9057, 3.5631, 8.8255, 5.0034, 3.8220),
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
})

test_that("fit_copula_1f fits ordinal data and returns a model result", {
  skip_if_not_installed("FactorCopula")
  skip_if_not_installed("statmod")

  set.seed(42)
  dat <- FactorCopula::r1factor(
    n = 500, d1 = 0, d2 = 5, categ = rep(3, 5),
    theta = rep(2, 5), copF1 = rep("gum", 5)
  )
  colnames(dat) <- paste0("I", 1:5)

  res <- fit_copula_1f(dat, family = "gum", nq = 15)

  expect_s3_class(res, "irtc_model_result")
  expect_equal(res$model, "copula-1f-gum")
  expect_equal(res$engine, "FactorCopula")
  expect_equal(res$engine_version, as.character(utils::packageVersion("FactorCopula")))
  expect_true(res$converged)
  expect_true(res$log_likelihood < 0)
  expect_equal(nrow(res$estimates), 5)
  expect_named(res$estimates, c("item", "family", "theta", "tau"), ignore.order = TRUE)
  expect_true(all(res$estimates$family == "gum"))
})

test_that("fit_copula_1f recovers the generating Gumbel parameter", {
  skip_if_not_installed("FactorCopula")
  skip_if_not_installed("statmod")

  set.seed(42)
  dat <- FactorCopula::r1factor(
    n = 500, d1 = 0, d2 = 5, categ = rep(3, 5),
    theta = rep(2, 5), copF1 = rep("gum", 5)
  )

  res <- fit_copula_1f(dat, family = "gum", nq = 15)

  # Same seed/design as the pre-test probe: max abs error there was 0.27;
  # 0.6 is a loose smoke-level bound.
  expect_lt(max(abs(res$estimates$theta - 2)), 0.6)
})

test_that("fit_copula_1f accepts per-item families and an irtc GRM simulation", {
  skip_if_not_installed("FactorCopula")
  skip_if_not_installed("statmod")

  sim <- simulate_grm(n_persons = 300, n_items = 4, n_categories = 3, seed = 5)
  res <- fit_copula_1f(sim, family = c("gum", "gum", "frk", "frk"), nq = 15)

  expect_s3_class(res, "irtc_model_result")
  expect_equal(res$model, "copula-1f-mixed")
  expect_equal(res$estimates$family, c("gum", "gum", "frk", "frk"))
  expect_equal(res$data_description$source, "simulation")
  expect_equal(res$data_description$generator$seed, 5)
})

test_that("fit_copula_1f validates inputs", {
  skip_if_not_installed("FactorCopula")

  ordinal <- matrix(sample(0:2, 60, replace = TRUE), ncol = 3)

  expect_error(fit_copula_1f(ordinal, family = c("gum", "gum")), "family")
  expect_error(fit_copula_1f("not data", family = "gum"), "matrix")

  constant <- matrix(1L, nrow = 50, ncol = 3)
  expect_error(fit_copula_1f(constant, family = "gum"), "categories")
})

test_that("fit_copula_1f supports binary items (single cutpoint per item)", {
  skip_if_not_installed("FactorCopula")
  skip_if_not_installed("statmod")

  sim <- simulate_2pl(n_persons = 400, n_items = 6, seed = 13)
  res <- fit_copula_1f(sim, family = "bvn", nq = 15)

  expect_s3_class(res, "irtc_model_result")
  expect_equal(res$model, "copula-1f-bvn")
  expect_true(res$converged)
  expect_true(res$log_likelihood < 0)
  expect_equal(nrow(res$estimates), 6)
})
