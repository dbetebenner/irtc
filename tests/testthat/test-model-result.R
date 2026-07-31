# The model-result schema is the charter's WP3 contract: every fit records
# engine, versions, seed, convergence, warnings, and timing — and no
# nonconverged fit can slip silently into a comparison.

make_result_args <- function() {
  list(
    model = "2pl",
    engine = "mirt",
    engine_version = "1.44",
    data_description = list(
      source = "simulation",
      generator = list(model = "2pl", seed = 42, n_persons = 200, n_items = 10)
    ),
    seed = 42,
    converged = TRUE,
    log_likelihood = -1234.5,
    n_parameters = 20L,
    estimates = data.frame(item = c("I1", "I2"), a = c(1.1, 0.9), b = c(0, 0.5)),
    warnings = character(0),
    runtime_seconds = 1.25
  )
}

test_that("new_model_result builds a validated result object", {
  res <- do.call(new_model_result, make_result_args())

  expect_s3_class(res, "irtc_model_result")
  expect_equal(res$model, "2pl")
  expect_equal(res$engine, "mirt")
  expect_true(res$converged)
  expect_s3_class(res$estimates, "data.frame")
  expect_true(is.character(res$fitted_at) && nzchar(res$fitted_at))
  expect_equal(res$schema_version, 1L)
})

test_that("new_model_result rejects missing or malformed fields", {
  args <- make_result_args()

  bad <- args; bad$model <- NULL
  expect_error(do.call(new_model_result, bad), "model")

  bad <- args; bad$converged <- "yes"
  expect_error(do.call(new_model_result, bad), "converged")

  bad <- args; bad$log_likelihood <- 10
  expect_error(do.call(new_model_result, bad), "log_likelihood")

  bad <- args; bad$estimates <- "not a data frame"
  expect_error(do.call(new_model_result, bad), "estimates")
})

test_that("model results round-trip through JSON without loss", {
  res <- do.call(new_model_result, make_result_args())

  json <- model_result_to_json(res)
  back <- model_result_from_json(json)

  expect_s3_class(back, "irtc_model_result")
  expect_equal(back$model, res$model)
  expect_equal(back$seed, res$seed)
  expect_equal(back$log_likelihood, res$log_likelihood)
  expect_equal(back$estimates, res$estimates)
  expect_equal(back$schema_version, res$schema_version)
})

test_that("print method surfaces convergence loudly", {
  res <- do.call(new_model_result, make_result_args())
  expect_output(print(res), "2pl")
  expect_output(print(res), "converged")

  args <- make_result_args()
  args$converged <- FALSE
  bad_fit <- do.call(new_model_result, args)
  expect_output(print(bad_fit), "NOT CONVERGED")
})
