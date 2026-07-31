test_that("simulate_2pl returns a well-formed simulation object", {
  sim <- simulate_2pl(n_persons = 200, n_items = 10, seed = 42)

  expect_s3_class(sim, "irtc_simulation")
  expect_named(
    sim,
    c("responses", "theta", "item_parameters", "generator"),
    ignore.order = TRUE
  )
  expect_true(is.matrix(sim$responses))
  expect_equal(dim(sim$responses), c(200, 10))
  expect_length(sim$theta, 200)
  expect_s3_class(sim$item_parameters, "data.frame")
  expect_equal(nrow(sim$item_parameters), 10)
  expect_named(sim$item_parameters, c("item", "a", "b"), ignore.order = TRUE)
})

test_that("simulate_2pl responses are binary and generator metadata is complete", {
  sim <- simulate_2pl(n_persons = 100, n_items = 5, seed = 1)

  expect_true(all(sim$responses %in% c(0L, 1L)))
  expect_equal(sim$generator$model, "2pl")
  expect_equal(sim$generator$seed, 1)
  expect_equal(sim$generator$n_persons, 100)
  expect_equal(sim$generator$n_items, 5)
})

test_that("simulate_2pl is reproducible under a seed and varies without one shared", {
  a <- simulate_2pl(n_persons = 50, n_items = 5, seed = 7)
  b <- simulate_2pl(n_persons = 50, n_items = 5, seed = 7)
  c <- simulate_2pl(n_persons = 50, n_items = 5, seed = 8)

  expect_identical(a$responses, b$responses)
  expect_identical(a$item_parameters, b$item_parameters)
  expect_false(identical(a$responses, c$responses))
})

test_that("simulate_2pl discrimination stays positive and difficulty in range", {
  sim <- simulate_2pl(n_persons = 50, n_items = 20, seed = 3)

  expect_true(all(sim$item_parameters$a > 0))
  expect_true(all(abs(sim$item_parameters$b) < 4))
})

test_that("simulate_2pl validates its inputs", {
  expect_error(simulate_2pl(n_persons = 0, n_items = 5, seed = 1), "n_persons")
  expect_error(simulate_2pl(n_persons = 50, n_items = 1, seed = 1), "n_items")
  expect_error(simulate_2pl(n_persons = 50, n_items = 5), "seed")
})

test_that("simulate_grm returns ordinal responses in the declared categories", {
  sim <- simulate_grm(n_persons = 150, n_items = 8, n_categories = 4, seed = 11)

  expect_s3_class(sim, "irtc_simulation")
  expect_equal(dim(sim$responses), c(150, 8))
  expect_true(all(sim$responses %in% seq_len(4)))
  # All categories should actually occur somewhere at this size.
  expect_setequal(sort(unique(as.vector(sim$responses))), seq_len(4))
  expect_equal(sim$generator$model, "grm")
  expect_equal(sim$generator$n_categories, 4)
})

test_that("simulate_grm thresholds are ordered within each item", {
  sim <- simulate_grm(n_persons = 50, n_items = 6, n_categories = 5, seed = 2)

  threshold_cols <- grep("^b[0-9]+$", names(sim$item_parameters), value = TRUE)
  expect_length(threshold_cols, 4)
  thresholds <- as.matrix(sim$item_parameters[, threshold_cols])
  expect_true(all(apply(thresholds, 1, function(x) all(diff(x) > 0))))
})

test_that("simulate_grm is reproducible under a seed", {
  a <- simulate_grm(n_persons = 40, n_items = 4, n_categories = 3, seed = 5)
  b <- simulate_grm(n_persons = 40, n_items = 4, n_categories = 3, seed = 5)

  expect_identical(a$responses, b$responses)
})

test_that("simulate_grm validates n_categories", {
  expect_error(
    simulate_grm(n_persons = 50, n_items = 5, n_categories = 1, seed = 1),
    "n_categories"
  )
})
