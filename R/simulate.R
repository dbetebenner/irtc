#' Simulate binary responses from a two-parameter logistic (2PL) model
#'
#' Generates person abilities `theta ~ N(0, 1)`, item discriminations
#' `a ~ Lognormal(0, 0.3)`, and item difficulties `b ~ Uniform(-2.5, 2.5)`,
#' then draws Bernoulli responses under
#' `P(Y = 1 | theta) = plogis(a * (theta - b))`.
#'
#' Every simulation records its generator metadata (model, seed, dimensions)
#' so downstream model results can carry full provenance.
#'
#' @param n_persons Number of persons (rows). Must be at least 1.
#' @param n_items Number of items (columns). Must be at least 2.
#' @param seed Integer seed. Required -- unseeded simulations are not
#'   reproducible and the charter forbids them in registered experiments.
#' @return An object of class `irtc_simulation`: a list with `responses`
#'   (integer matrix, persons x items), `theta` (numeric vector),
#'   `item_parameters` (data frame with `item`, `a`, `b`), and `generator`
#'   (list of model, seed, and dimensions).
#' @export
simulate_2pl <- function(n_persons, n_items, seed) {
  check_count(n_persons, "n_persons", min = 1)
  check_count(n_items, "n_items", min = 2)
  if (missing(seed)) {
    stop("`seed` is required and must be a single number (reproducibility is not optional).", call. = FALSE)
  }
  check_seed(seed)

  set.seed(seed)
  theta <- stats::rnorm(n_persons)
  a <- stats::rlnorm(n_items, meanlog = 0, sdlog = 0.3)
  b <- stats::runif(n_items, -2.5, 2.5)

  eta <- outer(theta, b, `-`) * rep(a, each = n_persons)
  p <- stats::plogis(eta)
  responses <- matrix(
    as.integer(stats::runif(n_persons * n_items) < p),
    nrow = n_persons,
    dimnames = list(NULL, paste0("I", seq_len(n_items)))
  )

  new_irtc_simulation(
    responses = responses,
    theta = theta,
    item_parameters = data.frame(
      item = paste0("I", seq_len(n_items)),
      a = a,
      b = b
    ),
    generator = list(
      model = "2pl",
      seed = seed,
      n_persons = n_persons,
      n_items = n_items
    )
  )
}

#' Simulate ordinal responses from a graded response model (GRM)
#'
#' Person abilities and discriminations as in [simulate_2pl()]. Each item
#' gets `n_categories - 1` strictly increasing thresholds (a uniform start
#' plus positive uniform gaps). Responses follow Samejima's graded response
#' model: `P(Y >= k | theta) = plogis(a * (theta - b_{k-1}))`.
#'
#' @inheritParams simulate_2pl
#' @param n_categories Number of ordered response categories (at least 2).
#'   Responses take values in `1:n_categories`.
#' @return An `irtc_simulation`; `item_parameters` holds `item`, `a`, and
#'   threshold columns `b1 ... b{n_categories - 1}`.
#' @export
simulate_grm <- function(n_persons, n_items, n_categories, seed) {
  check_count(n_persons, "n_persons", min = 1)
  check_count(n_items, "n_items", min = 2)
  check_count(n_categories, "n_categories", min = 2)
  if (missing(seed)) {
    stop("`seed` is required and must be a single number (reproducibility is not optional).", call. = FALSE)
  }
  check_seed(seed)

  set.seed(seed)
  theta <- stats::rnorm(n_persons)
  a <- stats::rlnorm(n_items, meanlog = 0, sdlog = 0.3)
  n_thresholds <- n_categories - 1
  thresholds <- t(vapply(
    seq_len(n_items),
    function(j) {
      start <- stats::runif(1, -2, -0.5)
      gaps <- stats::runif(n_thresholds - 1, 0.4, 1.2)
      cumsum(c(start, gaps))
    },
    numeric(n_thresholds)
  ))

  responses <- matrix(
    0L,
    nrow = n_persons,
    ncol = n_items,
    dimnames = list(NULL, paste0("I", seq_len(n_items)))
  )
  for (j in seq_len(n_items)) {
    # P(Y >= k) for k = 2..K, columns ordered by k.
    p_geq <- stats::plogis(outer(theta - 0, thresholds[j, ], function(th, b) a[j] * (th - b)))
    u <- stats::runif(n_persons)
    # Category = 1 + number of exceeded thresholds under inverse-CDF draw.
    responses[, j] <- 1L + rowSums(u < p_geq)
  }

  item_parameters <- data.frame(
    item = paste0("I", seq_len(n_items)),
    a = a
  )
  threshold_df <- as.data.frame(thresholds)
  names(threshold_df) <- paste0("b", seq_len(n_thresholds))
  item_parameters <- cbind(item_parameters, threshold_df)

  new_irtc_simulation(
    responses = responses,
    theta = theta,
    item_parameters = item_parameters,
    generator = list(
      model = "grm",
      seed = seed,
      n_persons = n_persons,
      n_items = n_items,
      n_categories = n_categories
    )
  )
}

new_irtc_simulation <- function(responses, theta, item_parameters, generator) {
  structure(
    list(
      responses = responses,
      theta = theta,
      item_parameters = item_parameters,
      generator = generator
    ),
    class = "irtc_simulation"
  )
}

check_count <- function(x, name, min) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x != as.integer(x) || x < min) {
    stop(sprintf("`%s` must be a single integer >= %d.", name, min), call. = FALSE)
  }
  invisible(TRUE)
}

check_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    stop("`seed` is required and must be a single number (reproducibility is not optional).", call. = FALSE)
  }
  invisible(TRUE)
}
