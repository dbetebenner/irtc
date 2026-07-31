test_that("package_info returns a list with name + version", {
  info <- package_info()
  expect_type(info, "list")
  expect_named(info, c("name", "version"))
  expect_type(info$name, "character")
  expect_type(info$version, "character")
})
