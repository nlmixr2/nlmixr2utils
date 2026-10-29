skip_if_not(exists("rxEventScope", envir = asNamespace("rxode2"), inherits = FALSE),
            "rxode2 has no event bus")

test_that(".plap runs every item inside the rxode2 event scope", {
  depth <- function(i) rxode2::rxEventDepth()
  expect_true(all(unlist(.plap(1:3, depth)) >= 1L))
  expect_identical(rxode2::rxEventDepth(), 0L)
  ## with rxThreads, too
  expect_true(all(unlist(.plap(1:2, depth, rxThreads = 1L)) >= 1L))
})

test_that(".plap workers of a plan set beforehand are scoped; plain future_lapply is not", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(future::sequential))
  depth <- function(i) rxode2::rxEventDepth()
  expect_true(all(unlist(.plap(1:4, depth)) >= 1L))
  ## negative control: the same workers without .plap's scope
  expect_true(all(unlist(future.apply::future_lapply(1:4, depth)) == 0L))
})

test_that("the lapply fallback is scoped and an error restores the depth", {
  local_mocked_bindings(requireNamespace = function(package, ...) package != "future.apply",
                        .package = "base")
  expect_true(all(unlist(.plap(1:2, function(i) rxode2::rxEventDepth())) >= 1L))
  expect_error(.plap(1, function(i) stop("boom")), "boom")
  expect_identical(rxode2::rxEventDepth(), 0L)
})
