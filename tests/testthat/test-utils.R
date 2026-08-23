skip_on_cran()

.cur <- loadNamespace("nlmixr2utils")

# =============================================================================
# setQuietFastControl
# =============================================================================

test_that("setQuietFastControl: sets print=0", {
  ctl <- list(
    print = 100,
    covMethod = "r,s",
    calcTables = TRUE,
    compress = TRUE
  )
  out <- .cur$setQuietFastControl(ctl)
  expect_equal(out$print, 0L)
})

test_that("setQuietFastControl: sets covMethod=0", {
  ctl <- list(
    print = 100,
    covMethod = "r,s",
    calcTables = TRUE,
    compress = TRUE
  )
  out <- .cur$setQuietFastControl(ctl)
  expect_equal(out$covMethod, 0L)
})

test_that("setQuietFastControl: sets calcTables=FALSE", {
  ctl <- list(
    print = 100,
    covMethod = "r,s",
    calcTables = TRUE,
    compress = TRUE
  )
  out <- .cur$setQuietFastControl(ctl)
  expect_false(out$calcTables)
})

test_that("setQuietFastControl: sets compress=FALSE", {
  ctl <- list(
    print = 100,
    covMethod = "r,s",
    calcTables = TRUE,
    compress = TRUE
  )
  out <- .cur$setQuietFastControl(ctl)
  expect_false(out$compress)
})

test_that("setQuietFastControl: preserves fields it does not touch", {
  ctl <- list(
    print = 100,
    covMethod = "r,s",
    calcTables = TRUE,
    compress = TRUE,
    eval.max = 999,
    grad.eps = 0.001
  )
  out <- .cur$setQuietFastControl(ctl)
  expect_equal(out$eval.max, 999)
  expect_equal(out$grad.eps, 0.001)
})

# =============================================================================
# .plap — parallel lapply wrapper
# =============================================================================

test_that(".plap: applies FUN to every element and preserves order", {
  res <- .cur$.plap(1:5, function(x) x * 10)
  expect_equal(unlist(res), c(10, 20, 30, 40, 50))
})

test_that(".plap: returns a list", {
  res <- .cur$.plap(1:3, function(x) x)
  expect_type(res, "list")
})

test_that(".plap: passes extra ... arguments to FUN", {
  res <- .cur$.plap(1:3, function(x, y) x + y, y = 100)
  expect_equal(unlist(res), c(101, 102, 103))
})

test_that(".plap: empty input returns empty list", {
  res <- .cur$.plap(integer(0), function(x) x)
  expect_equal(res, list())
})

test_that(".plap: .label function is accepted without error", {
  expect_no_error(
    .cur$.plap(1:4, function(x) x * 2, .label = function(x) paste("item", x))
  )
})

test_that(".plap: result length equals input length", {
  n <- 7
  res <- .cur$.plap(seq_len(n), function(x) x)
  expect_length(res, n)
})

test_that(".plap: FUN can return complex objects", {
  res <- .cur$.plap(1:3, function(x) list(val = x, sq = x^2))
  expect_equal(res[[2]]$val, 2)
  expect_equal(res[[3]]$sq, 9)
})

test_that(".plap: future.apply path preserves order", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  future::plan("sequential")

  res <- .cur$.plap(1:5, function(x) x * 10)

  expect_equal(unlist(res), c(10, 20, 30, 40, 50))
})

# =============================================================================
# .validateRxThreads
# =============================================================================

test_that(".validateRxThreads: accepts NULL", {
  expect_no_error(.cur$.validateRxThreads(NULL))
})

test_that(".validateRxThreads: accepts 'auto'", {
  expect_no_error(.cur$.validateRxThreads("auto"))
})

test_that(".validateRxThreads: accepts a positive integer", {
  expect_no_error(.cur$.validateRxThreads(4L))
})

test_that(".validateRxThreads: rejects invalid values", {
  bad_values <- list(0, -1, NA_real_, NaN, Inf, c(1, 2), 1.5, "bad")

  for (rxThreads in bad_values) {
    expect_error(.cur$.validateRxThreads(rxThreads), "rxThreads")
  }
})

# =============================================================================
# .resolveEffectiveWorkers / .resolveTotalCores
# =============================================================================

test_that(".resolveEffectiveWorkers: explicit integer passes through", {
  expect_equal(.cur$.resolveEffectiveWorkers(4L), 4L)
})

test_that(".resolveEffectiveWorkers: NULL reflects the ambient plan", {
  skip_if_not_installed("future")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)

  future::plan("sequential")
  expect_equal(.cur$.resolveEffectiveWorkers(NULL), 1L)

  future::plan("multisession", workers = 2L)
  expect_equal(.cur$.resolveEffectiveWorkers(NULL), 2L)
})

test_that(".resolveEffectiveWorkers: 'auto' returns a positive integer", {
  skip_if_not_installed("future")
  result <- .cur$.resolveEffectiveWorkers("auto")
  expect_type(result, "integer")
  expect_gte(result, 1L)
})

test_that(".resolveEffectiveWorkers: rejects invalid values", {
  bad_values <- list(0, -1, NA_real_, NaN, Inf, c(1, 2), 1.5, "bad")

  for (workers in bad_values) {
    expect_error(.cur$.resolveEffectiveWorkers(workers), "workers")
  }
})

test_that(".resolveTotalCores: uses parallel::detectCores(), not a policy-shrunk value", {
  real_cores <- parallel::detectCores()
  skip_if(is.na(real_cores), "parallel::detectCores() could not be determined")
  skip_if_not_installed("future")

  old_env <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
    } else {
      Sys.setenv("_R_CHECK_LIMIT_CORES_" = old_env)
    },
    add = TRUE
  )
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "true")

  # .resolveTotalCores() must report the real core count, not the
  # policy-shrunk future::availableCores() value.
  expect_equal(.cur$.resolveTotalCores(), as.integer(real_cores))
})

test_that(".resolveTotalCores: returns a positive integer or NA_integer_", {
  result <- .cur$.resolveTotalCores()
  expect_true(is.na(result) || (is.numeric(result) && result >= 1L))
})

# =============================================================================
# .withWorkerPlan
# =============================================================================

test_that(".withWorkerPlan: NULL workers evaluates expr and returns its value", {
  expect_equal(.cur$.withWorkerPlan(NULL, 1 + 1), 2)
})

test_that(".withWorkerPlan: NULL workers works without future installed", {
  # NULL still evaluates expr and returns its value; effectiveWorkers
  # resolves to 1L (not an error) when future isn't installed, and the
  # guard is skipped whenever effectiveWorkers <= 1, so this path stays
  # available either way.
  result <- .cur$.withWorkerPlan(NULL, "hello")
  expect_equal(result, "hello")
})

test_that(".withWorkerPlan: NULL workers passes side effects through", {
  x <- 0L
  .cur$.withWorkerPlan(NULL, {
    x <- 99L
  })
  expect_equal(x, 99L)
})

test_that(".withWorkerPlan: workers=1 evaluates expr and returns value", {
  skip_if_not_installed("future")
  expect_equal(.cur$.withWorkerPlan(1L, sum(1:10)), 55L)
})

test_that(".withWorkerPlan: workers=1 restores original plan on clean exit", {
  skip_if_not_installed("future")
  plan_before <- class(future::plan())
  on.exit(future::plan("sequential"), add = TRUE) # safety net
  .cur$.withWorkerPlan(1L, NULL)
  expect_equal(class(future::plan()), plan_before)
})

test_that(".withWorkerPlan: plan restored even when expr throws an error", {
  skip_if_not_installed("future")
  plan_before <- class(future::plan())
  on.exit(future::plan("sequential"), add = TRUE)
  try(.cur$.withWorkerPlan(1L, stop("intentional error")), silent = TRUE)
  expect_equal(class(future::plan()), plan_before)
})

test_that(".withWorkerPlan: error from expr is propagated to caller", {
  skip_if_not_installed("future")
  expect_error(.cur$.withWorkerPlan(1L, stop("boom")), "boom")
})

test_that(".withWorkerPlan: workers='auto' evaluates expr without error", {
  skip_if_not_installed("future")
  expect_no_error(.cur$.withWorkerPlan("auto", rxThreads = 1L, TRUE))
})

test_that(".withWorkerPlan: workers='auto' returns expr value", {
  skip_if_not_installed("future")
  result <- .cur$.withWorkerPlan("auto", rxThreads = 1L, 42L)
  expect_equal(result, 42L)
})

test_that(".withWorkerPlan: rejects invalid workers values", {
  bad_workers <- list(0, -1, NA_real_, NaN, Inf, c(1, 2), 1.5, "bad")

  for (workers in bad_workers) {
    expect_error(.cur$.withWorkerPlan(workers, TRUE), "workers")
  }
})

test_that(".withWorkerPlan: workers=2 restores original plan", {
  skip_if_not_installed("future")
  plan_before <- class(future::plan())
  on.exit(future::plan("sequential"), add = TRUE)

  .cur$.withWorkerPlan(2L, rxThreads = 1L, NULL)

  expect_equal(class(future::plan()), plan_before)
})

test_that(".withWorkerPlan: guard fires when workers * rxThreads exceeds cores (mocked)", {
  skip_if_not_installed("future")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  testthat::local_mocked_bindings(.resolveTotalCores = function() 4L)

  expect_error(
    .cur$.withWorkerPlan(workers = 2L, rxThreads = 3L, expr = NULL),
    class = "rlang_error"
  )
})

test_that(".withWorkerPlan: guard does not fire when product is within cores (mocked)", {
  skip_if_not_installed("future")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  testthat::local_mocked_bindings(.resolveTotalCores = function() 8L)

  expect_equal(
    .cur$.withWorkerPlan(workers = 2L, rxThreads = 3L, expr = 41 + 1),
    42
  )
})

test_that(".withWorkerPlan: guard is skipped for a single worker even if the product would exceed cores", {
  # This is the case the design got wrong initially: effectiveWorkers <= 1
  # must never abort, regardless of rxThreads or the mocked core count --
  # a single process cannot oversubscribe by definition.
  skip_if_not_installed("future")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  testthat::local_mocked_bindings(.resolveTotalCores = function() 2L)

  expect_equal(
    .cur$.withWorkerPlan(workers = 1L, rxThreads = 999L, expr = 1 + 1),
    2
  )
})

test_that(".withWorkerPlan: guard fires for an ambient parallel plan when workers = NULL (mocked)", {
  skip_if_not_installed("future")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  testthat::local_mocked_bindings(.resolveTotalCores = function() 4L)

  future::plan("multisession", workers = 2L)

  expect_error(
    .cur$.withWorkerPlan(workers = NULL, rxThreads = 3L, expr = NULL),
    class = "rlang_error"
  )
})

test_that(".withWorkerPlan: prints the exact effective workers/threads/cores numbers", {
  skip_if_not_installed("future")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  testthat::local_mocked_bindings(.resolveTotalCores = function() 8L)

  expect_message(
    .cur$.withWorkerPlan(workers = 2L, rxThreads = 3L, expr = NULL),
    "Workers.*2"
  )
  expect_message(
    .cur$.withWorkerPlan(workers = 2L, rxThreads = 3L, expr = NULL),
    "threads / worker.*3"
  )
  expect_message(
    .cur$.withWorkerPlan(workers = 2L, rxThreads = 3L, expr = NULL),
    "Cores available.*8"
  )
})

test_that(".withWorkerPlan: restores rxode2 thread count on clean exit", {
  skip_if_not_installed("future")
  skip_if_not_installed("rxode2")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  threads_before <- rxode2::getRxThreads()
  on.exit(rxode2::setRxThreads(threads_before), add = TRUE)

  .cur$.withWorkerPlan(workers = 1L, rxThreads = 1L, expr = NULL)

  expect_equal(rxode2::getRxThreads(), threads_before)
})

test_that(".withWorkerPlan: restores rxode2 thread count when expr throws", {
  skip_if_not_installed("future")
  skip_if_not_installed("rxode2")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  threads_before <- rxode2::getRxThreads()
  on.exit(rxode2::setRxThreads(threads_before), add = TRUE)

  try(
    .cur$.withWorkerPlan(
      workers = 1L,
      rxThreads = 1L,
      expr = stop("intentional error")
    ),
    silent = TRUE
  )

  expect_equal(rxode2::getRxThreads(), threads_before)
})

test_that(".withWorkerPlan: rxThreads propagates to the main-session rxode2 setting", {
  skip_if_not_installed("future")
  skip_if_not_installed("rxode2")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  threads_before <- rxode2::getRxThreads()
  on.exit(rxode2::setRxThreads(threads_before), add = TRUE)

  target <- if (threads_before == 1L) 2L else 1L
  result <- .cur$.withWorkerPlan(
    workers = 1L,
    rxThreads = target,
    expr = rxode2::getRxThreads()
  )

  expect_equal(result, target)
})

test_that(".withWorkerPlan: broadcasts rxThreads into multisession workers", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("rxode2")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)
  threads_before <- rxode2::getRxThreads()
  # Mock a generous core count so this test exercises broadcast behavior in
  # isolation from the guard -- workers=2 * target(<=2) must never trip the
  # abort here regardless of the real test machine's actual core count.
  testthat::local_mocked_bindings(.resolveTotalCores = function() 100L)

  target <- if (threads_before == 1L) 2L else 1L
  # A caller that never itself passes rxThreads to .plap()/future_lapply()
  # should still see workers running at the resolved thread count, via
  # the broadcast.
  result <- .cur$.withWorkerPlan(
    workers = 2L,
    rxThreads = target,
    expr = future.apply::future_lapply(1:2, function(i) rxode2::getRxThreads())
  )

  expect_equal(unlist(result), c(target, target))
})

test_that(".withWorkerPlan: invalid workers is rejected before any plan change", {
  skip_if_not_installed("future")
  plan_before <- future::plan()
  on.exit(future::plan(plan_before), add = TRUE)

  expect_error(.cur$.withWorkerPlan(workers = -1, rxThreads = 1L, expr = NULL))
  expect_equal(class(future::plan()), class(plan_before))
})

# =============================================================================
# resolveRxThreads
# =============================================================================

test_that("resolveRxThreads: NULL returns the current rxode2 thread count", {
  skip_if_not_installed("rxode2")
  expect_equal(
    .cur$resolveRxThreads(workers = NULL, rxThreads = NULL),
    as.integer(rxode2::getRxThreads())
  )
})

test_that("resolveRxThreads: explicit integer passes through", {
  expect_equal(.cur$resolveRxThreads(workers = 4L, rxThreads = 2L), 2L)
})

test_that("resolveRxThreads: 'auto' divides total cores by effective workers", {
  skip_if_not_installed("future")
  totalCores <- .cur$.resolveTotalCores()
  skip_if(is.na(totalCores), "total core count could not be determined")

  result <- .cur$resolveRxThreads(workers = 2L, rxThreads = "auto")
  expect_equal(result, max(1L, as.integer(floor(totalCores / 2L))))
})

test_that("resolveRxThreads: 'auto' never returns less than 1", {
  skip_if_not_installed("future")
  totalCores <- .cur$.resolveTotalCores()
  skip_if(is.na(totalCores), "total core count could not be determined")

  # deliberately request far more workers than cores, to force the floor
  result <- .cur$resolveRxThreads(
    workers = totalCores * 100L,
    rxThreads = "auto"
  )
  expect_equal(result, 1L)
})

test_that("resolveRxThreads: 'auto' falls back to 1L when total cores can't be determined", {
  testthat::local_mocked_bindings(.resolveTotalCores = function() NA_integer_)
  result <- .cur$resolveRxThreads(workers = 8L, rxThreads = "auto")
  expect_equal(result, 1L)
})

test_that("resolveRxThreads: rejects invalid rxThreads", {
  expect_error(
    .cur$resolveRxThreads(workers = NULL, rxThreads = -1),
    "rxThreads"
  )
})

test_that("resolveRxThreads: rejects invalid workers before touching rxThreads", {
  expect_error(
    .cur$resolveRxThreads(workers = -1, rxThreads = NULL),
    "workers"
  )
  expect_error(
    .cur$resolveRxThreads(workers = "bad", rxThreads = 1L),
    "workers"
  )
})
