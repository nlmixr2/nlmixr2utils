skip_on_cran()

.testDir <- function() {
  path <- tempfile("nlmixr2utils-cache-")
  dir.create(path)
  path
}

test_that("resolveRunDir resumes the latest numbered directory and allocates the next one", {
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)

  dir.create("myfit_boot_1")
  dir.create("myfit_boot_2")

  resumed <- resolveRunDir("boot", "myfit", restart = FALSE)
  fresh <- resolveRunDir("boot", "myfit", restart = TRUE)

  expect_equal(basename(resumed$path), "myfit_boot_2")
  expect_equal(resumed$mode, "resume")
  expect_equal(basename(fresh$path), "myfit_boot_3")
  expect_equal(fresh$mode, "new")
})

test_that("resolveRunDir distinguishes explicit resume and overwrite modes", {
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  explicit <- file.path(tmp, "custom-output")
  dir.create(explicit)

  resumed <- resolveRunDir("sir", "fit", restart = FALSE, outputDir = explicit)
  overwritten <- resolveRunDir(
    "sir",
    "fit",
    restart = TRUE,
    outputDir = explicit
  )
  created <- resolveRunDir(
    "sir",
    "fit",
    restart = FALSE,
    outputDir = file.path(tmp, "new-output")
  )

  expect_equal(resumed$mode, "resume")
  expect_equal(overwritten$mode, "overwrite")
  expect_equal(created$mode, "new")
})

test_that("writeRunState/readRunState round-trip state objects", {
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  state <- list(done = 3L, notes = "ok")

  writeRunState(tmp, state, "sse")

  expect_equal(readRunState(tmp, "sse"), state)
})

test_that("readRunState rejects future schema versions", {
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  saveRDS(
    list(schema_version = 999L, state = list()),
    file.path(tmp, "sse_state.rds")
  )

  err <- tryCatch(
    {
      readRunState(tmp, "sse")
      NULL
    },
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "newer than this helper understands")
})

test_that("taskCache stores and retrieves key-addressed artifacts", {
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  cache <- taskCache(tmp, "fits")

  expect_false(cache$has("sample/1"))
  cache$put("sample/1", list(ok = TRUE, n = 1L))

  expect_true(cache$has("sample/1"))
  expect_equal(cache$get("sample/1"), list(ok = TRUE, n = 1L))
  expect_equal(cache$keys(), "sample/1")
})

test_that("pendingTasks preserves order while removing completed keys", {
  expect_equal(
    pendingTasks(c("a", "b", "c", "d"), c("b", "d")),
    c("a", "c")
  )
})

test_that("withRunSeed persists the master seed and restores RNG state", {
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  set.seed(123)
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  expect_equal(withRunSeed(tmp, seed = 42L), 42L)
  expect_equal(withRunSeed(tmp, seed = 999L), 42L)

  derived_a <- withRunSeed(tmp, key = "sample-1")
  derived_b <- withRunSeed(tmp, key = "sample-1")
  derived_c <- withRunSeed(tmp, key = "sample-2")

  expect_equal(derived_a, derived_b)
  expect_false(identical(derived_a, derived_c))

  draws1 <- withRunSeed(tmp, key = "sample-1", expr = stats::runif(3))
  draws2 <- withRunSeed(tmp, key = "sample-1", expr = stats::runif(3))

  expect_equal(draws1, draws2)
  expect_equal(
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    before
  )
})

test_that("withRunSeed honors explicit seed-file prefixes", {
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  expect_equal(withRunSeed(tmp, seed = 7L, prefix = "sir"), 7L)
  expect_true(file.exists(file.path(tmp, "sir_seed.rds")))
  expect_false(file.exists(file.path(tmp, "run_seed.rds")))
  expect_equal(withRunSeed(tmp, seed = 99L, prefix = "sir"), 7L)
})

test_that("deriveFitName sanitizes list extraction expressions", {
  expect_equal(deriveFitName(substitute(myList$fit)), "myList_fit")
  expect_equal(deriveFitName(substitute(fits[[1]])), "fits_1")
  expect_equal(deriveFitName(substitute(1 + 1)), "1_1")
})

test_that("run-cache errors interpolate values from the calling frame", {
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  # See the matching test in test-raw-results.R: `.abortRunCache()` had the same
  # `.envir` defect. `get()` is a closure, so this also covers interpolation
  # against a calling frame that is not a plain function body.
  cache <- taskCache(tmp)
  expect_error(
    cache$get("no-such-key"),
    'Cache key "no-such-key" was not found',
    fixed = TRUE
  )

  path <- file.path(tmp, "run_state.rds")
  saveRDS(list(not_a_state = TRUE), path)
  expect_error(
    readRunState(tmp, "run"),
    "does not follow the versioned schema format",
    fixed = TRUE
  )
})
