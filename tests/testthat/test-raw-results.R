skip_on_cran()

.mockRawResultsFit <- function() {
  list(
    theta = c(tka = 0.5, tcl = 1.2, add.sd = 0.3),
    omega = structure(
      matrix(
        c(0.10, 0.02, 0.02, 0.20),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(
          c("eta.ka", "eta.cl"),
          c("eta.ka", "eta.cl")
        )
      )
    ),
    sigma = structure(
      matrix(
        0.4,
        nrow = 1,
        dimnames = list("eps1", "eps1")
      )
    ),
    iniDf = data.frame(
      ntheta = c(1L, 2L, 3L, NA, NA, NA, NA),
      neta1 = c(NA, NA, NA, 1L, 2L, 2L, NA),
      neta2 = c(NA, NA, NA, 1L, 1L, 2L, NA),
      name = c(
        "tka",
        "tcl",
        "add.sd",
        "eta.ka",
        "(eta.cl,eta.ka)",
        "eta.cl",
        "eps1"
      ),
      lower = c(-Inf, -Inf, 0, -Inf, -Inf, -Inf, 0),
      est = c(0.5, 1.2, 0.3, 0.10, 0.02, 0.20, 0.4),
      upper = rep(Inf, 7),
      fix = rep(FALSE, 7),
      label = rep(NA_character_, 7),
      backTransform = rep(NA_character_, 7),
      condition = c(NA, NA, "cp", "id", "id", "id", "cp"),
      err = c(NA, NA, "add", NA, NA, NA, "add"),
      stringsAsFactors = FALSE
    ),
    parFixedDf = data.frame(
      Parameter = c("Log Ka", "Log Cl", NA),
      Estimate = c(0.5, 1.2, 0.3),
      SE = c(0.05, 0.10, 0.02),
      check.names = FALSE,
      row.names = c("tka", "tcl", "add.sd")
    ),
    objf = 123.45,
    cov = structure(
      diag(c(0.0025, 0.0100), 2),
      dimnames = list(c("tka", "tcl"), c("tka", "tcl"))
    ),
    message = "",
    est = "focei"
  )
}

.testDir <- function() {
  path <- tempfile("nlmixr2utils-raw-")
  dir.create(path)
  path
}

test_that("rawResultsSchema exposes the canonical block layout", {
  fit <- .mockRawResultsFit()
  schema <- rawResultsSchema(fit)

  expect_equal(schema$thetaCols, c("tka", "tcl", "add.sd"))
  expect_equal(
    schema$omegaCols,
    c(
      "omega(eta.ka,eta.ka)",
      "omega(eta.cl,eta.ka)",
      "omega(eta.cl,eta.cl)"
    )
  )
  expect_equal(schema$sigmaCols, "sigma(eps1,eps1)")
  expect_equal(
    schema$seCols,
    paste0(
      c(schema$thetaCols, schema$omegaCols, schema$sigmaCols),
      ".se"
    )
  )
  expect_equal(schema$columns[[1L]], "source")
  expect_equal(tail(schema$columns, 1L), "sigma(eps1,eps1).se")
})

test_that("rawResultsRow populates parameter and SE blocks", {
  fit <- .mockRawResultsFit()
  row <- rawResultsRow(
    fit,
    source = "bootstrap",
    hypothesis = "sample",
    sample = 2L,
    modelLabel = "reference",
    role = "reference"
  )

  expect_equal(row$sample[[1L]], 2L)
  expect_equal(row[["tka"]][[1L]], 0.5)
  expect_equal(row[["omega(eta.cl,eta.ka)"]][[1L]], 0.02)
  expect_equal(row[["sigma(eps1,eps1)"]][[1L]], 0.4)
  expect_equal(row[["tcl.se"]][[1L]], 0.10)
  expect_true(is.na(row[["omega(eta.cl,eta.ka).se"]][[1L]]))
  expect_equal(row$minimization_successful[[1L]], 1L)
  expect_equal(row$covariance_step_successful[[1L]], 1L)
})

test_that("rawResultsRow does not mistake a normal exit message for failure", {
  # Real nlmixr2est fit objects have no $minimization_successful field and
  # always populate $message with the optimizer's exit text -- including on
  # success (e.g. "Normal exit from bobyqa"). $convergence (0 = success,
  # mirroring the underlying optimizer's ierr/status code) is the reliable
  # signal.
  fit <- .mockRawResultsFit()
  fit$message <- "Normal exit from bobyqa"
  fit$convergence <- 0L

  row <- rawResultsRow(
    fit,
    source = "bootstrap",
    hypothesis = "sample",
    sample = 3L,
    modelLabel = "reference",
    role = "reference"
  )

  expect_equal(row$minimization_successful[[1L]], 1L)
})

test_that("rawResultsRow flags a genuinely failed minimization via $convergence", {
  fit <- .mockRawResultsFit()
  fit$message <- "false convergence (8)"
  fit$convergence <- -42L

  row <- rawResultsRow(
    fit,
    source = "bootstrap",
    hypothesis = "sample",
    sample = 4L,
    modelLabel = "reference",
    role = "reference"
  )

  expect_equal(row$minimization_successful[[1L]], 0L)
})

test_that("writeRawResults/readRawResults round-trip and preserve header blocks", {
  fit <- .mockRawResultsFit()
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  rows <- do.call(
    rbind,
    list(
      rawResultsRow(
        fit,
        source = "bootstrap",
        hypothesis = "reference",
        sample = 0L,
        modelLabel = "reference",
        role = "reference"
      ),
      rawResultsRow(
        fit,
        source = "bootstrap",
        hypothesis = "sample",
        sample = 1L,
        modelLabel = "reference",
        role = "reference",
        theta = c(tka = 0.7, tcl = 1.1, add.sd = 0.25),
        omega = c(
          "omega(eta.ka,eta.ka)" = 0.11,
          "omega(eta.cl,eta.ka)" = 0.03,
          "omega(eta.cl,eta.cl)" = 0.22
        ),
        sigma = c("sigma(eps1,eps1)" = 0.5)
      )
    )
  )

  writeRawResults(rows, tmp)
  raw <- readRawResults(tmp)
  header <- attr(raw, "rawResultsHeader", exact = TRUE)

  expect_equal(names(raw), header$columns)
  expect_equal(nrow(raw), 2L)
  expect_equal(header$block_ranges$theta, c(13L, 15L))
  expect_equal(header$block_ranges$omega, c(16L, 18L))
  expect_equal(header$block_ranges$sigma, c(19L, 19L))
  expect_equal(raw[["omega(eta.cl,eta.ka)"]][[2L]], 0.03)
})

test_that("setupRawResultsFilter supports PsN-style strings and formulas", {
  raw <- data.frame(
    minimization_successful = c(1L, 0L, 1L),
    significant_digits = c(4.2, 5.1, 2.0)
  )

  psn_filter <- setupRawResultsFilter(
    "minimization_successful.eq.1,significant_digits.gt.3.5"
  )
  formula_filter <- setupRawResultsFilter(
    ~ minimization_successful == 1 & significant_digits > 3.5
  )

  expect_equal(psn_filter(raw), c(TRUE, FALSE, FALSE))
  expect_equal(formula_filter(raw), c(TRUE, FALSE, FALSE))
})

test_that("readRawResults rejects future schema versions", {
  fit <- .mockRawResultsFit()
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  rows <- rawResultsRow(
    fit,
    source = "bootstrap",
    hypothesis = "reference",
    sample = 0L,
    modelLabel = "reference",
    role = "reference"
  )
  writeRawResults(rows, tmp)

  header_path <- file.path(tmp, "raw_results_header.json")
  header <- jsonlite::fromJSON(header_path, simplifyVector = TRUE)
  header$schema_version <- 999L
  writeLines(
    jsonlite::toJSON(header, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    con = header_path
  )

  err <- tryCatch(
    {
      readRawResults(tmp)
      NULL
    },
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "newer than this package understands")
})

test_that("parseRawResultsParams rebuilds theta, omega, and sigma by sample", {
  fit <- .mockRawResultsFit()
  tmp <- .testDir()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  rows <- do.call(
    rbind,
    list(
      rawResultsRow(
        fit,
        source = "bootstrap",
        hypothesis = "reference",
        sample = 0L,
        modelLabel = "reference",
        role = "reference"
      ),
      rawResultsRow(
        fit,
        source = "bootstrap",
        hypothesis = "sample",
        sample = 1L,
        modelLabel = "reference",
        role = "reference",
        theta = c(tka = 0.7, tcl = 1.1, add.sd = 0.25),
        omega = c(
          "omega(eta.ka,eta.ka)" = 0.11,
          "omega(eta.cl,eta.ka)" = 0.03,
          "omega(eta.cl,eta.cl)" = 0.22
        ),
        sigma = c("sigma(eps1,eps1)" = 0.5)
      ),
      rawResultsRow(
        fit,
        source = "bootstrap",
        hypothesis = "sample",
        sample = 2L,
        modelLabel = "reference",
        role = "reference",
        theta = c(tka = 0.8, tcl = 1.0, add.sd = 0.28),
        omega = c(
          "omega(eta.ka,eta.ka)" = 0.12,
          "omega(eta.cl,eta.ka)" = 0.04,
          "omega(eta.cl,eta.cl)" = 0.21
        ),
        sigma = c("sigma(eps1,eps1)" = 0.45)
      )
    )
  )
  writeRawResults(rows, tmp)

  params <- parseRawResultsParams(
    tmp,
    fit,
    offset = 1L,
    filter = "minimization_successful.eq.1"
  )

  expect_length(params, 2L)
  expect_equal(params[[1L]]$sample, 1L)
  expect_equal(
    unname(params[[1L]]$theta[c("tka", "tcl", "add.sd")]),
    c(0.7, 1.1, 0.25)
  )
  expect_equal(params[[2L]]$omega[2, 1], 0.04)
  expect_equal(params[[2L]]$sigma[1, 1], 0.45)
})

test_that("rawResultsSchema works on the shipped theoFitOde example", {
  data("theoFitOde", package = "nlmixr2utils", envir = environment())

  schema <- suppressWarnings(rawResultsSchema(theoFitOde))

  expect_true(all(c("tka", "tcl", "tv", "add.sd") %in% schema$thetaCols))
  expect_true(any(grepl("^omega\\(", schema$omegaCols)))
})

test_that("raw-results errors interpolate values from the calling frame", {
  # `.abortRawResults()` used to let `cli_abort()` default `.envir` to its own
  # frame, so every `{var}` in a message resolved against the wrapper instead of
  # the caller. Messages became "could not evaluate cli expression" (or, for
  # names that shadow a base object such as `missing`, an unrelated coercion
  # error), hiding the real diagnostic.
  emptyDir <- file.path(tempdir(), "nlmixr2utils-empty-rawres")
  dir.create(emptyDir, showWarnings = FALSE)
  on.exit(unlink(emptyDir, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(
    readRawResults(emptyDir),
    "Could not find",
    fixed = TRUE
  )
  expect_error(
    .rawResultsFilterColumns(c("nope", "nah"), "objf"),
    'unknown raw-results columns "nope" and "nah"',
    fixed = TRUE
  )
  expect_error(
    .coerceFilterResult(1:3, 5L),
    "logical vector of length 5",
    fixed = TRUE
  )
  expect_error(
    .vectorWithNames(c(1, 2), c("a", "b", "c"), "theta"),
    "must be named or have length 3",
    fixed = TRUE
  )
  # `missing` shadows base::missing, the case that produced the most misleading
  # error of all.
  expect_error(
    .normalizeSchemaList(list(columns = 1)),
    "missing required fields",
    fixed = TRUE
  )
})

test_that("raw-results errors are attributed to the calling function", {
  # Threading `.envir` also makes `cli_abort()` default `call` to the caller's
  # frame, so the error names the function the user actually invoked rather
  # than the internal `.abortRawResults()` wrapper.
  err <- tryCatch(
    rawResultsRow(list(), source = 1),
    error = function(e) e
  )
  # Base R rather than rlang::call_name(): rlang is not a declared dependency,
  # and `R CMD check --as-cran` raises "unstated dependencies in 'tests'" for a
  # bare `::` into an undeclared package, which CI promotes to a failure.
  expect_equal(as.character(conditionCall(err)[[1L]]), "rawResultsRow")
})

test_that("a parameterless fit yields no se columns", {
  # paste0() drops zero-length arguments instead of returning zero length, so
  # paste0(character(0), ".se") is ".se". A fit with no estimated parameters
  # therefore gained a phantom ".se" column, and rows built from that schema had
  # a width no other schema could rbind against.
  empty <- list(
    theta = numeric(0),
    omega = matrix(numeric(0), 0L, 0L),
    sigma = matrix(numeric(0), 0L, 0L),
    iniDf = data.frame(),
    parFixedDf = data.frame()
  )

  schema <- rawResultsSchema(empty)

  expect_equal(schema$seCols, character(0))
  expect_false(".se" %in% schema$columns)
  expect_equal(schema$columns, schema$baseCols)
})
