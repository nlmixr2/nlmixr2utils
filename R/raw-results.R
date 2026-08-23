# Canonical raw-results helpers shared across nlmixr2 extension packages.

.rawResultsSchemaVersion <- 1L

.rawResultsBaseCols <- c(
  "source",
  "hypothesis",
  "sample",
  "model_label",
  "role",
  "minimization_successful",
  "covariance_step_successful",
  "estimate_near_boundary",
  "significant_digits",
  "condition_number",
  "objf",
  "error_message"
)

.abortRawResults <- function(...) {
  cli::cli_abort(c("!" = ...))
}

.isScalarCharacter <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

.normalizeSchemaList <- function(schema) {
  needed <- c("columns", "thetaCols", "omegaCols", "sigmaCols", "seCols")
  missing <- setdiff(needed, names(schema))
  if (length(missing) > 0L) {
    .abortRawResults(
      "Schema is missing required field{?s} {.val {missing}}."
    )
  }
  schema$baseCols <- if (!is.null(schema$baseCols)) {
    schema$baseCols
  } else {
    .rawResultsBaseCols
  }
  schema$schemaVersion <- if (!is.null(schema$schemaVersion)) {
    as.integer(schema$schemaVersion[[1L]])
  } else {
    .rawResultsSchemaVersion
  }
  schema
}

.thetaNamesFromFit <- function(fit) {
  theta <- fit$theta
  if (!is.null(theta) && length(theta) > 0L && !is.null(names(theta))) {
    return(names(theta))
  }

  iniDf <- fit$iniDf
  if (is.data.frame(iniDf) && "ntheta" %in% names(iniDf)) {
    thetaRows <- !is.na(iniDf$ntheta)
    if ("fix" %in% names(iniDf)) {
      thetaFixed <- !is.na(iniDf$fix) & iniDf$fix
      thetaRows <- thetaRows & !thetaFixed
    }
    thetaNames <- iniDf$name[thetaRows]
    thetaNames <- thetaNames[!is.na(thetaNames) & nzchar(thetaNames)]
    if (length(thetaNames) > 0L) {
      return(thetaNames)
    }
  }

  parDf <- fit$parFixedDf
  if (
    is.data.frame(parDf) && nrow(parDf) > 0L && !is.null(rownames(parDf))
  ) {
    thetaNames <- rownames(parDf)
    thetaNames <- thetaNames[!is.na(thetaNames) & nzchar(thetaNames)]
    if (length(thetaNames) > 0L) {
      return(thetaNames)
    }
  }

  character(0)
}

.thetaValuesFromFit <- function(fit, thetaNames = .thetaNamesFromFit(fit)) {
  vals <- rep(NA_real_, length(thetaNames))
  names(vals) <- thetaNames

  theta <- fit$theta
  if (!is.null(theta) && length(theta) > 0L) {
    theta <- as.numeric(theta)
    names(theta) <- names(fit$theta)
    common <- intersect(thetaNames, names(theta))
    vals[common] <- unname(theta[common])
  }

  vals
}

.thetaSeFromFit <- function(fit, thetaNames = .thetaNamesFromFit(fit)) {
  vals <- rep(NA_real_, length(thetaNames))
  names(vals) <- thetaNames

  parDf <- fit$parFixedDf
  if (
    is.data.frame(parDf) &&
      nrow(parDf) > 0L &&
      "SE" %in% names(parDf) &&
      !is.null(rownames(parDf))
  ) {
    common <- intersect(thetaNames, rownames(parDf))
    vals[common] <- as.numeric(parDf[common, "SE", drop = TRUE])
    return(vals)
  }

  covMat <- fit$cov
  if (
    is.matrix(covMat) && nrow(covMat) > 0L && nrow(covMat) == ncol(covMat)
  ) {
    se <- sqrt(diag(covMat))
    common <- intersect(thetaNames, names(se))
    vals[common] <- unname(se[common])
  }

  vals
}

.matrixCoordLabel <- function(
  prefix,
  row,
  col,
  rowName = NULL,
  colName = NULL
) {
  rowLab <- if (!is.null(rowName) && !is.na(rowName) && nzchar(rowName)) {
    rowName
  } else {
    as.character(row)
  }
  colLab <- if (!is.null(colName) && !is.na(colName) && nzchar(colName)) {
    colName
  } else {
    as.character(col)
  }
  paste0(prefix, "(", rowLab, ",", colLab, ")")
}

.matrixInfo <- function(mat, prefix) {
  if (
    !is.matrix(mat) || length(mat) == 0L || nrow(mat) == 0L || ncol(mat) == 0L
  ) {
    return(data.frame(
      colName = character(0),
      row = integer(0),
      col = integer(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  rowNames <- rownames(mat)
  colNames <- colnames(mat)
  idx <- which(lower.tri(mat, diag = TRUE), arr.ind = TRUE)
  idx <- idx[order(idx[, "col"], idx[, "row"]), , drop = FALSE]

  data.frame(
    colName = vapply(
      seq_len(nrow(idx)),
      function(i) {
        .matrixCoordLabel(
          prefix = prefix,
          row = idx[i, "row"],
          col = idx[i, "col"],
          rowName = if (!is.null(rowNames)) {
            rowNames[[idx[i, "row"]]]
          } else {
            NULL
          },
          colName = if (!is.null(colNames)) {
            colNames[[idx[i, "col"]]]
          } else {
            NULL
          }
        )
      },
      character(1)
    ),
    row = idx[, "row"],
    col = idx[, "col"],
    value = mat[idx],
    stringsAsFactors = FALSE
  )
}

.omegaInfoFromFit <- function(fit) {
  omegaMat <- fit$omega
  if (!is.matrix(omegaMat) || nrow(omegaMat) == 0L || ncol(omegaMat) == 0L) {
    return(.matrixInfo(matrix(numeric(0), 0, 0), "omega"))
  }

  iniDf <- fit$iniDf
  if (
    !is.data.frame(iniDf) ||
      !all(c("neta1", "neta2", "name") %in% names(iniDf))
  ) {
    return(.matrixInfo(omegaMat, "omega"))
  }

  omegaRows <- iniDf[!is.na(iniDf$neta1), , drop = FALSE]
  if (nrow(omegaRows) == 0L) {
    return(.matrixInfo(matrix(numeric(0), 0, 0), "omega"))
  }

  omegaFixed <- if ("fix" %in% names(omegaRows)) {
    !is.na(omegaRows$fix) & omegaRows$fix
  } else {
    rep(FALSE, nrow(omegaRows))
  }
  lowerRows <- omegaRows[
    !omegaFixed & omegaRows$neta1 >= omegaRows$neta2,
    ,
    drop = FALSE
  ]
  if (nrow(lowerRows) == 0L) {
    return(.matrixInfo(matrix(numeric(0), 0, 0), "omega"))
  }
  lowerRows <- lowerRows[
    order(lowerRows$neta2, lowerRows$neta1),
    ,
    drop = FALSE
  ]

  diagRows <- omegaRows[omegaRows$neta1 == omegaRows$neta2, , drop = FALSE]
  idxToName <- stats::setNames(
    diagRows$name,
    as.character(diagRows$neta1)
  )
  rowNames <- rownames(omegaMat)
  colNames <- colnames(omegaMat)

  data.frame(
    colName = vapply(
      seq_len(nrow(lowerRows)),
      function(i) {
        r <- lowerRows$neta1[[i]]
        cIdx <- lowerRows$neta2[[i]]
        .matrixCoordLabel(
          prefix = "omega",
          row = r,
          col = cIdx,
          rowName = if (!is.null(idxToName[[as.character(r)]])) {
            idxToName[[as.character(r)]]
          } else if (!is.null(rowNames) && length(rowNames) >= r) {
            rowNames[[r]]
          } else {
            NULL
          },
          colName = if (!is.null(idxToName[[as.character(cIdx)]])) {
            idxToName[[as.character(cIdx)]]
          } else if (!is.null(colNames) && length(colNames) >= cIdx) {
            colNames[[cIdx]]
          } else {
            NULL
          }
        )
      },
      character(1)
    ),
    row = as.integer(lowerRows$neta1),
    col = as.integer(lowerRows$neta2),
    value = vapply(
      seq_len(nrow(lowerRows)),
      function(i) {
        omegaMat[lowerRows$neta1[[i]], lowerRows$neta2[[i]]]
      },
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}

.sigmaInfoFromFit <- function(fit) {
  .matrixInfo(fit$sigma, "sigma")
}

.schemaHeader <- function(schema) {
  totalParam <- c(schema$thetaCols, schema$omegaCols, schema$sigmaCols)
  blockRange <- function(cols, startIndex) {
    if (length(cols) == 0L) {
      return(NULL)
    }
    c(as.integer(startIndex), as.integer(startIndex + length(cols) - 1L))
  }

  thetaStart <- length(schema$baseCols) + 1L
  omegaStart <- thetaStart + length(schema$thetaCols)
  sigmaStart <- omegaStart + length(schema$omegaCols)
  seStart <- sigmaStart + length(schema$sigmaCols)

  list(
    schema_version = as.integer(schema$schemaVersion),
    columns = unname(schema$columns),
    base_cols = unname(schema$baseCols),
    theta_cols = unname(schema$thetaCols),
    omega_cols = unname(schema$omegaCols),
    sigma_cols = unname(schema$sigmaCols),
    se_cols = unname(schema$seCols),
    block_ranges = list(
      base = blockRange(schema$baseCols, 1L),
      theta = blockRange(schema$thetaCols, thetaStart),
      omega = blockRange(schema$omegaCols, omegaStart),
      sigma = blockRange(schema$sigmaCols, sigmaStart),
      se = blockRange(schema$seCols, seStart)
    ),
    parameter_cols = unname(totalParam)
  )
}

.schemaFromHeader <- function(header) {
  .normalizeSchemaList(list(
    columns = header$columns,
    baseCols = if (!is.null(header$base_cols)) {
      header$base_cols
    } else {
      .rawResultsBaseCols
    },
    thetaCols = if (!is.null(header$theta_cols)) {
      header$theta_cols
    } else {
      character(0)
    },
    omegaCols = if (!is.null(header$omega_cols)) {
      header$omega_cols
    } else {
      character(0)
    },
    sigmaCols = if (!is.null(header$sigma_cols)) {
      header$sigma_cols
    } else {
      character(0)
    },
    seCols = if (!is.null(header$se_cols)) header$se_cols else character(0),
    schemaVersion = if (!is.null(header$schema_version)) {
      header$schema_version
    } else {
      .rawResultsSchemaVersion
    }
  ))
}

.inferHeaderFromRows <- function(rows) {
  if (!is.data.frame(rows)) {
    .abortRawResults("{.arg rows} must be a data frame.")
  }

  cols <- names(rows)
  missingBase <- setdiff(.rawResultsBaseCols, cols)
  if (length(missingBase) > 0L) {
    .abortRawResults(
      "Raw-results rows are missing required base column{?s} {.val {missingBase}}."
    )
  }

  nonBase <- setdiff(cols, .rawResultsBaseCols)
  seCols <- nonBase[grepl("\\.se$", nonBase)]
  paramCols <- setdiff(nonBase, seCols)
  thetaCols <- paramCols[
    !grepl("^omega\\(", paramCols) &
      !grepl("^sigma\\(", paramCols)
  ]
  omegaCols <- paramCols[grepl("^omega\\(", paramCols)]
  sigmaCols <- paramCols[grepl("^sigma\\(", paramCols)]

  orderedParam <- c(thetaCols, omegaCols, sigmaCols)
  orderedSe <- paste0(orderedParam, ".se")
  orderedCols <- c(.rawResultsBaseCols, orderedParam, orderedSe)

  .schemaHeader(.normalizeSchemaList(list(
    columns = orderedCols,
    thetaCols = thetaCols,
    omegaCols = omegaCols,
    sigmaCols = sigmaCols,
    seCols = orderedSe
  )))
}

.canonicalizeRows <- function(rows, header = NULL) {
  if (!is.data.frame(rows)) {
    .abortRawResults("{.arg rows} must be a data frame.")
  }

  header <- if (is.null(header)) {
    .inferHeaderFromRows(rows)
  } else {
    header
  }
  schema <- .schemaFromHeader(header)
  missingCols <- setdiff(schema$columns, names(rows))
  if (length(missingCols) > 0L) {
    for (col in missingCols) {
      rows[[col]] <- NA
    }
  }
  rows <- rows[, schema$columns, drop = FALSE]
  attr(rows, "rawResultsHeader") <- .schemaHeader(schema)
  class(rows) <- unique(c("nlmixr2RawResults", class(rows)))
  rows
}

.maybeReadRawResults <- function(rawres) {
  if (is.character(rawres) && length(rawres) == 1L) {
    return(readRawResults(rawres))
  }
  rawres
}

.rawResultsFilterColumns <- function(cols, available) {
  missingCols <- setdiff(unique(cols), available)
  if (length(missingCols) > 0L) {
    .abortRawResults(
      "Filter references unknown raw-results column{?s} {.val {missingCols}}."
    )
  }
}

.coerceFilterResult <- function(x, nExpected) {
  if (!is.logical(x) || length(x) != nExpected) {
    .abortRawResults(
      "Raw-results filter must return a logical vector of length {nExpected}."
    )
  }
  x[is.na(x)] <- FALSE
  x
}

.parsePsnValue <- function(x) {
  if (grepl("^['\"].*['\"]$", x)) {
    return(sub("^['\"](.*)['\"]$", "\\1", x))
  }
  utils::type.convert(x, as.is = TRUE)
}

.vectorWithNames <- function(x, targetNames, arg) {
  if (is.null(x)) {
    return(stats::setNames(numeric(0), character(0)))
  }
  nms <- names(x)
  x <- as.numeric(x)
  names(x) <- nms
  if (!is.null(names(x))) {
    return(x)
  }
  if (length(x) != length(targetNames)) {
    .abortRawResults(
      "{.arg {arg}} must be named or have length {length(targetNames)}."
    )
  }
  stats::setNames(x, targetNames)
}

.extractConditionNumber <- function(fit) {
  for (field in c(
    "conditionNumberCov",
    "conditionNumberCor",
    "conditionNumber"
  )) {
    value <- fit[[field]]
    if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
      return(as.numeric(value))
    }
  }
  NA_real_
}

.extractObjf <- function(fit) {
  value <- fit$objf
  if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
    return(as.numeric(value))
  }
  NA_real_
}

.guessMinSuccess <- function(fit, objf) {
  flag <- fit$minimization_successful
  if (length(flag) == 1L && !is.na(flag)) {
    return(as.integer(flag))
  }
  msg <- fit$message
  if (is.character(msg) && length(msg) > 0L && nzchar(msg[[1L]])) {
    return(0L)
  }
  if (is.finite(objf)) {
    return(1L)
  }
  NA_integer_
}

.guessCovSuccess <- function(fit) {
  flag <- fit$covariance_step_successful
  if (length(flag) == 1L && !is.na(flag)) {
    return(as.integer(flag))
  }
  covMat <- fit$cov
  if (
    is.matrix(covMat) && nrow(covMat) > 0L && nrow(covMat) == ncol(covMat)
  ) {
    return(1L)
  }
  if (is.null(covMat) || length(covMat) == 0L) {
    return(0L)
  }
  NA_integer_
}

.guessBoundary <- function(fit) {
  flag <- fit$estimate_near_boundary
  if (length(flag) == 1L && !is.na(flag)) {
    return(as.integer(flag))
  }
  NA_integer_
}

.guessSigDigits <- function(fit) {
  digits <- fit$significant_digits
  if (is.numeric(digits) && length(digits) == 1L && !is.na(digits)) {
    return(as.numeric(digits))
  }
  NA_real_
}

#' Canonical raw-results helpers
#'
#' These helpers define the shared per-fit raw-results schema used by the
#' `nlmixr2` extension packages. Writers emit the canonical column order,
#' optional standard-error columns, and a JSON sidecar describing the block
#' boundaries so downstream readers do not need to re-parse parameter labels.
#'
#' @name raw-results
#' @section Lifecycle:
#' Stable.
NULL

#' @rdname raw-results
#' @param fit A fitted `nlmixr2` object, or a fit-like list containing `theta`,
#'   `omega`, `sigma`, and related metadata.
#' @return `rawResultsSchema()` returns a list with `columns`, `baseCols`,
#'   `thetaCols`, `omegaCols`, `sigmaCols`, `seCols`, and `schemaVersion`.
#' @export
rawResultsSchema <- function(fit) {
  thetaCols <- .thetaNamesFromFit(fit)
  omegaInfo <- .omegaInfoFromFit(fit)
  sigmaInfo <- .sigmaInfoFromFit(fit)
  paramCols <- c(thetaCols, omegaInfo$colName, sigmaInfo$colName)
  seCols <- paste0(paramCols, ".se")

  .normalizeSchemaList(list(
    columns = c(.rawResultsBaseCols, paramCols, seCols),
    thetaCols = thetaCols,
    omegaCols = omegaInfo$colName,
    sigmaCols = sigmaInfo$colName,
    seCols = seCols
  ))
}

#' @rdname raw-results
#' @param source Canonical producer name such as `"bootstrap"`, `"sir"`, or
#'   `"sse"`.
#' @param hypothesis Hypothesis label recorded in the raw-results row.
#' @param sample Integer replicate index. Use `0` for a reference row.
#' @param modelLabel Short model label stored in `model_label`.
#' @param role Role label stored in `role`.
#' @param errorMessage Optional error message recorded for failed fits.
#' @param objf Optional objective-function value override.
#' @param minimizationSuccessful,covarianceStepSuccessful,estimateNearBoundary
#'   Optional diagnostic flag overrides. When `NULL`, `rawResultsRow()` uses
#'   simple heuristics based on the supplied fit.
#' @param significantDigits Optional significant-digits override.
#' @param conditionNumber Optional condition-number override.
#' @param theta,omega,sigma Optional parameter overrides. `theta` may be a
#'   named vector or an unnamed vector in schema order. `omega` and `sigma`
#'   may be named vectors, unnamed vectors in schema order, or full matrices.
#' @param se Optional standard-error overrides in parameter-column order.
#' @param schema Optional schema list from [rawResultsSchema()]. Supply this to
#'   target a wider union schema than the one implied by `fit`.
#' @return `rawResultsRow()` returns a one-row data frame in canonical schema
#'   order.
#' @export
rawResultsRow <- function(
  fit = NULL,
  source,
  hypothesis,
  sample,
  modelLabel,
  role,
  errorMessage = NA_character_,
  objf = NULL,
  minimizationSuccessful = NULL,
  covarianceStepSuccessful = NULL,
  estimateNearBoundary = NULL,
  significantDigits = NULL,
  conditionNumber = NULL,
  theta = NULL,
  omega = NULL,
  sigma = NULL,
  se = NULL,
  schema = NULL
) {
  if (!.isScalarCharacter(source)) {
    .abortRawResults("{.arg source} must be a single string.")
  }
  if (!.isScalarCharacter(hypothesis)) {
    .abortRawResults("{.arg hypothesis} must be a single string.")
  }
  if (!is.numeric(sample) || length(sample) != 1L || is.na(sample)) {
    .abortRawResults("{.arg sample} must be a single numeric value.")
  }
  if (!.isScalarCharacter(modelLabel)) {
    .abortRawResults("{.arg modelLabel} must be a single string.")
  }
  if (!.isScalarCharacter(role)) {
    .abortRawResults("{.arg role} must be a single string.")
  }

  if (is.null(schema)) {
    if (is.null(fit)) {
      .abortRawResults(
        "Supply either {.arg fit} or {.arg schema} to build a raw-results row."
      )
    }
    schema <- rawResultsSchema(fit)
  } else {
    schema <- .normalizeSchemaList(schema)
  }

  row <- as.list(stats::setNames(
    rep(NA, length(schema$columns)),
    schema$columns
  ))
  row$source <- source
  row$hypothesis <- hypothesis
  row$sample <- as.integer(sample)
  row$model_label <- modelLabel
  row$role <- role

  fitTheta <- if (!is.null(fit)) {
    .thetaValuesFromFit(fit, schema$thetaCols)
  } else {
    stats::setNames(rep(NA_real_, length(schema$thetaCols)), schema$thetaCols)
  }
  fitOmega <- if (!is.null(fit)) {
    stats::setNames(
      .omegaInfoFromFit(fit)$value,
      .omegaInfoFromFit(fit)$colName
    )
  } else {
    stats::setNames(rep(NA_real_, length(schema$omegaCols)), schema$omegaCols)
  }
  fitSigma <- if (!is.null(fit)) {
    stats::setNames(
      .sigmaInfoFromFit(fit)$value,
      .sigmaInfoFromFit(fit)$colName
    )
  } else {
    stats::setNames(rep(NA_real_, length(schema$sigmaCols)), schema$sigmaCols)
  }
  fitSe <- if (!is.null(fit)) {
    thetaSe <- .thetaSeFromFit(fit, schema$thetaCols)
    stats::setNames(
      c(
        thetaSe,
        rep(NA_real_, length(schema$omegaCols) + length(schema$sigmaCols))
      ),
      c(schema$thetaCols, schema$omegaCols, schema$sigmaCols)
    )
  } else {
    stats::setNames(
      rep(
        NA_real_,
        length(schema$thetaCols) +
          length(schema$omegaCols) +
          length(schema$sigmaCols)
      ),
      c(schema$thetaCols, schema$omegaCols, schema$sigmaCols)
    )
  }

  thetaVals <- .vectorWithNames(theta, schema$thetaCols, "theta")
  if (length(thetaVals) > 0L) {
    fitTheta[intersect(names(thetaVals), schema$thetaCols)] <- thetaVals[
      intersect(names(thetaVals), schema$thetaCols)
    ]
  }

  omegaVals <- if (is.matrix(omega)) {
    info <- .matrixInfo(omega, "omega")
    stats::setNames(info$value, info$colName)
  } else {
    .vectorWithNames(omega, schema$omegaCols, "omega")
  }
  if (length(omegaVals) > 0L) {
    fitOmega[intersect(names(omegaVals), schema$omegaCols)] <- omegaVals[
      intersect(names(omegaVals), schema$omegaCols)
    ]
  }

  sigmaVals <- if (is.matrix(sigma)) {
    info <- .matrixInfo(sigma, "sigma")
    stats::setNames(info$value, info$colName)
  } else {
    .vectorWithNames(sigma, schema$sigmaCols, "sigma")
  }
  if (length(sigmaVals) > 0L) {
    fitSigma[intersect(names(sigmaVals), schema$sigmaCols)] <- sigmaVals[
      intersect(names(sigmaVals), schema$sigmaCols)
    ]
  }

  seVals <- .vectorWithNames(
    se,
    c(schema$thetaCols, schema$omegaCols, schema$sigmaCols),
    "se"
  )
  if (length(seVals) > 0L) {
    fitSe[intersect(names(seVals), names(fitSe))] <- seVals[
      intersect(names(seVals), names(fitSe))
    ]
  }

  for (nm in names(fitTheta)) {
    row[[nm]] <- fitTheta[[nm]]
  }
  for (nm in names(fitOmega)) {
    row[[nm]] <- fitOmega[[nm]]
  }
  for (nm in names(fitSigma)) {
    row[[nm]] <- fitSigma[[nm]]
  }
  for (nm in names(fitSe)) {
    row[[paste0(nm, ".se")]] <- fitSe[[nm]]
  }

  row$minimization_successful <- if (is.null(minimizationSuccessful)) {
    if (is.null(fit)) {
      NA_integer_
    } else {
      .guessMinSuccess(fit, if (is.null(objf)) .extractObjf(fit) else objf)
    }
  } else {
    as.integer(minimizationSuccessful)
  }
  row$covariance_step_successful <- if (is.null(covarianceStepSuccessful)) {
    if (is.null(fit)) NA_integer_ else .guessCovSuccess(fit)
  } else {
    as.integer(covarianceStepSuccessful)
  }
  row$estimate_near_boundary <- if (is.null(estimateNearBoundary)) {
    if (is.null(fit)) NA_integer_ else .guessBoundary(fit)
  } else {
    as.integer(estimateNearBoundary)
  }
  row$significant_digits <- if (is.null(significantDigits)) {
    if (is.null(fit)) NA_real_ else .guessSigDigits(fit)
  } else {
    as.numeric(significantDigits)
  }
  row$condition_number <- if (is.null(conditionNumber)) {
    if (is.null(fit)) NA_real_ else .extractConditionNumber(fit)
  } else {
    as.numeric(conditionNumber)
  }
  row$objf <- if (is.null(objf)) {
    if (is.null(fit)) NA_real_ else .extractObjf(fit)
  } else {
    as.numeric(objf)
  }
  row$error_message <- if (is.null(errorMessage)) {
    NA_character_
  } else {
    as.character(errorMessage)
  }

  out <- as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  .canonicalizeRows(out, .schemaHeader(schema))
}

#' @rdname raw-results
#' @param rows Data frame of canonical raw-results rows.
#' @param dir Directory where the CSV, RDS, and JSON sidecar should be written.
#' @param basename Basename used for the output files.
#' @return `writeRawResults()` invisibly returns a list containing the canonical
#'   data frame and the three output paths.
#' @export
writeRawResults <- function(rows, dir, basename = "raw_results") {
  if (!.isScalarCharacter(dir)) {
    .abortRawResults("{.arg dir} must be a single directory path.")
  }
  if (!.isScalarCharacter(basename)) {
    .abortRawResults("{.arg basename} must be a single string.")
  }

  header <- attr(rows, "rawResultsHeader", exact = TRUE)
  canonical <- .canonicalizeRows(rows, header)
  header <- attr(canonical, "rawResultsHeader", exact = TRUE)

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  csvPath <- file.path(dir, paste0(basename, ".csv"))
  rdsPath <- file.path(dir, paste0(basename, ".rds"))
  headerPath <- file.path(dir, paste0(basename, "_header.json"))

  utils::write.csv(
    canonical,
    csvPath,
    row.names = FALSE,
    na = "NA"
  )
  saveRDS(canonical, rdsPath)
  writeLines(
    jsonlite::toJSON(header, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    con = headerPath,
    useBytes = TRUE
  )

  invisible(list(
    data = canonical,
    csvPath = csvPath,
    rdsPath = rdsPath,
    headerPath = headerPath
  ))
}

.resolveRawResultsPaths <- function(path) {
  if (!.isScalarCharacter(path)) {
    .abortRawResults("{.arg path} must be a single file or directory path.")
  }

  if (dir.exists(path)) {
    base <- file.path(path, "raw_results")
    if (file.exists(paste0(base, ".rds"))) {
      return(list(dataPath = paste0(base, ".rds"), basePath = base))
    }
    if (file.exists(paste0(base, ".csv"))) {
      return(list(dataPath = paste0(base, ".csv"), basePath = base))
    }
    .abortRawResults(
      "Could not find {.file raw_results.csv} or {.file raw_results.rds} in {.path {path}}."
    )
  }

  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "rds")) {
    base <- sub(paste0("\\.", ext, "$"), "", path)
    return(list(dataPath = path, basePath = base))
  }
  if (file.exists(paste0(path, ".rds"))) {
    return(list(dataPath = paste0(path, ".rds"), basePath = path))
  }
  if (file.exists(paste0(path, ".csv"))) {
    return(list(dataPath = paste0(path, ".csv"), basePath = path))
  }

  .abortRawResults(
    "Could not resolve a raw-results file from {.path {path}}."
  )
}

#' @rdname raw-results
#' @param path Path to a canonical raw-results CSV, RDS, or containing
#'   directory.
#' @return `readRawResults()` returns the raw-results data frame with the parsed
#'   header attached as the `rawResultsHeader` attribute.
#' @export
readRawResults <- function(path) {
  paths <- .resolveRawResultsPaths(path)
  headerPath <- paste0(paths$basePath, "_header.json")
  ext <- tolower(tools::file_ext(paths$dataPath))

  data <- if (ext == "rds") {
    readRDS(paths$dataPath)
  } else {
    utils::read.csv(
      paths$dataPath,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = "NA"
    )
  }

  header <- if (file.exists(headerPath)) {
    jsonlite::fromJSON(headerPath, simplifyVector = TRUE)
  } else {
    attr(data, "rawResultsHeader", exact = TRUE)
  }
  if (is.null(header)) {
    cli::cli_warn(c(
      "!" = "Raw-results header sidecar was not found for {.path {paths$dataPath}}.",
      "i" = "Inferring block boundaries from column names."
    ))
    header <- .inferHeaderFromRows(data)
  }

  version <- header$schema_version
  if (is.null(version)) {
    .abortRawResults("Raw-results header is missing {.field schema_version}.")
  }
  versionNum <- as.integer(version[[1L]])
  currentVersion <- .rawResultsSchemaVersion
  if (versionNum > currentVersion) {
    .abortRawResults(
      paste0(
        "Raw-results schema version ",
        versionNum,
        " is newer than this package understands (",
        currentVersion,
        ")."
      )
    )
  }

  data <- .canonicalizeRows(data, header)
  attr(data, "rawResultsPath") <- normalizePath(
    paths$dataPath,
    mustWork = FALSE
  )
  data
}

#' @rdname raw-results
#' @param filter Either a PsN-style character filter, a one-sided formula, or
#'   an unevaluated expression.
#' @return `setupRawResultsFilter()` returns a predicate function that accepts a
#'   raw-results data frame and returns a logical inclusion vector.
#' @export
setupRawResultsFilter <- function(filter) {
  if (is.null(filter)) {
    predicate <- function(rawres) {
      rawres <- .maybeReadRawResults(rawres)
      rep(TRUE, nrow(rawres))
    }
    class(predicate) <- c("nlmixr2RawResultsFilter", class(predicate))
    return(predicate)
  }

  if (is.function(filter)) {
    class(filter) <- unique(c("nlmixr2RawResultsFilter", class(filter)))
    return(filter)
  }

  if (inherits(filter, "formula")) {
    expr <- filter[[2L]]
    env <- environment(filter)
    predicate <- function(rawres) {
      rawres <- .maybeReadRawResults(rawres)
      .rawResultsFilterColumns(all.vars(expr), names(rawres))
      out <- eval(expr, envir = rawres, enclos = env)
      .coerceFilterResult(out, nrow(rawres))
    }
    class(predicate) <- c("nlmixr2RawResultsFilter", class(predicate))
    return(predicate)
  }

  if (is.language(filter)) {
    expr <- filter
    env <- parent.frame()
    predicate <- function(rawres) {
      rawres <- .maybeReadRawResults(rawres)
      .rawResultsFilterColumns(all.vars(expr), names(rawres))
      out <- eval(expr, envir = rawres, enclos = env)
      .coerceFilterResult(out, nrow(rawres))
    }
    class(predicate) <- c("nlmixr2RawResultsFilter", class(predicate))
    return(predicate)
  }

  if (is.character(filter)) {
    parts <- trimws(unlist(
      strsplit(filter, ",", fixed = TRUE),
      use.names = FALSE
    ))
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0L) {
      .abortRawResults("{.arg filter} did not contain any filter clauses.")
    }

    clauses <- lapply(parts, function(part) {
      match <- regexec("^(.*)\\.(eq|ne|gt|ge|lt|le)\\.(.*)$", part)
      parsed <- regmatches(part, match)[[1L]]
      if (length(parsed) != 4L) {
        .abortRawResults(
          "Could not parse PsN-style filter clause {.val {part}}."
        )
      }
      list(
        column = parsed[[2L]],
        op = parsed[[3L]],
        value = .parsePsnValue(parsed[[4L]])
      )
    })

    predicate <- function(rawres) {
      rawres <- .maybeReadRawResults(rawres)
      .rawResultsFilterColumns(
        vapply(clauses, `[[`, character(1), "column"),
        names(rawres)
      )

      out <- rep(TRUE, nrow(rawres))
      for (clause in clauses) {
        lhs <- rawres[[clause$column]]
        rhs <- clause$value
        this <- switch(
          clause$op,
          eq = lhs == rhs,
          ne = lhs != rhs,
          gt = lhs > rhs,
          ge = lhs >= rhs,
          lt = lhs < rhs,
          le = lhs <= rhs
        )
        this[is.na(this)] <- FALSE
        out <- out & this
      }
      out
    }
    class(predicate) <- c("nlmixr2RawResultsFilter", class(predicate))
    return(predicate)
  }

  .abortRawResults(
    "{.arg filter} must be NULL, a function, a formula, an expression, or a character vector."
  )
}

.rebuildLowerTriMatrix <- function(info, values, template, sampleId, kind) {
  if (nrow(info) == 0L) {
    return(template)
  }
  mat <- template
  for (i in seq_len(nrow(info))) {
    nm <- info$colName[[i]]
    value <- values[[nm]]
    if (is.na(value)) {
      .abortRawResults(
        "Sample {.val {sampleId}} is missing required {.val {kind}} parameter {.val {nm}}."
      )
    }
    mat[info$row[[i]], info$col[[i]]] <- value
    mat[info$col[[i]], info$row[[i]]] <- value
  }
  mat
}

#' @rdname raw-results
#' @param rawres A raw-results data frame, a path understood by
#'   [readRawResults()], or the result of [readRawResults()].
#' @param offset Integer sample offset. The default `1L` skips the canonical
#'   reference row with `sample = 0`.
#' @return `parseRawResultsParams()` returns a named list of per-sample
#'   parameter sets, each containing `sample`, `source`, `hypothesis`,
#'   `modelLabel`, `role`, `theta`, `omega`, and `sigma`.
#' @export
parseRawResultsParams <- function(rawres, fit, offset = 1L, filter = NULL) {
  rawres <- .maybeReadRawResults(rawres)
  if (!is.data.frame(rawres)) {
    .abortRawResults("{.arg rawres} must resolve to a data frame.")
  }
  if (
    !is.numeric(offset) || length(offset) != 1L || is.na(offset) || offset < 0
  ) {
    .abortRawResults("{.arg offset} must be a single non-negative number.")
  }

  rawHeader <- attr(rawres, "rawResultsHeader", exact = TRUE)
  if (is.null(rawHeader)) {
    rawHeader <- .inferHeaderFromRows(rawres)
  }
  rawSchema <- .schemaFromHeader(rawHeader)
  fitSchema <- rawResultsSchema(fit)

  missingTheta <- setdiff(fitSchema$thetaCols, rawSchema$thetaCols)
  missingOmega <- setdiff(fitSchema$omegaCols, rawSchema$omegaCols)
  missingSigma <- setdiff(fitSchema$sigmaCols, rawSchema$sigmaCols)
  missingCols <- c(missingTheta, missingOmega, missingSigma)
  if (length(missingCols) > 0L) {
    .abortRawResults(
      "Raw-results input is missing parameter column{?s} {.val {missingCols}} required by the supplied fit."
    )
  }

  rows <- rawres[rawres$sample >= as.integer(offset), , drop = FALSE]
  if (!is.null(filter)) {
    predicate <- if (is.function(filter)) {
      filter
    } else {
      setupRawResultsFilter(filter)
    }
    rows <- rows[predicate(rows), , drop = FALSE]
  }
  if (nrow(rows) == 0L) {
    return(list())
  }

  rows <- rows[order(rows$sample), , drop = FALSE]
  if (anyDuplicated(rows$sample) > 0L) {
    .abortRawResults(
      "More than one raw-results row remains for at least one sample after applying {.arg offset} and {.arg filter}."
    )
  }

  thetaTemplate <- .thetaValuesFromFit(fit, fitSchema$thetaCols)
  omegaTemplate <- fit$omega
  if (!is.matrix(omegaTemplate)) {
    omegaTemplate <- matrix(numeric(0), 0, 0)
  }
  sigmaTemplate <- fit$sigma
  if (!is.matrix(sigmaTemplate)) {
    sigmaTemplate <- matrix(numeric(0), 0, 0)
  }
  omegaInfo <- .omegaInfoFromFit(fit)
  sigmaInfo <- .sigmaInfoFromFit(fit)

  out <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    sampleId <- as.integer(row$sample[[1L]])

    thetaVals <- thetaTemplate
    if (length(fitSchema$thetaCols) > 0L) {
      thetaVals[fitSchema$thetaCols] <- as.numeric(row[
        1,
        fitSchema$thetaCols,
        drop = TRUE
      ])
      if (anyNA(thetaVals[fitSchema$thetaCols])) {
        missingTheta <- fitSchema$thetaCols[is.na(thetaVals[
          fitSchema$thetaCols
        ])]
        .abortRawResults(
          "Sample {.val {sampleId}} is missing required theta parameter{?s} {.val {missingTheta}}."
        )
      }
    }

    omegaVals <- if (length(fitSchema$omegaCols) > 0L) {
      stats::setNames(
        as.numeric(row[1, fitSchema$omegaCols, drop = TRUE]),
        fitSchema$omegaCols
      )
    } else {
      numeric(0)
    }
    sigmaVals <- if (length(fitSchema$sigmaCols) > 0L) {
      stats::setNames(
        as.numeric(row[1, fitSchema$sigmaCols, drop = TRUE]),
        fitSchema$sigmaCols
      )
    } else {
      numeric(0)
    }

    list(
      sample = sampleId,
      source = row$source[[1L]],
      hypothesis = row$hypothesis[[1L]],
      modelLabel = row$model_label[[1L]],
      role = row$role[[1L]],
      theta = thetaVals,
      omega = .rebuildLowerTriMatrix(
        omegaInfo,
        omegaVals,
        omegaTemplate,
        sampleId = sampleId,
        kind = "omega"
      ),
      sigma = .rebuildLowerTriMatrix(
        sigmaInfo,
        sigmaVals,
        sigmaTemplate,
        sampleId = sampleId,
        kind = "sigma"
      )
    )
  })
  names(out) <- paste0("sample_", rows$sample)
  out
}
