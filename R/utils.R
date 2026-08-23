#' Apply FUN over X with optional parallel execution and progress reporting
#'
#' When \pkg{future.apply} is available, uses \code{future_lapply()} under the
#' current \code{future::plan()}.  If \pkg{progressr} is also available,
#' progress is reported.  Otherwise falls back to \code{base::lapply()}.
#'
#' @param X       vector or list to iterate over
#' @param FUN     function applied to each element of \code{X}
#' @param ...     additional arguments passed to \code{FUN}
#' @param .label  optional \code{function(x) -> character} producing a per-item
#'   progress label; \code{x} is each element of \code{X}
#' @return list of results in the same order as \code{X}
#' @examples
#' .plap(1:3, function(x) x * 2)
#' @export
.plap <- function(X, FUN, ..., .label = NULL) {
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    return(lapply(X, FUN, ...))
  }

  if (!requireNamespace("progressr", quietly = TRUE)) {
    return(future.apply::future_lapply(
      X,
      FUN,
      ...,
      future.seed = TRUE,
      future.packages = "nlmixr2utils"
    ))
  }

  progressr::with_progress({
    p <- progressr::progressor(steps = length(X))
    future.apply::future_lapply(
      X,
      function(x, ...) {
        on.exit(p(message = if (!is.null(.label)) .label(x) else ""))
        FUN(x, ...)
      },
      ...,
      future.seed = TRUE,
      future.packages = "nlmixr2utils"
    )
  })
}

#' Validate a worker-plan specification
#'
#' @param workers \code{NULL}, \code{"auto"}, \code{1}, or a positive integer.
#' @return Invisibly returns \code{NULL} when \code{workers} is valid.
#' @examples
#' .validateWorkers(1L)
#' .validateWorkers("auto")
#' @export
.validateWorkers <- function(workers) {
  if (is.null(workers) || identical(workers, "auto")) {
    return(invisible(NULL))
  }
  if (
    !is.numeric(workers) ||
      length(workers) != 1L ||
      is.na(workers) ||
      !is.finite(workers) ||
      workers < 1 ||
      workers != as.integer(workers)
  ) {
    cli::cli_abort(
      "{.arg workers} must be NULL, \"auto\", 1, or a positive integer."
    )
  }
  invisible(NULL)
}

#' Validate an rxode2-threads-per-worker specification
#'
#' @param rxThreads \code{NULL}, \code{"auto"}, \code{1}, or a positive
#'   integer.
#' @return Invisibly returns \code{NULL} when \code{rxThreads} is valid.
#' @examples
#' .validateRxThreads(1L)
#' .validateRxThreads("auto")
#' @export
.validateRxThreads <- function(rxThreads) {
  if (is.null(rxThreads) || identical(rxThreads, "auto")) {
    return(invisible(NULL))
  }
  if (
    !is.numeric(rxThreads) ||
      length(rxThreads) != 1L ||
      is.na(rxThreads) ||
      !is.finite(rxThreads) ||
      rxThreads < 1 ||
      rxThreads != as.integer(rxThreads)
  ) {
    cli::cli_abort(
      "{.arg rxThreads} must be NULL, \"auto\", 1, or a positive integer."
    )
  }
  invisible(NULL)
}

#' Resolve the effective number of parallel workers for a workers=
#' specification, without changing the current future plan
#'
#' @param workers \code{NULL}, \code{"auto"}, or a positive integer -- same
#'   values accepted by \code{.withWorkerPlan()}'s \code{workers} argument.
#' @return integer; the number of workers that would actually be used. For
#'   \code{NULL}, this reflects the number of workers in the *currently
#'   active* \code{future} plan (\code{1L} for a \code{sequential} plan),
#'   not a hypothetical future one.
#' @examples
#' .resolveEffectiveWorkers(4L)
#' .resolveEffectiveWorkers(NULL)
#' @export
.resolveEffectiveWorkers <- function(workers) {
  .validateWorkers(workers)
  if (is.null(workers)) {
    if (requireNamespace("future", quietly = TRUE)) {
      return(as.integer(future::nbrOfWorkers()))
    }
    return(1L)
  }
  if (identical(workers, "auto")) {
    if (requireNamespace("future", quietly = TRUE)) {
      return(max(1L, as.integer(future::availableCores(omit = 1L))))
    }
    return(1L)
  }
  as.integer(workers)
}

#' Resolve the total logical core count for the oversubscription guard
#'
#' Deliberately uses \code{parallel::detectCores()} (the true physical/
#' logical core count) rather than \code{future::availableCores()}, which
#' is policy-adjustable (honors \code{_R_CHECK_LIMIT_CORES_},
#' \code{options(mc.cores=)}, and HPC scheduler allocations) and can report
#' far fewer cores than the machine actually has -- \code{rxode2} does not
#' respect any of those, so checking against a policy-shrunk value produces
#' false-positive guard failures.
#'
#' @return integer core count, or \code{NA_integer_} if it cannot be
#'   determined (the guard is skipped in that case rather than blocking on
#'   an unknown core count).
#' @noRd
.resolveTotalCores <- function() {
  n <- suppressWarnings(as.integer(parallel::detectCores()))
  if (!is.na(n) && n >= 1L) {
    return(n)
  }
  NA_integer_
}

#' Temporarily set a future parallel plan for the duration of an expression
#'
#' @param workers \code{NULL} (leave the current plan unchanged),
#'   \code{1} (force sequential), a positive integer (use that many
#'   \code{multisession} workers), or \code{"auto"} (use
#'   \code{future::availableCores(omit = 1)}).
#' @param expr expression to evaluate; the prior plan is always restored on
#'   exit, even if \code{expr} throws an error.
#' @return value of \code{expr}
#' @examples
#' .withWorkerPlan(NULL, 1 + 1)
#' @export
.withWorkerPlan <- function(workers, expr) {
  .validateWorkers(workers)
  if (is.null(workers)) {
    return(force(expr))
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    cli::cli_warn(c(
      "!" = "Package {.pkg future} is not installed.",
      "i" = "Ignoring {.arg workers} and running sequentially."
    ))
    return(force(expr))
  }
  if (identical(workers, "auto")) {
    workers <- max(1L, as.integer(future::availableCores(omit = 1L)))
  } else {
    workers <- as.integer(workers)
  }
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  if (workers == 1L) {
    future::plan("sequential")
  } else {
    future::plan("multisession", workers = workers)
  }
  force(expr)
}

#' Make an estimation control object quieter and faster
#'
#' `setQuietFastControl()` is a small utility for repeated model evaluations
#' where full iteration printing, covariance estimation, table generation, and
#' object compression are unnecessary. It returns the input control object after
#' forcing a small set of fields to faster, quieter defaults.
#'
#' Specifically, it sets:
#'
#' * `print = 0`
#' * `covMethod = 0`
#' * `calcTables = FALSE`
#' * `compress = FALSE`
#'
#' @param ctl A control object, typically a named list passed to an
#'   `nlmixr2()` estimation routine.
#' @return The modified control object.
#' @examples
#' ctl <- list(
#'   print = 100,
#'   covMethod = "r,s",
#'   calcTables = TRUE,
#'   compress = TRUE
#' )
#'
#' setQuietFastControl(ctl)
#' @export
setQuietFastControl <- function(ctl) {
  # make estimation steps quieter
  ctl$print <- 0L
  # make estimation steps faster
  ctl$covMethod <- 0L
  ctl$calcTables <- FALSE
  ctl$compress <- FALSE
  ctl
}
