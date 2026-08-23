#' Apply FUN over X with optional parallel execution and progress reporting
#'
#' When \pkg{future.apply} is available, uses \code{future_lapply()} under the
#' current \code{future::plan()}.  If \pkg{progressr} is also available,
#' progress is reported.  Otherwise falls back to \code{base::lapply()}.
#'
#' @param X       vector or list to iterate over
#' @param FUN     function applied to each element of \code{X}
#' @param ...     additional arguments passed to \code{FUN}
#' @param rxThreads optional; when not \code{NULL}, every call to \code{FUN}
#'   captures its own worker's current \code{rxode2::getRxThreads()} value,
#'   sets \code{rxode2::setRxThreads(rxThreads)}, runs \code{FUN}, and
#'   restores the captured value on exit -- applied identically whether the
#'   call runs in the main process or inside a parallel worker, and safe
#'   for persistent \code{multisession} workers reused across separate
#'   \code{.plap()} calls (a later call does not inherit an earlier call's
#'   setting).
#' @param .label  optional \code{function(x) -> character} producing a per-item
#'   progress label; \code{x} is each element of \code{X}
#' @return list of results in the same order as \code{X}
#' @examples
#' .plap(1:3, function(x) x * 2)
#' @export
.plap <- function(X, FUN, ..., rxThreads = NULL, .label = NULL) {
  wrappedFUN <- if (!is.null(rxThreads)) {
    force(rxThreads)
    force(FUN)
    function(x, ...) {
      origThreads <- rxode2::getRxThreads()
      on.exit(rxode2::setRxThreads(origThreads), add = TRUE)
      rxode2::setRxThreads(rxThreads)
      FUN(x, ...)
    }
  } else {
    FUN
  }

  if (!requireNamespace("future.apply", quietly = TRUE)) {
    return(lapply(X, wrappedFUN, ...))
  }

  if (!requireNamespace("progressr", quietly = TRUE)) {
    return(future.apply::future_lapply(
      X,
      wrappedFUN,
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
        wrappedFUN(x, ...)
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

#' Resolve the effective rxode2 threads-per-worker for an rxThreads=
#' specification
#'
#' Validates \strong{both} arguments -- \code{workers} first, then
#' \code{rxThreads} -- so an invalid \code{workers} value is rejected
#' cleanly here rather than reaching \code{.resolveEffectiveWorkers()}'s
#' \code{as.integer()} coercion with garbage input.
#'
#' @param workers the same \code{workers} value passed to the caller's own
#'   \code{workers=} argument -- used only to compute \code{rxThreads =
#'   "auto"}. Note that \code{workers} is validated on \strong{every} call,
#'   even when the branch taken doesn't otherwise use it (\code{rxThreads =
#'   NULL} or an explicit integer) -- so a caller passing an invalid
#'   \code{workers} alongside a perfectly valid \code{rxThreads} still gets
#'   an error about \code{workers}. This is intentional defense-in-depth.
#' @param rxThreads \code{NULL} (use the current \code{rxode2::getRxThreads()}
#'   value), \code{"auto"} (divide the total core count evenly across
#'   \code{workers}, minimum \code{1}), or a positive integer.
#' @return integer; the rxode2 thread count that would actually be used.
#' @examples
#' resolveRxThreads(NULL, NULL)
#' resolveRxThreads(4L, "auto")
#' @export
resolveRxThreads <- function(workers, rxThreads = NULL) {
  .validateWorkers(workers)
  .validateRxThreads(rxThreads)
  if (is.null(rxThreads)) {
    return(as.integer(rxode2::getRxThreads()))
  }
  if (identical(rxThreads, "auto")) {
    # workers was already validated above by .validateWorkers(); this call
    # is not redundant, it resolves NULL/"auto"/integer down to an actual
    # worker count.
    effectiveWorkers <- .resolveEffectiveWorkers(workers)
    totalCores <- .resolveTotalCores()
    if (is.na(totalCores)) {
      # Total core count could not be determined -- degrade to the most
      # conservative value (1L) rather than guessing from rxode2's current
      # thread setting, which can itself be a large, unauthoritative value
      # and would risk exactly the oversubscription "auto" exists to avoid.
      return(1L)
    }
    return(max(1L, as.integer(floor(totalCores / effectiveWorkers))))
  }
  as.integer(rxThreads)
}

#' Temporarily set a future parallel plan for the duration of an expression
#'
#' Also guards against requesting more OS threads than the machine has,
#' whenever more than one worker is involved: the *effective* number of
#' workers (from \code{workers}, or from the ambient \code{future} plan
#' when \code{workers = NULL}) times the *effective* number of rxode2
#' threads per worker (from \code{rxThreads}, or from
#' \code{rxode2::getRxThreads()} when \code{rxThreads = NULL}) must not
#' exceed the total core count, or the call aborts before anything is
#' evaluated or any global state is changed. A single worker (the default,
#' sequential case) is never subject to this check -- it cannot
#' oversubscribe by definition.
#'
#' @param workers \code{NULL} (leave the current plan unchanged),
#'   \code{1} (force sequential), a positive integer (use that many
#'   \code{multisession} workers), or \code{"auto"} (use
#'   \code{future::availableCores(omit = 1)}).
#' @param expr expression to evaluate; the prior plan is always restored on
#'   exit, even if \code{expr} throws an error. The main session's rxode2
#'   thread count is restored the same way; thread counts broadcast into an
#'   \emph{ambient} plan's existing workers (i.e. when \code{workers = NULL})
#'   are not reverted, since this function never created or owns that plan.
#' @param rxThreads \code{NULL} (use the current \code{rxode2::getRxThreads()}
#'   value), \code{"auto"} (divide the total core count evenly across the
#'   effective worker count), or a positive integer -- the rxode2 thread
#'   count applied in the main session for the duration of \code{expr}, and
#'   best-effort broadcast into every worker of the active plan (whether
#'   newly created by this call or already ambient). \code{.plap()}'s own
#'   \code{rxThreads} argument remains the authoritative per-task mechanism
#'   for callers that use it; this broadcast exists so callers that do not
#'   still get an accurate thread count applied.
#' @return value of \code{expr}
#' @examples
#' .withWorkerPlan(NULL, 1 + 1)
#' @export
.withWorkerPlan <- function(workers, expr, rxThreads = NULL) {
  effectiveRxThreads <- resolveRxThreads(workers, rxThreads)
  effectiveWorkers <- .resolveEffectiveWorkers(workers)
  totalCores <- .resolveTotalCores()
  requested <- as.double(effectiveWorkers) * as.double(effectiveRxThreads)

  cli::cli_inform(c(
    "i" = "Workers                 : {effectiveWorkers}",
    "i" = "rxode2 threads / worker : {effectiveRxThreads}",
    "i" = "Total threads requested : {requested}",
    "i" = "Cores available         : {if (is.na(totalCores)) 'unknown' else totalCores}"
  ))

  # A single worker cannot oversubscribe -- rxode2's own thread count there
  # is already bounded by this machine's hardware, independent of this
  # feature.
  if (
    effectiveWorkers > 1L &&
      !is.na(totalCores) &&
      requested > as.double(totalCores)
  ) {
    cli::cli_abort(c(
      "!" = paste0(
        "Requested {effectiveWorkers} worker{?s} x {effectiveRxThreads} ",
        "rxode2 thread{?s} = {requested} threads, but only {totalCores} ",
        "core{?s} {?is/are} available."
      ),
      "i" = paste0(
        "Lower {.arg workers}, or set {.arg rxThreads} (e.g. ",
        "{.code rxThreads = 1}) so their product is <= {totalCores}."
      )
    ))
  }

  origThreads <- rxode2::getRxThreads()
  on.exit(rxode2::setRxThreads(origThreads), add = TRUE)

  if (is.null(workers)) {
    rxode2::setRxThreads(effectiveRxThreads)
    if (effectiveWorkers > 1L) {
      .broadcastRxThreads(effectiveRxThreads, effectiveWorkers)
    }
    return(force(expr))
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    cli::cli_warn(c(
      "!" = "Package {.pkg future} is not installed.",
      "i" = "Ignoring {.arg workers} and running sequentially."
    ))
    rxode2::setRxThreads(effectiveRxThreads)
    return(force(expr))
  }
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  if (effectiveWorkers == 1L) {
    future::plan("sequential")
  } else {
    future::plan("multisession", workers = effectiveWorkers)
  }
  rxode2::setRxThreads(effectiveRxThreads)
  if (effectiveWorkers > 1L) {
    .broadcastRxThreads(effectiveRxThreads, effectiveWorkers)
  }
  force(expr)
}

#' Broadcast an rxode2 thread-count setting to every worker in the current
#' future plan
#'
#' Best-effort: dispatches exactly \code{nWorkers} tasks under the current
#' plan, each setting \code{rxode2::setRxThreads(rxThreads)} in whichever
#' worker process it lands on. \code{future}'s default round-robin
#' scheduling makes each of \code{nWorkers} tasks land on a distinct worker
#' in the common case, so this closes the propagation gap for callers that
#' dispatch their own tasks without separately passing \code{rxThreads} to
#' \code{.plap()} -- it is not a hard guarantee for every possible
#' scheduling order (wrapped in \code{tryCatch()}, never blocks the run on
#' failure), so \code{.plap(rxThreads = ...)} remains the authoritative
#' per-task mechanism for callers that use it.
#' @param rxThreads thread count to set in each worker
#' @param nWorkers number of workers in the current plan
#' @return invisible NULL
#' @noRd
.broadcastRxThreads <- function(rxThreads, nWorkers) {
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    return(invisible(NULL))
  }
  tryCatch(
    future.apply::future_lapply(
      seq_len(nWorkers),
      function(i) {
        rxode2::setRxThreads(rxThreads)
        NULL
      },
      future.seed = FALSE,
      future.packages = "rxode2"
    ),
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
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
