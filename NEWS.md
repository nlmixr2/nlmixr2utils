# nlmixr2utils (development version)

* `.plap()` runs every item inside the rxode2 event scope (when rxode2 has an
  event bus), in the main process, in the `lapply()` fallback and in the
  workers of a `future` plan set before the call.  Fits and solves made by
  the items are then silent for loggers such as nlmixr2log, so a parallel
  driver (`runSCM()`, `runSIR()`) logs one entry instead of one run per
  internal fit.  Nothing changes without a listener.

* New vignette `vignette("events")` for driver authors: how `.plap()` scopes
  its items on the rxode2 event bus and how a driver reports one result.

# nlmixr2utils 0.3.1

* Fixed `rawResultsSchema()` and `.schemaHeader()` emitting a phantom `".se"`
  column for a fit with no estimated parameters. `paste0()` drops zero-length
  arguments rather than returning zero length, so `paste0(character(0), ".se")`
  is `".se"`, not `character(0)`. Rows built from such a schema carried a column
  nothing else could `rbind()` against.
* Fixed the internal `.abortRawResults()` and `.abortRunCache()` error
  helpers, which passed their message straight to `cli::cli_abort()` without
  threading `.envir`. Because `cli_abort()` defaults `.envir` to
  `parent.frame()` — the helper's own frame, not the caller's — every `{var}`
  interpolation resolved against the wrapper and failed. All 16 affected
  messages reported `Could not evaluate cli {} expression` instead of the real
  diagnostic, and one (`{missing}`, which shadows `base::missing`) degraded
  further into `cannot coerce type 'special' to vector of type 'character'`.
  Validation failures in `readRawResults()`, `parseRawResultsParams()`,
  `setupRawResultsFilter()`, `rawResultsRow()`, `readRunState()`, and
  `taskCache()$get()` now report what actually went wrong, and name the
  function the user called rather than the internal helper.
* Fixed `rawResultsRow()`'s `minimization_successful` column, which
  previously reported failure (`0`) for essentially every real fit. The
  fallback heuristic treated any non-empty `fit$message` as a failure
  signal, but `nlmixr2est` fit objects populate `$message` with the
  optimizer's exit text on success too (e.g. `"Normal exit from bobyqa"`),
  and never carry the `$minimization_successful` field the primary check
  looked for. The fallback now uses `fit$convergence` (0 = success,
  mirroring the underlying optimizer's `ierr`/`status` code) instead.

# nlmixr2utils 0.3

* `.withWorkerPlan()` now guards against requesting more parallel OS threads
  than the machine has: whenever more than one worker is involved, it
  checks the effective worker count (from `workers`, or from the ambient
  `future` plan when `workers = NULL`) times the effective rxode2 thread
  count per worker (from the new `rxThreads` argument, or from
  `rxode2::getRxThreads()` when unspecified) against the total physical
  core count (`parallel::detectCores()`), and aborts with a clear message
  before evaluating anything if the product would exceed it. A single
  worker is never subject to this check.
* **Breaking change:** because rxode2's own default thread count was never
  designed with the assumption that multiple worker processes would each
  run a copy of it, existing calls to `.withWorkerPlan()` with `workers >
  1` or `workers = "auto"` that do not also set `rxThreads` will, on most
  real multi-core machines, now abort where they previously ran silently
  oversubscribed. Set `rxThreads` explicitly (e.g. `rxThreads = 1`) to
  restore the prior behavior.
* New `resolveRxThreads(workers, rxThreads)` resolves the effective rxode2
  thread count for a given `workers`/`rxThreads` combination, validating
  both arguments.
* `.plap()` gained an `rxThreads` argument that propagates the resolved
  thread count into every task (whether it runs in the main process or a
  `multisession` worker) and restores each worker's own prior setting
  afterward, so persistent workers reused across calls don't leak state
  between them. `.withWorkerPlan()` also best-effort broadcasts the
  resolved thread count to every worker in the active plan on its own, so
  callers that don't route `rxThreads` through their own `.plap()` calls
  still get an accurate thread count applied.

# nlmixr2utils 0.2

* Promoted the shared raw-results and run-cache helper APIs to stable status now that downstream bootstrap and SIR packages use the common output, restart, and seeding infrastructure.

# nlmixr2utils 0.1

* Added experimental shared raw-results helpers (`rawResultsSchema()`, `rawResultsRow()`, `writeRawResults()`, `readRawResults()`, `parseRawResultsParams()`, and `setupRawResultsFilter()`) plus run-cache helpers (`resolveRunDir()`, `readRunState()`, `writeRunState()`, `taskCache()`, `pendingTasks()`, `withRunSeed()`, and `deriveFitName()`) for downstream nlmixr2 extension packages.

* Initial package largely cloned from `nlmixr2extra`, providing shared worker-plan
  helpers, covariance utilities, equation-printing methods, reexports, and the
  `theoFitOde` package data used by the new extension packages.
