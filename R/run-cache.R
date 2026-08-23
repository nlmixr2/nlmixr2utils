# Shared run-cache helpers for nlmixr2 extension packages.

.runCacheSchemaVersion <- 1L

.abortRunCache <- function(...) {
  cli::cli_abort(c("!" = ...))
}

.sanitizeToken <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_.]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

.stateSchema <- function(schema) {
  if (is.character(schema) && length(schema) == 1L && !is.na(schema)) {
    return(list(prefix = schema, version = .runCacheSchemaVersion))
  }
  if (is.list(schema) && !is.null(schema$prefix)) {
    version <- if (!is.null(schema$version)) {
      schema$version
    } else if (!is.null(schema$schemaVersion)) {
      schema$schemaVersion
    } else {
      .runCacheSchemaVersion
    }
    return(list(
      prefix = as.character(schema$prefix[[1L]]),
      version = as.integer(version[[1L]])
    ))
  }
  .abortRunCache(
    "{.arg schema} must be a prefix string or a list with {.field prefix}."
  )
}

.statePath <- function(dir, schema) {
  schema <- .stateSchema(schema)
  file.path(dir, paste0(schema$prefix, "_state.rds"))
}

.runPrefixFromDir <- function(dir) {
  base <- basename(normalizePath(dir, mustWork = FALSE))
  match <- regexec("^.*_([^_]+)_[0-9]+$", base)
  parsed <- regmatches(base, match)[[1L]]
  if (length(parsed) == 2L && nzchar(parsed[[2L]])) {
    return(parsed[[2L]])
  }
  "run"
}

.seedPath <- function(dir, prefix = NULL) {
  prefix <- if (is.null(prefix)) {
    .runPrefixFromDir(dir)
  } else {
    .sanitizeToken(prefix)
  }
  file.path(dir, paste0(prefix, "_seed.rds"))
}

.saveRdsAtomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(basename(path), "_"), tmpdir = dirname(path))
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  saveRDS(object, tmp)
  if (file.exists(path)) {
    unlink(path)
  }
  ok <- file.rename(tmp, path)
  if (!ok) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp, force = TRUE)
  }
  invisible(path)
}

# Hand-rolled rather than pulled from a hashing package (e.g. digest) to avoid
# a new hard dependency for what only needs to be a stable, reproducible
# integer, not a cryptographic hash. `version = 2` pins the serialization
# format (stable since R 3.5) so the same `x` hashes to the same seed across
# R versions and platforms.
.hashSeed <- function(x) {
  bytes <- as.integer(serialize(x, NULL, version = 2))
  idx <- seq_along(bytes)
  mod <- 2147483646
  acc <- sum((bytes + 1) * ((idx %% 65521) + 1))
  as.integer((acc %% mod) + 1L)
}

#' Shared run-cache helpers
#'
#' These helpers implement the common numbered-run-directory, state-file,
#' task-cache, and deterministic seeding patterns used by the split
#' `nlmixr2` extension packages.
#'
#' @name run-cache
#' @section Lifecycle:
#' Stable.
NULL

#' @rdname run-cache
#' @param prefix Short run prefix such as `"boot"`, `"sir"`, or `"sse"`.
#' @param fitName Base fit name used when constructing numbered output
#'   directories.
#' @param restart Logical flag controlling resume versus fresh-run behavior.
#'   With `outputDir = NULL`, `restart = FALSE` resumes the latest numbered
#'   directory when one exists; otherwise a new numbered directory is selected.
#'   With an explicit `outputDir`, `restart = TRUE` marks an existing directory
#'   for overwrite.
#' @param outputDir Optional explicit output directory.
#' @return `resolveRunDir()` returns a list with `path`, `mode`, `created`, and
#'   `prefix`.
#' @export
resolveRunDir <- function(prefix, fitName, restart, outputDir = NULL) {
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    .abortRunCache("{.arg prefix} must be a single string.")
  }
  if (!is.character(fitName) || length(fitName) != 1L || is.na(fitName)) {
    .abortRunCache("{.arg fitName} must be a single string.")
  }
  if (!is.logical(restart) || length(restart) != 1L || is.na(restart)) {
    .abortRunCache("{.arg restart} must be TRUE or FALSE.")
  }

  prefix <- .sanitizeToken(prefix)
  fit_label <- .sanitizeToken(fitName)
  if (!nzchar(fit_label)) {
    fit_label <- "fit"
  }

  if (!is.null(outputDir)) {
    path <- normalizePath(outputDir, mustWork = FALSE)
    exists <- dir.exists(path)
    mode <- if (exists) {
      if (isTRUE(restart)) "overwrite" else "resume"
    } else {
      "new"
    }
    return(list(
      path = path,
      mode = mode,
      created = identical(mode, "new"),
      prefix = prefix
    ))
  }

  pattern <- paste0("^", fit_label, "_", prefix, "_([0-9]+)$")
  existing <- list.dirs(".", full.names = FALSE, recursive = FALSE)
  existing <- existing[grepl(pattern, existing)]
  if (!isTRUE(restart) && length(existing) > 0L) {
    nums <- as.integer(sub(pattern, "\\1", existing))
    chosen <- existing[[which.max(nums)]]
    return(list(
      path = normalizePath(chosen, mustWork = FALSE),
      mode = "resume",
      created = FALSE,
      prefix = prefix
    ))
  }

  next_num <- if (length(existing) == 0L) {
    1L
  } else {
    max(as.integer(sub(pattern, "\\1", existing))) + 1L
  }
  chosen <- paste0(fit_label, "_", prefix, "_", next_num)
  list(
    path = normalizePath(chosen, mustWork = FALSE),
    mode = "new",
    created = TRUE,
    prefix = prefix
  )
}

#' @rdname run-cache
#' @param dir Run directory containing the state file.
#' @param schema Either a prefix string, for example `"sse"`, or a list with
#'   `prefix` and optional `version`.
#' @param state Arbitrary R object to persist.
#' @return `writeRunState()` invisibly returns the state-file path;
#'   `readRunState()` returns the saved state or `NULL` when the file does not
#'   exist.
#' @export
writeRunState <- function(dir, state, schema) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    .abortRunCache("{.arg dir} must be a single directory path.")
  }
  schema_info <- .stateSchema(schema)
  path <- .statePath(dir, schema_info)
  .saveRdsAtomic(
    list(
      schema_version = schema_info$version,
      state = state
    ),
    path
  )
}

#' @rdname run-cache
#' @export
readRunState <- function(dir, schema) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    .abortRunCache("{.arg dir} must be a single directory path.")
  }
  schema_info <- .stateSchema(schema)
  path <- .statePath(dir, schema_info)
  if (!file.exists(path)) {
    return(NULL)
  }

  payload <- readRDS(path)
  if (
    !is.list(payload) ||
      is.null(payload$schema_version) ||
      !"state" %in% names(payload)
  ) {
    .abortRunCache(
      "State file {.path {path}} does not follow the versioned schema format."
    )
  }
  payload_version <- as.integer(payload$schema_version[[1L]])
  current_version <- schema_info$version
  if (payload_version > current_version) {
    .abortRunCache(
      paste0(
        "State file schema version ",
        payload_version,
        " is newer than this helper understands (",
        current_version,
        ")."
      )
    )
  }
  payload$state
}

.cacheKeyPath <- function(cache_dir, key) {
  if (!is.character(key) || length(key) != 1L || is.na(key)) {
    .abortRunCache("Cache keys must be single strings.")
  }
  file.path(cache_dir, paste0(utils::URLencode(key, reserved = TRUE), ".rds"))
}

#' @rdname run-cache
#' @param key Optional cache namespace used to isolate one family of artifacts
#'   inside a run directory.
#' @return `taskCache()` returns a list with cache methods `has()`, `get()`,
#'   `put()`, and `keys()`.
#' @export
taskCache <- function(dir, key = NULL) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    .abortRunCache("{.arg dir} must be a single directory path.")
  }

  cache_dir <- if (is.null(key)) {
    file.path(normalizePath(dir, mustWork = FALSE), "task_cache")
  } else {
    file.path(
      normalizePath(dir, mustWork = FALSE),
      paste0(.sanitizeToken(key), "_cache")
    )
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  cache <- list(
    has = function(key) {
      file.exists(.cacheKeyPath(cache_dir, key))
    },
    get = function(key) {
      path <- .cacheKeyPath(cache_dir, key)
      if (!file.exists(path)) {
        .abortRunCache(
          "Cache key {.val {key}} was not found in {.path {cache_dir}}."
        )
      }
      readRDS(path)
    },
    put = function(key, value) {
      .saveRdsAtomic(value, .cacheKeyPath(cache_dir, key))
    },
    keys = function() {
      files <- list.files(cache_dir, pattern = "\\.rds$", full.names = FALSE)
      files <- sub("\\.rds$", "", files)
      utils::URLdecode(files)
    },
    dir = cache_dir
  )
  class(cache) <- c("nlmixr2TaskCache", class(cache))
  cache
}

#' @rdname run-cache
#' @param allKeys Full set of task keys.
#' @param completedKeys Completed task keys.
#' @return `pendingTasks()` returns the ordered subset of `allKeys` that does
#'   not appear in `completedKeys`.
#' @export
pendingTasks <- function(allKeys, completedKeys) {
  allKeys <- as.character(allKeys)
  completedKeys <- as.character(completedKeys)
  allKeys[!allKeys %in% completedKeys]
}

.masterSeed <- function(dir, seed = NULL, prefix = NULL) {
  path <- .seedPath(dir, prefix = prefix)
  if (file.exists(path)) {
    payload <- readRDS(path)
    if (is.list(payload) && !is.null(payload$master_seed)) {
      return(as.integer(payload$master_seed[[1L]]))
    }
    if (is.numeric(payload) && length(payload) == 1L) {
      return(as.integer(payload[[1L]]))
    }
    .abortRunCache(
      "Seed file {.path {path}} does not contain a valid master seed."
    )
  }

  master <- if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      .abortRunCache("{.arg seed} must be NULL or a single numeric value.")
    }
    as.integer(seed[[1L]])
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    .hashSeed(list(
      seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    ))
  } else {
    .hashSeed(list(time = unclass(Sys.time()), pid = Sys.getpid()))
  }

  .saveRdsAtomic(
    list(
      schema_version = .runCacheSchemaVersion,
      master_seed = master
    ),
    path
  )
  master
}

#' @rdname run-cache
#' @param seed Optional explicit master seed. When `NULL`, the helper derives a
#'   stable integer seed from the current `.Random.seed` if one exists, and
#'   persists it for later resumes.
#' @param prefix Optional seed-file prefix override. Supply this when you need a
#'   stable artifact name such as `"boot"` or `"sir"` regardless of the
#'   directory basename.
#' @param expr Optional expression to evaluate under a derived per-key seed.
#' @return With no `key`, `withRunSeed()` returns the persisted master seed.
#'   With `key` and no `expr`, it returns the derived per-key seed. With both
#'   `key` and `expr`, it evaluates `expr` under that derived seed and restores
#'   the prior RNG state on exit.
#' @export
withRunSeed <- function(dir, seed = NULL, key = NULL, prefix = NULL, expr) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    .abortRunCache("{.arg dir} must be a single directory path.")
  }

  master <- .masterSeed(dir, seed = seed, prefix = prefix)
  if (is.null(key)) {
    if (!missing(expr)) {
      .abortRunCache(
        "Supply {.arg key} when using {.arg expr} with {.fn withRunSeed}."
      )
    }
    return(master)
  }

  derived <- .hashSeed(list(master = master, key = as.character(key)))
  if (missing(expr)) {
    return(derived)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  old_kind <- RNGkind()
  on.exit(
    {
      do.call(RNGkind, as.list(old_kind))
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  set.seed(derived)
  eval.parent(substitute(expr))
}

#' @rdname run-cache
#' @param maxLength Maximum length of the returned fit name.
#' @return `deriveFitName()` returns a sanitized single-string fit label.
#' @export
deriveFitName <- function(expr, maxLength = 50L) {
  if (
    !is.numeric(maxLength) ||
      length(maxLength) != 1L ||
      is.na(maxLength) ||
      maxLength < 1
  ) {
    .abortRunCache("{.arg maxLength} must be a positive number.")
  }

  label <- tryCatch(
    deparse(expr, width.cutoff = 500L)[1L],
    error = function(e) ""
  )
  label <- .sanitizeToken(label)
  if (!nzchar(label)) {
    return("fit")
  }
  substr(label, 1L, as.integer(maxLength))
}
