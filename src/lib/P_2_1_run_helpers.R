# Helpers for running a pipeline (the same file in all Opportunity Lab repos: keep them identical)
#
#   find_versioned()    find a script or config by its name, with or without the
#                       version prefix (P_2_1_, A_2_0_I1_, A_2_0_M1_, ...). The
#                       prefixes only exist outside the CBS RA, so code that looks
#                       files up by name must accept both.
#   source_versioned()  source such a script into the global environment
#   start_run_log() / log_line() / finish_run_log()
#                       a plain-text log per run: git commit, R version and
#                       packages, the configs used, and the time per step
#
# (c) Netherlands Opportunity Lab 2026


find_versioned <- function(dir, name, ext = "R") {
  pattern <- paste0("^([A-Z]_?[0-9]+_[0-9]+_([A-Z][0-9]+_)?)?", name, "\\.", ext, "$")
  hits <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(hits) != 1) {
    stop("Expected exactly one file '", name, ".", ext, "' (optionally with a version prefix) in ",
         dir, ", found ", length(hits), if (length(hits) > 0) paste0(": ", paste(basename(hits), collapse = ", ")),
         call. = FALSE)
  }
  hits
}

source_versioned <- function(dir, name) {
  source(find_versioned(dir, name), local = globalenv())
}


#### RUN LOG ####

git_commit <- function() {
  out <- tryCatch(suppressWarnings(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)),
                  error = function(e) character(0))
  if (length(out) == 1 && nchar(out) == 40) out else "unknown (git not available)"
}

start_run_log <- function(log_dir, label) {
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  file <- file.path(log_dir, paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", label, ".log"))
  log <- list(file = file, start = Sys.time())
  log_line(log, "run:      ", label)
  log_line(log, "started:  ", format(log$start))
  log_line(log, "commit:   ", git_commit())
  log_line(log, "R:        ", R.version.string)
  log
}

log_line <- function(log, ...) {
  line <- paste0(...)
  message(line)
  cat(line, "\n", sep = "", file = log$file, append = TRUE)
}

# time one step: log_step(log, "02_predictors", source_versioned(dir, name))
log_step <- function(log, step, expr) {
  t0 <- Sys.time()
  log_line(log, "---- ", step, " started ", format(t0))
  force(expr)
  log_line(log, "---- ", step, " done in ", format(round(difftime(Sys.time(), t0, units = "mins"), 1)))
  invisible(NULL)
}

# append the config files and the loaded packages, then the total run time
finish_run_log <- function(log, config_files = character(0)) {
  for (f in config_files) {
    log_line(log, "\n==== config: ", f)
    cat(readLines(f, warn = FALSE), sep = "\n", file = log$file, append = TRUE)
  }
  cat("\n==== sessionInfo()\n", file = log$file, append = TRUE)
  cat(utils::capture.output(utils::sessionInfo()), sep = "\n", file = log$file, append = TRUE)
  log_line(log, "\nfinished: ", format(Sys.time()), " (",
           format(round(difftime(Sys.time(), log$start, units = "mins"), 1)), ")")
  invisible(log$file)
}
