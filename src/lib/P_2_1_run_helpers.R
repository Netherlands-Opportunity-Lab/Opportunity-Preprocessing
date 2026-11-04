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

# The commit the scripts were run from, read straight from the .git folder, so it
# also works where the git command is not on the PATH (the CBS RA, where git only
# comes with TortoiseGit). Looks for .git in `dir` and its parent folders; follows
# HEAD to the branch ref (loose file or packed-refs). Returns e.g.
# "3f2a...c9 (branch S_2_1)", or "unknown (no .git folder found)".
# Uncommitted changes cannot be seen this way; when the git command does work,
# " + uncommitted changes" is added if the working tree differs from the commit.
git_commit <- function(dir = getwd()) {
  find_git_dir <- function(d) {
    d <- normalizePath(d, winslash = "/", mustWork = FALSE)
    repeat {
      g <- file.path(d, ".git")
      if (dir.exists(g)) return(g)
      if (file.exists(g)) {                       # worktree/submodule: ".git" is a file "gitdir: <path>"
        target <- sub("^gitdir:\\s*", "", readLines(g, n = 1, warn = FALSE))
        if (!grepl("^(/|[A-Za-z]:)", target)) target <- file.path(d, target)
        return(normalizePath(target, winslash = "/", mustWork = FALSE))
      }
      parent <- dirname(d)
      if (identical(parent, d)) return(NA_character_)
      d <- parent
    }
  }
  read_ref <- function(git_dir, ref) {
    loose <- file.path(git_dir, ref)
    if (file.exists(loose)) return(trimws(readLines(loose, n = 1, warn = FALSE)))
    packed <- file.path(git_dir, "packed-refs")
    if (file.exists(packed)) {
      lines <- readLines(packed, warn = FALSE)
      hit <- lines[!grepl("^[#^]", lines) & endsWith(lines, paste0(" ", ref))]
      if (length(hit) >= 1) return(sub(" .*$", "", hit[1]))
    }
    NA_character_
  }

  git_dir <- tryCatch(find_git_dir(dir), error = function(e) NA_character_)
  if (is.na(git_dir) || !file.exists(file.path(git_dir, "HEAD"))) return("unknown (no .git folder found)")

  head <- trimws(readLines(file.path(git_dir, "HEAD"), n = 1, warn = FALSE))
  if (grepl("^ref:", head)) {
    ref    <- trimws(sub("^ref:", "", head))
    hash   <- read_ref(git_dir, ref)
    branch <- paste0("branch ", sub("^refs/heads/", "", ref))
  } else {
    hash   <- head
    branch <- "detached HEAD"
  }
  if (is.na(hash) || !grepl("^[0-9a-f]{40}$", hash)) return(paste0("unknown (", branch, " has no commits yet)"))

  dirty <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(dir), "status", "--porcelain"),
                                             stdout = TRUE, stderr = FALSE)),
                    error = function(e) NULL)
  status <- attr(dirty, "status")
  note <- if (is.null(dirty) || (!is.null(status) && status != 0)) ""
          else if (length(dirty) > 0) " + uncommitted changes" else ""
  paste0(hash, " (", branch, ")", note)
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
