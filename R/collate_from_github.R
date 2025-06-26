# R/collate_from_github.R

# Package environment to store session-level cache paths
.collator_env <- new.env(parent = emptyenv())

#' Collate Source Code from a GitHub Repository
#'
#' Clones a public GitHub repository to a temporary directory, collates its
#' source code into a single text block, and copies it to the clipboard.
#'
#' @param repo The GitHub repository in "user/repo" format (e.g., "r-lib/usethis").
#' @param branch The name of the branch to clone. If `NULL` (the default), the
#'   repository's default branch is used.
#' @param ... Arguments to pass on to \code{\link{collate_from_path}}, such as
#'   `output_file`, `include_patterns`, or `exclude_patterns`.
#'
#' @return The collated text as a single character string (invisibly).
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_alert_warning
#' @seealso \code{\link{collate_from_path}} for collating from a local directory.
#' @examples
#' \dontrun{
#' # Collate the 'dplyr' package from its default branch
#' collate_from_github("tidyverse/dplyr")
#'
#' # Collate a specific development branch from a repository
#' # (This assumes a branch named 'develop' exists)
#' collate_from_github("user/repo", branch = "develop")
#'
#' # Collate a smaller package and save it to a file
#' collate_from_github(
#'   "r-lib/crayon",
#'   output_file = "crayon_source.txt",
#'   copy_to_clipboard = FALSE
#' )
#' }
collate_from_github <- function(repo, branch = NULL, ...) {
  git_path <- Sys.which("git")
  if (git_path == "") {
    cli_abort("Command-line 'git' not found...")
  }
  if (!grepl("/", repo, fixed = TRUE)) {
    cli_abort("The `repo` format must be 'user/repo'...")
  }

  # --- SESSION-LEVEL CACHING IN TEMPDIR ---
  # Create a unique cache key for this repo + branch combination
  cache_key <- paste0(gsub("/", "_", repo), if (!is.null(branch)) paste0("_", branch) else "")

  # Check if we already have a cached path for this repo in this session
  if (exists(cache_key, envir = .collator_env)) {
    repo_path <- get(cache_key, envir = .collator_env)
  } else {
    # Create a new temp directory for this repo
    repo_path <- file.path(tempdir(), "collator_cache", cache_key)
    dir.create(dirname(repo_path), showWarnings = FALSE, recursive = TRUE)
    # Store the path in our session cache
    assign(cache_key, repo_path, envir = .collator_env)
  }

  repo_url <- paste0("https://github.com/", repo)

  if (dir.exists(repo_path)) {
    # --- REPO EXISTS: PULL LATEST CHANGES ---
    cli_alert_info("Cached repo found. Pulling latest changes for '{repo}'...")

    # Construct pull command
    pull_args <- c("pull", "origin", branch %||% "HEAD")

    result <- tryCatch(
      withr::with_dir(repo_path, system2(git_path, pull_args, stdout = TRUE, stderr = TRUE)),
      warning = function(w) w
    )

    if (inherits(result, "warning") || any(grepl("fatal:|error:", result, ignore.case = TRUE))) {
      cli_alert_warning("Could not pull updates for cached repo. Using existing version. Error: {paste(result, collapse = '\\n')}")
    } else {
      cli_alert_success("Successfully pulled updates.")
    }

  } else {
    # --- REPO DOES NOT EXIST: CLONE IT ---
    cli_alert_info("Cloning '{repo}' into temporary cache...")

    clone_args <- c("clone", "--depth", "1")
    if (!is.null(branch)) {
      clone_args <- c(clone_args, "--branch", branch)
    }
    clone_args <- c(clone_args, repo_url, repo_path)

    result <- tryCatch(
      system2(git_path, clone_args, stdout = TRUE, stderr = TRUE),
      warning = function(w) w
    )

    if (inherits(result, "warning") || any(grepl("fatal:", result, ignore.case = TRUE))) {
      output_text <- paste(result, collapse = "\n")
      if (grepl("repository not found", output_text, ignore.case = TRUE)) {
        cli_abort("Failed to clone: Repository not found (it may be private or misspelled).")
      } else if (grepl("could not find remote branch", output_text, ignore.case = TRUE)) {
        cli_abort("Failed to clone: The branch '{branch}' does not exist.")
      } else {
        cli_abort(c("Git clone failed. Raw output:", paste(" ", result)))
      }
    }
    cli_alert_success("Successfully cloned repository to temporary cache.")
  }

  # The rest of the function proceeds as before
  collate_from_path(path = repo_path, ...)
}

#' Clear the session-level repository cache
#'
#' Removes all cached repository paths from the current session. The actual
#' temporary directories will be cleaned up when the R session ends.
#'
#' @return NULL (invisibly)
#' @export
clear_collator_cache <- function() {
  rm(list = ls(envir = .collator_env), envir = .collator_env)
  cli_alert_success("Collator cache cleared.")
  invisible(NULL)
}

# Helper for a nicer cli message
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
