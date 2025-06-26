# R/collate_from_github.R

#' Collate Source Code from a GitHub Repository
#'
#' Clones a public GitHub repository to a persistent cache directory, collates its
#' source code into a single text block, and optionally copies it to the clipboard.
#'
#' @param repo Character. The GitHub repository in "user/repo" format (e.g., "r-lib/usethis").
#' @param branch Character or NULL. The branch to clone. If `NULL`, uses the default branch.
#' @param ... Arguments passed to \code{\link{collate_from_path}}, such as
#'   `output_file`, `include_patterns`, or `exclude_patterns`.
#'
#' @return The collated text as a single character string (invisibly).
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
#' @importFrom rappdirs user_cache_dir
#' @seealso \code{\link{collate_from_path}} for collating from a local directory.
#' @examples
#' \dontrun{
#' collate_from_github("tidyverse/dplyr")
#' collate_from_github("user/repo", branch = "develop")
#' collate_from_github("r-lib/crayon", output_file = "crayon_source.txt", copy_to_clipboard = FALSE)
#' }
collate_from_github <- function(repo, branch = NULL, ...) {
  # Input validation
  if (!is.character(repo) || length(repo) != 1L || is.na(repo) || repo == "") {
    cli_abort("Argument 'repo' must be a non-empty character string.")
  }
  if (!grepl("^[^/]+/[^/]+$", repo)) {
    cli_abort("Argument 'repo' must be in 'user/repo' format.")
  }
  if (!is.null(branch) && (!is.character(branch) || length(branch) != 1L || is.na(branch) || branch == "")) {
    cli_abort("Argument 'branch' must be NULL or a non-empty character string.")
  }
  git_path <- Sys.which("git")
  if (git_path == "") {
    cli_abort("Command-line 'git' not found. Please install Git and ensure it is on your PATH.")
  }

  # Set up cache directory for the repo
  cache_root <- rappdirs::user_cache_dir(appname = "collator")
  dir.create(cache_root, showWarnings = FALSE, recursive = TRUE)
  repo_path <- file.path(cache_root, repo)
  repo_url <- paste0("https://github.com/", repo)

  # Clone or update the repository in cache
  if (dir.exists(repo_path)) {
    cli_alert_info("Cached repo found. Pulling latest changes for '{repo}'...")
    pull_args <- c("pull", "origin", if (is.null(branch)) "HEAD" else branch)
    result <- tryCatch(
      withr::with_dir(repo_path, system2(git_path, pull_args, stdout = TRUE, stderr = TRUE)),
      warning = function(w) w
    )
    if (inherits(result, "warning") || any(grepl("fatal:|error:", result, ignore.case = TRUE))) {
      cli_warn("Could not pull updates for cached repo. Using existing version. Error: {paste(result, collapse = '\\n')}")
    } else {
      cli_alert_success("Successfully pulled updates.")
    }
  } else {
    cli_alert_info("Cloning '{repo}' into cache...")
    clone_args <- c("clone", "--depth", "1")
    if (!is.null(branch)) clone_args <- c(clone_args, "--branch", branch)
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
    cli_alert_success("Successfully cloned and cached repository.")
  }

  # Collate the code from the cached repository
  collate_from_path(path = repo_path, ...)
}

# Helper: Null coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
