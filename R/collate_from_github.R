# R/collate_from_github.R

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
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#' @importFrom gert git_clone
#' @importFrom rappdirs user_cache_dir
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

  # --- CACHING LOGIC ---
  # Define a persistent cache directory for the package
  cache_root <- rappdirs::user_cache_dir(appname = "collator")
  dir.create(cache_root, showWarnings = FALSE, recursive = TRUE)

  # Path for the specific repo in our cache
  repo_path <- file.path(cache_root, repo)
  repo_url <- paste0("https://github.com/", repo)

  if (dir.exists(repo_path)) {
    # --- REPO EXISTS: PULL LATEST CHANGES ---
    cli_alert_info("Cached repo found. Pulling latest changes for '{repo}'...")

    # Construct pull command. We must set the working directory.
    # We use withr::with_dir to safely change directory only for this command.
    pull_args <- c("pull", "origin", branch %||% "HEAD")

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
    # --- REPO DOES NOT EXIST: CLONE IT ---
    cli_alert_info("Cloning '{repo}' into cache...")

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
    cli_alert_success("Successfully cloned and cached repository.")
  }

  # The rest of the function proceeds, using the (now guaranteed to exist) repo_path
  collate_from_path(path = repo_path, ...)
}

# Helper for a nicer cli message
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
