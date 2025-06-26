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
  # --- NEW: Check for command-line git ---
  git_path <- Sys.which("git")
  if (git_path == "") {
    cli_abort(c(
      "x" = "Command-line 'git' not found.",
      "!" = "This function requires 'git' to be installed and on the system's PATH.",
      "i" = "Please install git from https://git-scm.com/downloads"
    ))
  }

  if (!grepl("/", repo, fixed = TRUE)) {
    cli_abort("The `repo` format must be 'user/repo'...")
  }

  repo_path <- file.path(tempdir(), basename(repo))
  # Clean up directory on exit
  on.exit(unlink(repo_path, recursive = TRUE, force = TRUE), add = TRUE)

  repo_url <- paste0("https://github.com/", repo)

  # --- Build the git command arguments ---
  args <- c("clone", "--depth", "1") # --depth 1 for a faster shallow clone
  if (!is.null(branch)) {
    args <- c(args, "--branch", branch)
  }
  args <- c(args, repo_url, repo_path)

  cli_alert_info("Running git clone...")

  # --- Execute the system command ---
  result <- tryCatch({
    system2(git_path, args = args, stdout = TRUE, stderr = TRUE)
  }, warning = function(w) {
    # system2 can throw warnings on non-zero exit, which we treat as errors
    w
  })

  # --- Check for cloning errors ---
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

  cli_alert_success("Successfully cloned repository.")

  # Now, call the function that uses gert::git_ls, which will work correctly.
  collate_from_path(path = repo_path, ...)
}

# Helper for a nicer cli message
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
