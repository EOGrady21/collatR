
#' Collate Source Code from a GitHub Repository
#'
#' Clones a public GitHub repository to a temporary directory, collates its
#' source code into a single text block, and copies it to the clipboard.
#'
#' @param repo The GitHub repository in "user/repo" format (e.g., "r-lib/usethis").
#' @param branch The name of the branch to clone. Defaults to the repository's
#'   default branch.
#' @param ... Arguments to pass on to \code{\link{collate_from_path}}, such as
#'   `output_file`, `include_patterns`, or `exclude_patterns`.
#'
#' @return The collated text as a single character string (invisibly).
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#' @importFrom gert git_clone
#'
#' @seealso \code{\link{collate_from_path}} for collating from a local directory.
#' @author Emily O'Grady
#' @examples
#' \dontrun{
#' # Collate the 'dplyr' package from GitHub and copy to clipboard
#' collate_from_github("tidyverse/dplyr")
#'
#' # Collate a smaller package and save it to a file
#' collate_from_github(
#'   "r-lib/crayon",
#'   output_file = "crayon_source.txt",
#'   copy_to_clipboard = FALSE
#' )
#' }
collate_from_github <- function(repo, branch = NULL, ...) {
  if (!requireNamespace("gert", quietly = TRUE)) {
    cli_abort("The 'gert' package is required to clone from GitHub. Please install it with `install.packages('gert')`.")
  }
  if (!grepl("/", repo, fixed = TRUE)) {
    cli_abort("The `repo` format must be 'user/repo' (e.g., 'tidyverse/dplyr').")
  }

  temp_path <- file.path(tempdir(), basename(repo))
  on.exit(unlink(temp_path, recursive = TRUE, force = TRUE), add = TRUE)

  repo_url <- paste0("https://github.com/", repo)

  cli_alert_info("Cloning '{repo}' from GitHub...")
  tryCatch({
    gert::git_clone(repo_url, path = temp_path, branch = branch)
    cli_alert_success("Successfully cloned repository.")
  }, error = function(e) {
    cli_abort("Failed to clone repository '{repo}'. Please check the name and your internet connection.")
  })

  # Call the main function on the temporary path
  collate_from_path(path = temp_path, ...)
}
