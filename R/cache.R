#' @describeIn collator_cache Clear all repositories from the cache directory.
#'
#' @param force Logical. If `FALSE` (the default), an interactive prompt will ask
#'   for confirmation before deleting files.
#' @return Invisibly returns `NULL`. Used for side effects.
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
clear_collator_cache <- function(force = FALSE) {
  # Input validation
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("Argument 'force' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  cache_dir <- get_collator_cache_dir()

  if (!dir.exists(cache_dir) || length(list.files(cache_dir, recursive = TRUE)) == 0) {
    cli::cli_alert_success("Cache is already empty. Nothing to do.")
    return(invisible(NULL))
  }

  # Prompt user for confirmation if not forced
  if (!force) {
    items <- list.files(cache_dir, recursive = TRUE)
    n_items <- length(items)
    cli::cli_alert_info(
      "You are about to delete {n_items} file{?s} from {.path {cache_dir}}."
    )

    # Prompt until valid input
    repeat {
      response <- tolower(trimws(readline("Are you sure you want to continue? (yes/no): ")))
      if (response %in% c("yes", "y")) break
      if (response %in% c("no", "n")) {
        cli::cli_alert_info("Cache clearing cancelled.")
        return(invisible(NULL))
      }
      cat("Please enter 'yes', 'y', 'no', or 'n'.\n")
    }
  }

  # Proceed with deletion
  cli::cli_alert_info("Deleting cache directory: {.path {cache_dir}}")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cli::cli_alert_success("Cache successfully cleared.")
  invisible(NULL)
}

#' Manage the collator Cache
#'
#' Helper functions to interact with the directory used by `collator` to cache
#' cloned GitHub repositories.
#'
#' @name collator_cache
NULL

#' @describeIn collator_cache Get the path to the cache directory.
#'
#' @return A character string with the path to the cache directory.
#' @export
#' @importFrom rappdirs user_cache_dir
#' @examples
#' \dontrun{
#' # See where collator stores cloned repositories
#' get_collator_cache_dir()
#' }
get_collator_cache_dir <- function() {
  dir <- rappdirs::user_cache_dir(appname = "collator")
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || dir == "") {
    stop("Could not determine a valid cache directory path.", call. = FALSE)
  }
  dir
}
