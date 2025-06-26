

#' @describeIn collator_cache Clear all repositories from the cache directory.
#' @param force Logical. If `FALSE` (the default), an interactive prompt will ask
#'   for confirmation before deleting files.
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_warn
clear_collator_cache <- function(force = FALSE) {
  cache_dir <- get_collator_cache_dir()

  if (!dir.exists(cache_dir) || length(list.files(cache_dir)) == 0) {
    cli_alert_success("Cache is already empty. Nothing to do.")
    return(invisible())
  }

  if (!force) {
    items <- list.files(cache_dir, recursive = TRUE)
    n_items <- length(items)

    cli_alert_info("You are about to delete {n_items} file{?s} from {.path {cache_dir}}.")

    # Use the real cli_prompt function
    # It will loop until the user enters 'yes' or 'no'.
    yes_no_prompt <- function(prompt_message) {
  repeat {
    response <- tolower(readline(prompt_message))
    if (response %in% c("yes", "y")) {
      return(TRUE)
    } else if (response %in% c("no", "n")) {
      return(FALSE)
    } else {
      cat("Please enter 'yes', 'y', 'no', or 'n'.\n")
    }
  }
}

    response <- ""
    while (!response %in% c("yes", 'y', 'n', "no")) {
      response <- tolower(yes_no_prompt("Are you sure you want to continue? (yes/no): "))
    }

    if (response == "no") {
      cli_alert_info("Cache clearing cancelled.")
      return(invisible())
    }
    # -----------------------------------------------
  }

  # If we get here, either force was TRUE or the user confirmed.
  cli_alert_info("Deleting cache directory: {.path {cache_dir}}")

  unlink(cache_dir, recursive = TRUE, force = TRUE)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  cli_alert_success("Cache successfully cleared.")
  invisible()
}


#' Manage the collator Cache
#'
#' Helper functions to interact with the directory used by `collator` to cache
#' cloned GitHub repositories.
#'
#' @name collator_cache
NULL

#' @describeIn collator_cache Get the path to the cache directory.
#' @export
#' @importFrom rappdirs user_cache_dir
#' @examples
#' \dontrun{
#' # See where collator stores cloned repositories
#' get_collator_cache_dir()
#' }
get_collator_cache_dir <- function() {
  rappdirs::user_cache_dir(appname = "collator")
}
