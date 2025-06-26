# R/collate_from_path.R

#' Collate Source Code from a Local Path
#'
#' @description
#' Scans a local directory, finds relevant source files based on include/exclude
#' patterns, and combines them into a single text file. Optionally copies the result
#' to the clipboard.
#'
#' @param path Character. Root path to the local repository or R package. Defaults to
#'   the current working directory.
#' @param output_file Character or NULL. Path for the output text file. If `NULL`,
#'   no file is written.
#' @param copy_to_clipboard Logical. If `TRUE` (default), the collated text is
#'   copied to the system clipboard. Requires the 'clipr' package.
#' @param preamble Character or NULL. Text to add to the top of the collated file.
#'   A default preamble is generated if `NULL`.
#' @param include_patterns Character vector. Regex patterns for file paths to include.
#' @param exclude_patterns Character vector. Regex patterns for file paths to exclude.
#'
#' @return The collated text as a single character string (invisibly).
#' @export
#' @importFrom cli cli_progress_bar cli_progress_update cli_alert_success cli_alert_info cli_warn
#' @importFrom clipr write_clip clipr_available
#' @importFrom gert git_ls
#' @seealso \code{\link{collate_from_github}} for collating from a remote repository.
collate_from_path <- function(
  path = ".",
  output_file = NULL,
  copy_to_clipboard = TRUE,
  preamble = NULL,
  include_patterns = c(
    "^DESCRIPTION$", "^NAMESPACE$", "^NEWS\\.md$", "^README\\.md$",
    "^\\.Rprofile$", "^\\.gitignore$", "^\\.Rbuildignore$",
    "\\.R$", "\\.Rmd$", "\\.yml$", "\\.yaml$", "\\.sh$",
    "^vignettes[/\\]", "^tests[/\\]", "^inst[/\\]",
    "^data[/\\]", "^\\.github[/\\]"
  ),
  exclude_patterns = c(
    "\\.Rproj$", "/renv/", "/packrat/", "/pkgdown/",
    "/man/", "\\.DS_Store", "\\.Rhistory", "\\.RData"
  )
) {
  # Input validation
  if (!is.character(path) || length(path) != 1L || is.na(path) || path == "") {
    stop("Argument 'path' must be a non-empty character string.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("The specified path does not exist: ", path, call. = FALSE)
  }
  if (!is.null(output_file) && (!is.character(output_file) || length(output_file) != 1L || is.na(output_file) || output_file == "")) {
    stop("Argument 'output_file' must be NULL or a non-empty character string.", call. = FALSE)
  }
  if (!is.logical(copy_to_clipboard) || length(copy_to_clipboard) != 1L || is.na(copy_to_clipboard)) {
    stop("Argument 'copy_to_clipboard' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(preamble) && (!is.character(preamble) || length(preamble) != 1L || is.na(preamble))) {
    stop("Argument 'preamble' must be NULL or a character string.", call. = FALSE)
  }
  if (!is.character(include_patterns) || any(is.na(include_patterns))) {
    stop("Argument 'include_patterns' must be a character vector.", call. = FALSE)
  }
  if (!is.character(exclude_patterns) || any(is.na(exclude_patterns))) {
    stop("Argument 'exclude_patterns' must be a character vector.", call. = FALSE)
  }

  # --- 1. Find all relevant files ---
  if (dir.exists(file.path(path, ".git"))) {
    all_files <- gert::git_ls(repo = path)$path
  } else {
    all_files <- list.files(path, recursive = TRUE, all.files = FALSE, no.. = TRUE)
    dotfiles <- c(".Rprofile", ".gitignore", ".Rbuildignore")
    dotfiles_exist <- dotfiles[file.exists(file.path(path, dotfiles))]
    all_files <- unique(c(all_files, dotfiles_exist))
  }

  included_files <- unique(unlist(lapply(include_patterns, function(pattern) {
    grep(pattern, all_files, value = TRUE, ignore.case = TRUE)
  })))

  if (length(exclude_patterns) > 0) {
    excluded_mask <- grepl(paste(exclude_patterns, collapse = "|"), included_files, ignore.case = TRUE)
    final_files <- included_files[!excluded_mask]
  } else {
    final_files <- included_files
  }
  final_files <- sort(final_files)

  if (length(final_files) == 0) {
    cli_alert_info("No files in local path '{path}' matched the specified patterns.")
    return(invisible(""))
  }

  # --- 2. Collate file contents ---
  if (is.null(preamble)) {
    preamble <- sprintf(
      "This file is an automated collation of the source code from the '%s' repository.",
      basename(normalizePath(path))
    )
  }
  collated_content <- c(
    paste0(preamble, "\n", strrep("=", 60), "\n")
  )

  cli_alert_info("Collating {length(final_files)} file{?s}...")
  pb <- cli_progress_bar(format = " {cli::pb_bar} {cli::pb_percent}", total = length(final_files))

  for (f in final_files) {
    cli_progress_update(id = pb)
    file_path <- file.path(path, f)
    if (is_binary_file(file_path)) next
    header <- paste0("\n--- START OF FILE: ", f, " ---\n")
    footer <- paste0("\n--- END OF FILE: ", f, " ---\n")
    file_content <- tryCatch(
      readLines(file_path, warn = FALSE),
      error = function(e) {
        cli_warn("Could not read file: {f}. Skipping.")
        return(NULL)
      }
    )
    if (!is.null(file_content)) {
      collated_content <- c(collated_content, header, file_content, footer)
    }
  }

  final_text <- paste(collated_content, collapse = "\n")

  # --- 3. Output ---
  if (!is.null(output_file)) {
    writeLines(final_text, output_file)
  }
  if (copy_to_clipboard) {
    if (clipr_available()) {
      write_clip(final_text, allow_non_interactive = TRUE)
      cli_alert_success("Collated code copied to clipboard!")
    } else {
      cli_warn("`clipr` is not available; skipping clipboard copy.")
    }
  }
  invisible(final_text)
}
