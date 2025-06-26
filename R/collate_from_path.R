# R/collate_from_path.R

#' Collate Source Code from a Local Path
#'
#' @description
#' Scans a local directory, finds relevant source files based on include/exclude
#' patterns, and combines them into a single text file. The output is
#' automatically copied to the clipboard for convenience.
#'
#' @param path The root path to the local repository or R package. Defaults to the
#'   current working directory.
#' @param output_file The path for the output text file. If `NULL` (the default),
#'   no file is written.
#' @param copy_to_clipboard Logical. If `TRUE` (the default), the collated text is
#'   copied to the system clipboard. Requires the 'clipr' package.
#' @param preamble A character string to add to the top of the collated file,
#'   providing context for an LLM. A default preamble is generated if `NULL`.
#' @param include_patterns A character vector of regex patterns for file paths
#'   to include.
#' @param exclude_patterns A character vector of regex patterns for file paths
#'   to exclude.
#'
#' @return The collated text as a single character string (invisibly).
#' @export
#' @importFrom cli cli_progress_bar cli_progress_update cli_alert_success cli_alert_info
#' @importFrom clipr write_clip clipr_available
#' @importFrom gert git_ls
#' @seealso \code{\link{collate_from_github}} for collating from a remote repository.
collate_from_path <- function(path = ".",
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
                                "/man/", "\\.DS_Store", "\\.Rhistory",
                                "\\.RData"
                              )) {

  if (!dir.exists(path)) {
    stop("The specified path does not exist: ", path, call. = FALSE)
  }

  # --- 1. Find all relevant files ---
  if (dir.exists(file.path(path, ".git"))) {
    all_files <- gert::git_ls(repo = path)$path
  } else {
    all_files <- list.files(path, recursive = TRUE, all.files = FALSE, no.. = TRUE)
    dotfiles <- c(".Rprofile", ".gitignore", ".Rbuildignore")
    dotfiles_exist <- dotfiles[file.exists(file.path(path, dotfiles))]
    all_files <- c(all_files, dotfiles_exist)
  }

  included_files <- unique(unlist(sapply(include_patterns, function(pattern) {
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

  collated_content <- character()
  if (is.null(preamble)) {
    preamble <- paste0("This file is an automated collation of the source code from the '",
                       basename(normalizePath(path)), "' repository...")
  }
  collated_content[1] <- paste0(preamble, "\n", strrep("=", 60), "\n")
  cli_alert_info("Collating {length(final_files)} file{?s}...")
  pb <- cli_progress_bar(format = " {cli::pb_bar} {cli::pb_percent}", total = length(final_files))
  for (f in final_files) {
    cli_progress_update(id = pb)
    file_path <- file.path(path, f)
    if (is_binary_file(file_path)) { next }
    tryCatch({
      content <- readLines(file_path, warn = FALSE)
      header <- paste0("\n--- START OF FILE: ", f, " ---\n")
      footer <- paste0("\n--- END OF FILE: ", f, " ---\n")
      collated_content <- c(collated_content, header, content, footer)
    }, error = function(e) { cli_warn("Could not read file: {f}. Skipping.") })
  }
  final_text <- paste(collated_content, collapse = "\n")
  if (!is.null(output_file)) {
     writeLines(final_text, output_file)
  }
  if (copy_to_clipboard) {
    if (clipr_available()) {
      write_clip(final_text, allow_non_interactive = TRUE)
      cli_alert_success("Collated code copied to clipboard!")
    } else { cli_warn("`clipr` is not available...") }
  }
  invisible(final_text)
}
