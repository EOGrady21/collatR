#' Scans a local directory, finds relevant source files based on include/exclude
#' patterns, and combines them into a single text file. The output is
#' automatically copied to the clipboard for convenience.
#'
#' @param path The root path to the local repository or R package. Defaults to the
#'   current working directory.
#' @param output_file The path for the output text file. If NULL (the default),
#'   a temporary file is created.
#' @param copy_to_clipboard Logical. If TRUE (the default), the collated text is
#'   copied to the system clipboard. Requires the 'clipr' package.
#' @param preamble A character string to add to the top of the collated file,
#'   providing context for an LLM. A default preamble is generated if NULL.
#' @param include_patterns A character vector of regex patterns for file paths
#'   to include. The default is curated for typical R projects.
#' @param exclude_patterns A character vector of regex patterns for file paths
#'   to exclude. Applied after the include patterns.
#'
#' @return The collated text as a single character string (invisibly).
#' @export
#' @importFrom cli cli_progress_bar cli_progress_update cli_alert_success cli_alert_info cli_alert_warning
#' @importFrom clipr write_clip clipr_available
#' @importFrom gert git_ls
#' @seealso \code{\link{collate_from_github}} for collating from a remote repository.
#' @author Emily O'Grady
#' @examples
#' \dontrun{
#' # Create a dummy project
#' dir.create("my_temp_project")
#' writeLines("a <- 1", "my_temp_project/script.R")
#' writeLines("Test Project", "my_temp_project/README.md")
#'
#' # Collate the code from the project path and copy to clipboard
#' collate_from_path("my_temp_project")
#'
#' # Clean up
#' unlink("my_temp_project", recursive = TRUE)
#' }
#'
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
  # Let's simplify and assume for this use case it will always be a git repo.
  all_files <- gert::git_ls(repo = path)$path

  # --- ADDING THIS DEBUGGING BLOCK ---
  # message("--- Debug: Files found by gert::git_ls ---")
  # if(length(all_files) > 0) {
  #   print(head(all_files))
  # } else {
  #   print("gert::git_ls returned character(0)")
  #   message("Does the directory '", path, "' exist and contain files?")
  #   print(list.files(path, recursive = TRUE, all.files = TRUE))
  # }
  # message("--------------------------------------")
  #
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
    cli_alert_warning("No files found matching the criteria in '{path}'.")
    return(invisible(""))
  }

  collated_content <- character()
  if (is.null(preamble)) {
    preamble <- paste0("This file is an automated collation of the source code from the '", basename(normalizePath(path)), "' repository...")
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
    }, error = function(e) {
      cli_alert_warning("Could not read file: {f}. Skipping.")
    })
  }
  final_text <- paste(collated_content, collapse = "\n")
  if (!is.null(output_file)) {
    tryCatch({ writeLines(final_text, output_file) }, error = function(e) { stop(...) })
  }
  if (copy_to_clipboard) {
    if (clipr_available()) {
      write_clip(final_text, allow_non_interactive = TRUE)
      cli_alert_success("Collated code copied to clipboard!")
    } else {
      cli_alert_warning("`clipr` is not available...")
    }
  }
  invisible(final_text)
}
