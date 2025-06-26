#' Check if a file is likely binary
#'
#' Reads the first few bytes of a file to check for NULL bytes, which are a
#' strong indicator of a binary file format.
#'
#' @param file_path Character. Path to the file.
#' @param n_bytes Integer. Number of bytes to check. Default is 1024.
#' @return Logical. `TRUE` if a NULL byte is found or file cannot be read, `FALSE` otherwise.
#' @noRd
is_binary_file <- function(file_path, n_bytes = 1024L) {
  # Input validation
  if (!is.character(file_path) || length(file_path) != 1L || is.na(file_path) || file_path == "") {
    stop("Argument 'file_path' must be a non-empty character string.", call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop(sprintf("File does not exist: '%s'", file_path), call. = FALSE)
  }
  if (!is.numeric(n_bytes) || length(n_bytes) != 1L || is.na(n_bytes) || n_bytes <= 0) {
    stop("Argument 'n_bytes' must be a single positive integer.", call. = FALSE)
  }
  n_bytes <- as.integer(n_bytes)

  # Try to read the file and check for NULL bytes
  tryCatch({
    con <- file(file_path, "rb")
    on.exit(close(con), add = TRUE)
    bytes <- readBin(con, "raw", n = n_bytes)
    any(bytes == as.raw(0))
  }, error = function(e) {
    # If file can't be read, treat as binary to skip
    TRUE
  })
}
