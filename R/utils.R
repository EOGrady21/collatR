
#' Check if a file is likely binary
#'
#' Reads the first few bytes of a file to check for NULL bytes, which are a
#' strong indicator of a binary file format.
#'
#' @param file_path Path to the file.
#' @param n_bytes Number of bytes to check.
#' @return Logical `TRUE` if a NULL byte is found, `FALSE` otherwise.
#' @noRd
is_binary_file <- function(file_path, n_bytes = 1024) {
  tryCatch({
    con <- file(file_path, "rb")
    on.exit(close(con))
    bytes <- readBin(con, "raw", n = n_bytes)
    return(any(bytes == as.raw(0)))
  }, error = function(e) {
    # If we can't read it, treat it as something to skip
    TRUE
  })
}
