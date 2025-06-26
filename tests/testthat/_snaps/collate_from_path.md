# collate_from_path handles empty directories gracefully

    Code
      result <- collate_from_path(".", copy_to_clipboard = FALSE)
    Message
      i No files in local path '.' matched the specified patterns.
    Code
      expect_equal(result, "")

