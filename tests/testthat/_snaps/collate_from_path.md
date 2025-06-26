# collate_from_path handles empty directories gracefully

    Code
      result <- collate_from_path(".", copy_to_clipboard = FALSE)
    Message
      ! No files found matching the criteria in '.'.
    Code
      expect_equal(result, "")

