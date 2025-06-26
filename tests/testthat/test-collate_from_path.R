# tests/testthat/test-collate_from_path.R

# Helper function to create a dummy project structure
create_dummy_project <- function(path = ".") {
  # Create all directories first
  dir.create(file.path(path, "R"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(path, "man"), showWarnings = FALSE, recursive = TRUE)

  # Now write the files
  writeLines("Package: mytestpkg", file.path(path, "DESCRIPTION"))
  writeLines("a <- 1\nb <- 2", file.path(path, "R/script.R"))
  writeLines("# My Test Project", file.path(path, "README.md"))
  writeLines("*.Rproj", file.path(path, ".gitignore"))
  writeLines("This is a man page.", file.path(path, "man/man.Rd"))
  writeBin(as.raw(c(0x00, 0x01, 0x00, 0x02)), file.path(path, "data.bin"))
}

test_that("collate_from_path works on a basic project", {
  withr::with_tempdir({
    create_dummy_project()
    result <- collate_from_path(".", copy_to_clipboard = FALSE)

    expect_type(result, "character")
    expect_length(result, 1)
    expect_true(grepl("--- START OF FILE: DESCRIPTION ---", result, fixed = TRUE))
    expect_true(grepl("--- START OF FILE: R/script.R ---", result, fixed = TRUE))
    expect_true(grepl("Package: mytestpkg", result, fixed = TRUE))
    expect_false(grepl("--- START OF FILE: man/man.Rd ---", result, fixed = TRUE))
    expect_false(grepl("--- START OF FILE: data.bin ---", result, fixed = TRUE))
  })
})

test_that("collate_from_path handles custom include/exclude patterns", {
  withr::with_tempdir({
    create_dummy_project()
    result <- collate_from_path(
      ".",
      include_patterns = c("README\\.md$"),
      copy_to_clipboard = FALSE
    )
    expect_true(grepl("--- START OF FILE: README.md ---", result, fixed = TRUE))
    expect_false(grepl("--- START OF FILE: DESCRIPTION ---", result, fixed = TRUE))
  })
})

test_that("collate_from_path errors on non-existent path", {
  expect_error(
    collate_from_path("a/path/that/does/not/exist"),
    regexp = "The specified path does not exist"
  )
})


test_that("collate_from_path handles empty directories gracefully", {
  withr::with_tempdir({
    # expect_snapshot() captures all console output and compares it to a
    # stored snapshot. It's the correct way to test cli output.
    expect_snapshot({
      result <- collate_from_path(".", copy_to_clipboard = FALSE)
      # We also check the return value inside the snapshot block
      expect_equal(result, "")
    })
  })
})
