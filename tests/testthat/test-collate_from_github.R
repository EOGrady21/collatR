# tests/testthat/test-collate_from_github.R

# This helper function is defined in the other test file, which is fine
# as testthat loads all helpers and test files.

test_that("collate_from_github successfully calls path function after mock clone", {
  mock_git_clone <- function(url, path, branch = NULL) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    create_dummy_project(path) # Assumes this helper is defined elsewhere
  }

  mockery::stub(
    where = collate_from_github,
    what = "gert::git_clone",
    how = mock_git_clone
  )

  result <- collate_from_github("user/repo", copy_to_clipboard = FALSE)

  expect_true(grepl("--- START OF FILE: DESCRIPTION ---", result, fixed = TRUE))
  expect_true(grepl("Package: mytestpkg", result, fixed = TRUE))
})

test_that("collate_from_github errors gracefully on clone failure", {
  # Define a mock that *always* fails
  mock_git_clone_fail <- function(url, path, branch = NULL) {
    stop("Simulated git clone failure")
  }

  mockery::stub(collate_from_github, "gert::git_clone", mock_git_clone_fail)

  # Check that our function catches the error and provides a user-friendly message
  expect_error(
    collate_from_github("user/repo"),
    regexp = "Failed to clone repository"
  )
})

test_that("collate_from_github validates repo name format", {
  expect_error(
    collate_from_github("invalid-repo-name"),
    regexp = "The `repo` format must be 'user/repo'"
  )
})

test_that("collate_from_github passes branch argument to gert", {
  # This variable will live outside the mock to capture the argument
  captured_branch <- "---NOT SET---"

  # This mock function will capture the 'branch' argument it receives
  # and then perform the standard dummy project creation.
  mock_git_clone_spy <- function(url, path, branch = NULL) {
    # Use '<<-' to assign to the 'captured_branch' in the parent environment
    captured_branch <<- branch
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    create_dummy_project(path) # Assumes helper is defined elsewhere
  }

  mockery::stub(
    where = collate_from_github,
    what = "gert::git_clone",
    how = mock_git_clone_spy
  )

  # --- Test 1: A specific branch is passed correctly ---
  collate_from_github("user/repo", branch = "develop", copy_to_clipboard = FALSE)
  expect_equal(captured_branch, "develop")

  # --- Test 2: The default NULL value is passed correctly ---
  # Reset the capture variable before the next run
  captured_branch <- "---NOT SET---"
  collate_from_github("user/repo", copy_to_clipboard = FALSE)
  expect_null(captured_branch)
})
