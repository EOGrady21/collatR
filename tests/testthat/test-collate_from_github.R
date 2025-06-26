# tests/testthat/test-collate_from_github.R

test_that("collate_from_github successfully calls path function after mock clone", {
  withr::with_tempdir({
    mock_system2 <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        # Simulate successful clone
        repo_path <- args[length(args)]
        dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
        create_dummy_project(repo_path)
        return(character(0))
      }
      return(character(0))
    }

    mockery::stub(collate_from_github, "Sys.which", "/usr/bin/git")
    mockery::stub(collate_from_github, "system2", mock_system2)

    clear_collator_cache()

    result <- collate_from_github("user/repo", copy_to_clipboard = FALSE)

    expect_true(grepl("--- START OF FILE: DESCRIPTION ---", result, fixed = TRUE))
    expect_true(grepl("Package: mytestpkg", result, fixed = TRUE))
  })
})

test_that("collate_from_github errors gracefully on clone failure", {
  withr::with_tempdir({
    # Define a mock that always fails
    mock_system2_fail <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        return("fatal: repository not found")
      }
      return(character(0))
    }

    mockery::stub(collate_from_github, "Sys.which", "/usr/bin/git")
    mockery::stub(collate_from_github, "system2", mock_system2_fail)

    clear_collator_cache()

    # Check that our function catches the error and provides a user-friendly message
    expect_error(
      collate_from_github("user/repo", copy_to_clipboard = FALSE),
      regexp = "Repository not found"
    )
  })
})

test_that("collate_from_github validates repo name format", {
  expect_error(
    collate_from_github("invalid-repo-name"),
    regexp = "The `repo` format must be 'user/repo'"
  )
})

test_that("collate_from_github passes branch argument correctly", {
  withr::with_tempdir({
    # This variable will capture the branch argument
    captured_branch <- "---NOT SET---"

    # Mock function to capture the branch argument
    mock_system2_spy <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        # Find the branch argument if it exists
        branch_idx <- which(args == "--branch")
        if (length(branch_idx) > 0) {
          captured_branch <<- args[branch_idx + 1]
        } else {
          captured_branch <<- NULL
        }

        # Create the repo directory
        repo_path <- args[length(args)]
        dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
        create_dummy_project(repo_path)
        return(character(0))
      }
      return(character(0))
    }

    mockery::stub(collate_from_github, "Sys.which", "/usr/bin/git")
    mockery::stub(collate_from_github, "system2", mock_system2_spy)

    # --- Test 1: A specific branch is passed correctly ---
    clear_collator_cache()
    collate_from_github("user/repo", branch = "develop", copy_to_clipboard = FALSE)
    expect_equal(captured_branch, "develop")

    # --- Test 2: The default NULL value is passed correctly ---
    # Reset the capture variable before the next run
    captured_branch <- "---NOT SET---"
    clear_collator_cache()
    collate_from_github("user/repo2", copy_to_clipboard = FALSE)  # Different repo to avoid cache
    expect_null(captured_branch)
  })
})

test_that("collate_from_github successfully clones a new repo", {
  withr::with_tempdir({
    mock_system2 <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        repo_path <- args[length(args)]
        dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
        create_dummy_project(repo_path)
        return(character(0))
      }
      return(character(0))
    }

    mockery::stub(collate_from_github, "Sys.which", "/usr/bin/git")
    mockery::stub(collate_from_github, "system2", mock_system2)

    clear_collator_cache()

    expect_message(
      result <- collate_from_github("eogrady21/eogrady21", copy_to_clipboard = FALSE),
      "Cloning.*into temporary cache"
    )
    expect_type(result, "character")
  })
})

test_that("collate_from_github uses cache and pulls updates", {
  withr::with_tempdir({
    clone_count <- 0
    pull_count <- 0

    mock_system2 <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        clone_count <<- clone_count + 1
        repo_path <- args[length(args)]
        dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
        create_dummy_project(repo_path)
        return(character(0))
      } else if (any(grepl("pull", args))) {
        pull_count <<- pull_count + 1
        return(character(0))
      }
      return(character(0))
    }

    mockery::stub(collate_from_github, "Sys.which", "/usr/bin/git")
    mockery::stub(collate_from_github, "system2", mock_system2)

    clear_collator_cache()

    # First call should clone
    collate_from_github("user/repo", copy_to_clipboard = FALSE)
    expect_equal(clone_count, 1)
    expect_equal(pull_count, 0)

    # Second call should pull
    expect_message(
      result <- collate_from_github("user/repo", copy_to_clipboard = FALSE),
      "Pulling latest changes"
    )
    expect_equal(clone_count, 1)  # Still 1
    expect_equal(pull_count, 1)   # Now 1
  })
})
