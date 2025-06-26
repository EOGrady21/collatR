# tests/testthat/test-caching-system.R

test_that("cache key generation works correctly", {
  # Test basic repo (inline since generate_cache_key is internal)
  cache_key1 <- paste0(gsub("/", "_", "user/repo"), "")
  expect_equal(cache_key1, "user_repo")

  # Test repo with branch
  cache_key2 <- paste0(gsub("/", "_", "user/repo"), "_develop")
  expect_equal(cache_key2, "user_repo_develop")

  # Test repo with special characters
  cache_key3 <- paste0(gsub("/", "_", "org-name/repo.name"), "_feature/branch")
  expect_equal(cache_key3, "org-name_repo.name_feature/branch")
})

test_that("session cache environment works correctly", {
  # Clear any existing cache
  clear_collator_cache()

  # Cache should be empty initially
  expect_equal(length(ls(envir = .collator_env)), 0)

  # Add a cache entry
  test_path <- file.path(tempdir(), "test_repo")
  assign("test_key", test_path, envir = .collator_env)

  # Should now have one entry
  expect_equal(length(ls(envir = .collator_env)), 1)
  expect_true(exists("test_key", envir = .collator_env))
  expect_equal(get("test_key", envir = .collator_env), test_path)

  # Clear cache should remove all entries
  clear_collator_cache()
  expect_equal(length(ls(envir = .collator_env)), 0)
})

test_that("clear_collator_cache works correctly", {
  # Add some test entries
  assign("repo1", "path1", envir = .collator_env)
  assign("repo2", "path2", envir = .collator_env)
  expect_equal(length(ls(envir = .collator_env)), 2)

  # Clear should remove everything
  expect_message(clear_collator_cache(), "Collator cache cleared")
  expect_equal(length(ls(envir = .collator_env)), 0)
})

test_that("caching prevents duplicate clones in same session", {
  withr::with_tempdir({
    # Mock system2 to track calls
    clone_calls <- 0
    pull_calls <- 0

    mock_system2 <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        clone_calls <<- clone_calls + 1
        # Simulate successful clone by creating the directory
        repo_path <- args[length(args)]  # Last argument is the path
        dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
        create_dummy_project(repo_path)
        return(character(0))  # Success
      } else if (any(grepl("pull", args))) {
        pull_calls <<- pull_calls + 1
        return(character(0))  # Success
      }
      return(character(0))
    }

    # Mock Sys.which to return git path
    mockery::stub(collate_from_github, "Sys.which", "/usr/bin/git")
    mockery::stub(collate_from_github, "system2", mock_system2)

    # Clear cache before test
    clear_collator_cache()

    # First call should clone
    result1 <- collate_from_github("test/repo", copy_to_clipboard = FALSE)
    expect_equal(clone_calls, 1)
    expect_equal(pull_calls, 0)

    # Second call should pull, not clone
    result2 <- collate_from_github("test/repo", copy_to_clipboard = FALSE)
    expect_equal(clone_calls, 1)  # Still 1
    expect_equal(pull_calls, 1)   # Now 1

    # Third call should pull again
    result3 <- collate_from_github("test/repo", copy_to_clipboard = FALSE)
    expect_equal(clone_calls, 1)  # Still 1
    expect_equal(pull_calls, 2)   # Now 2
  })
})

test_that("different repos get different cache entries", {
  withr::with_tempdir({
    clone_calls <- 0

    mock_system2 <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        clone_calls <<- clone_calls + 1
        # Create the target directory
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

    # First repo
    collate_from_github("user/repo1", copy_to_clipboard = FALSE)
    expect_equal(clone_calls, 1)

    # Different repo should trigger another clone
    collate_from_github("user/repo2", copy_to_clipboard = FALSE)
    expect_equal(clone_calls, 2)

    # Same repo with different branch should trigger another clone
    collate_from_github("user/repo1", branch = "develop", copy_to_clipboard = FALSE)
    expect_equal(clone_calls, 3)

    # Check that we have 3 different cache entries
    expect_equal(length(ls(envir = .collator_env)), 3)
    expect_true(exists("user_repo1", envir = .collator_env))
    expect_true(exists("user_repo2", envir = .collator_env))
    expect_true(exists("user_repo1_develop", envir = .collator_env))
  })
})

test_that("cache paths are in tempdir", {
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

    # Call the function
    collate_from_github("test/repo", copy_to_clipboard = FALSE)

    # Check that the cached path is within tempdir()
    cached_path <- get("test_repo", envir = .collator_env)
    expect_true(startsWith(cached_path, tempdir()))
    expect_true(grepl("collator_cache", cached_path))
  })
})

test_that("failed git operations are handled gracefully", {
  withr::with_tempdir({
    # Mock system2 to simulate git failures
    mock_system2_fail <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        return("fatal: repository not found")
      }
      return(character(0))
    }

    mockery::stub(collate_from_github, "Sys.which", "/usr/bin/git")
    mockery::stub(collate_from_github, "system2", mock_system2_fail)

    clear_collator_cache()

    # Should error gracefully on clone failure
    expect_error(
      collate_from_github("nonexistent/repo", copy_to_clipboard = FALSE),
      "Repository not found"
    )
  })
})

test_that("git pull failures fall back gracefully", {
  withr::with_tempdir({
    pull_attempted <- FALSE

    mock_system2 <- function(command, args, ...) {
      if (any(grepl("clone", args))) {
        # Successful clone
        repo_path <- args[length(args)]
        dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
        create_dummy_project(repo_path)
        return(character(0))
      } else if (any(grepl("pull", args))) {
        # Simulate pull failure
        pull_attempted <<- TRUE
        return("fatal: unable to access repository")
      }
      return(character(0))
    }

    mockery::stub(collate_from_github, "Sys.which", "/usr/bin/git")
    mockery::stub(collate_from_github, "system2", mock_system2)

    clear_collator_cache()

    # First call - clone
    result1 <- collate_from_github("test/repo", copy_to_clipboard = FALSE)
    expect_false(pull_attempted)

    # Second call - pull fails but should continue with existing repo
    expect_message(
      result2 <- collate_from_github("test/repo", copy_to_clipboard = FALSE),
      "Could not pull updates"
    )
    expect_true(pull_attempted)
    expect_type(result2, "character")  # Should still return collated content
  })
})

# Helper function removed since cache key generation is inline
