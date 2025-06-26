# tests/testthat/test-collate_from_github.R

test_that("collate_from_github successfully clones a new repo", {
  # Mock system2 to simulate a successful git clone (returns nothing on success)
  mock_clone <- mockery::mock(NULL)
  # Mock collate_from_path to prevent it from actually running
  mockery::stub(collate_from_github, "collate_from_path", \(...) "ok")

  withr::with_tempdir({
    # Point the cache to a temporary directory for this test
    mockery::stub(collate_from_github, "rappdirs::user_cache_dir", \(...) ".")
    mockery::stub(collate_from_github, "system2", mock_clone)

    expect_message(
      result <- collate_from_github("eogrady21/eogrady21"),
      "Cloning 'eogrady21/eogrady21' into cache..."
    )
    expect_equal(result, "ok")

    # Check that our system2 mock was called once for the clone
    mockery::expect_called(mock_clone, 1)
  })
})

test_that("collate_from_github uses cache and pulls updates", {
  # Mock system2 for a successful git pull
  mock_pull <- mockery::mock(NULL)
  mockery::stub(collate_from_github, "collate_from_path", \(...) "ok")

  withr::with_tempdir({
    mockery::stub(collate_from_github, "rappdirs::user_cache_dir", \(...) ".")
    mockery::stub(collate_from_github, "system2", mock_pull)

    # Create a dummy repo directory to trigger the "pull" logic
    dir.create("user/repo", recursive = TRUE)

    expect_message(
      result <- collate_from_github("user/repo"),
      "Pulling latest changes"
    )
    expect_equal(result, "ok")
    mockery::expect_called(mock_pull, 1)
  })
})

test_that("collate_from_github handles clone failure", {
  # Mock system2 to simulate a failure by returning a warning object
  mockery::stub(collate_from_github, "system2", \(...) warning("fatal: repo not found"))

  withr::with_tempdir({
    mockery::stub(collate_from_github, "rappdirs::user_cache_dir", \(...) ".")

    expect_error(
      collate_from_github("user/repo"),
      "Git clone failed."
    )
  })
})
