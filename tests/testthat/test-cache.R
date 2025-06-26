# tests/testthat/test-cache.R

test_that("get_collator_cache_dir returns a character path", {
  path <- get_collator_cache_dir()
  expect_type(path, "character")
  expect_true(grepl("collator", path, ignore.case = TRUE))
})

test_that("clear_collator_cache handles an empty cache", {
  withr::with_tempdir({
    mockery::stub(clear_collator_cache, "get_collator_cache_dir", \() ".")
    expect_message(clear_collator_cache(force = TRUE), "Cache is already empty")
  })
})

test_that("clear_collator_cache deletes files with force = TRUE", {
  withr::with_tempdir({
    # Create a subdirectory to act as the cache
    dir.create("my_cache")
    mockery::stub(clear_collator_cache, "get_collator_cache_dir", \() "my_cache")

    writeLines("test", "my_cache/dummy_file.txt")
    expect_true(file.exists("my_cache/dummy_file.txt"))

    clear_collator_cache(force = TRUE)

    # The directory is recreated, but it should be empty
    expect_equal(length(list.files("my_cache")), 0)
  })
})




