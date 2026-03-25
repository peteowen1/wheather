test_that("validate_date_range accepts valid ranges", {
  expect_no_error(validate_date_range("2024-01-01", "2024-12-31"))
  expect_no_error(validate_date_range("2024-01-01", "2024-01-01"))  # same day
})

test_that("validate_date_range rejects start after end", {
  expect_error(validate_date_range("2024-12-31", "2024-01-01"), "must be before")
})

test_that("validate_date_range rejects invalid dates", {
  expect_error(validate_date_range("not-a-date", "2024-01-01"), "not a valid date")
  expect_error(validate_date_range("2024-01-01", "nope"), "not a valid date")
})

test_that("validate_date_range includes label in error", {
  expect_error(validate_date_range("2024-12-31", "2024-01-01", "Period 1"), "Period 1")
})

test_that("split_contiguous splits on gaps", {
  dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-05", "2024-01-06"))
  result <- split_contiguous(dates)
  expect_length(result, 2)
  expect_equal(result[[1]], as.Date(c("2024-01-01", "2024-01-02")))
  expect_equal(result[[2]], as.Date(c("2024-01-05", "2024-01-06")))
})

test_that("split_contiguous returns single range for contiguous dates", {
  dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  result <- split_contiguous(dates)
  expect_length(result, 1)
  expect_equal(result[[1]], dates)
})

test_that("split_contiguous handles single date", {
  dates <- as.Date("2024-01-01")
  result <- split_contiguous(dates)
  expect_length(result, 1)
  expect_equal(result[[1]], dates)
})

test_that("round_coords rounds to 2 decimal places by default", {
  result <- round_coords(51.5074, -0.1278)
  expect_equal(result$lat, 51.51)
  expect_equal(result$lon, -0.13)
})
