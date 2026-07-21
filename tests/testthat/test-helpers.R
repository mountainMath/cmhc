# Tests for the pure parsing helpers in R/helpers.R. These do not touch the network.

test_that("parse_numeric strips formatting and maps sentinel values", {
  expect_equal(parse_numeric("1,234"), 1234)
  expect_equal(parse_numeric("5 %"), 5)
  expect_equal(parse_numeric("7.5"), 7.5)
  # A dash denotes nil and is treated as zero
  expect_equal(parse_numeric("-"), 0)
  # Suppressed / unavailable markers become NA
  expect_true(is.na(parse_numeric("++")))
  expect_true(is.na(parse_numeric("n/a")))
  expect_true(is.na(parse_numeric("**")))
})

test_that("parse_numeric is vectorised and preserves order", {
  input <- c("1,234", "5 %", "-", "++", "n/a", "**", "7.5")
  expect_equal(parse_numeric(input), c(1234, 5, 0, NA, NA, NA, 7.5))
})

test_that("date_from_cmhc_year_month parses CMHC month-year strings", {
  expect_equal(date_from_cmhc_year_month("Oct 2019"), as.Date("2019-10-01"))
  expect_equal(date_from_cmhc_year_month(c("Oct 2019", "Jan 2020")),
               as.Date(c("2019-10-01", "2020-01-01")))
})

test_that("cmhc_quality_labels maps single-letter codes to plain text", {
  expect_equal(unname(cmhc_quality_labels[["a"]]), "Excellent")
  expect_equal(unname(cmhc_quality_labels[["d"]]), "Fair (Use with Caution)")
  expect_named(cmhc_quality_labels, c("a", "b", "c", "d"))
})
