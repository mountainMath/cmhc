# Tests for the table registry and discovery helpers in R/cmhc_tables.R.
# These read the hardcoded catalogue only and do not touch the network.

test_that("list_cmhc_tables returns a consistent registry", {
  short <- suppressWarnings(list_cmhc_tables(short = TRUE))
  full <- suppressWarnings(list_cmhc_tables(short = FALSE))

  expect_s3_class(short, "tbl_df")
  expect_gt(nrow(short), 0)
  expect_true(all(c("Survey", "Series", "Dimension", "Breakdown", "GeoFilter", "Filters")
                  %in% names(short)))
  # The full form additionally carries the CMHC table code used for API calls
  expect_true("TableCode" %in% names(full))
  expect_false(any(is.na(full$TableCode)))
})

test_that("discovery helpers narrow the registry as filters are applied", {
  surveys <- suppressWarnings(list_cmhc_surveys())
  expect_true("Rms" %in% surveys$Survey)

  series <- suppressWarnings(list_cmhc_series("Rms"))
  expect_true(all(series$Survey == "Rms"))
  expect_true("Vacancy Rate" %in% series$Series)

  dims <- suppressWarnings(list_cmhc_dimensions("Rms", "Vacancy Rate"))
  expect_true("Bedroom Type" %in% dims$Dimension)

  breakdowns <- suppressWarnings(
    list_cmhc_breakdowns("Rms", "Vacancy Rate", "Bedroom Type"))
  expect_true("Historical Time Periods" %in% breakdowns$Breakdown)
})
