# Tests for the geographic-identifier helpers in R/cmhc_geography.R. No network access.

test_that("cmhc_geo_level_for_census dispatches on GeoUID length", {
  expect_equal(cmhc_geo_level_for_census("59933"), "CMA")   # 5-digit CMA
  expect_equal(cmhc_geo_level_for_census("5915022"), "CSD") # 7-digit CSD
  expect_equal(cmhc_geo_level_for_census("35"), "PR")       # 2-digit province
  expect_equal(cmhc_geo_level_for_census("1"), "C")         # Canada
  # The census code for all of Canada is "01"; it maps to the Canada level
  expect_equal(cmhc_geo_level_for_census("01"), "C")
})

test_that("cmhc_geo_level_for_cmhc distinguishes CT, CSD and CMA", {
  expect_equal(cmhc_geo_level_for_cmhc("2410"), "CMA")       # 4-char metro code
  expect_equal(cmhc_geo_level_for_cmhc("5350039"), "CSD")    # 7-char, no dot
  expect_equal(cmhc_geo_level_for_cmhc("590.123"), "CT")     # 7-char, with dot
})

test_that("census_to_cmhc_geocode translates census geographies to CMHC codes", {
  # Vancouver CMA (census 59933) is CMHC metro code 2410
  expect_equal(census_to_cmhc_geocode("59933"), "2410")
  # Canada
  expect_equal(census_to_cmhc_geocode("01"), "1")
  # CSDs pass through unchanged
  expect_equal(census_to_cmhc_geocode("5915022"), "5915022")
})

test_that("cmhc_region_params_from_census assembles API parameters", {
  params <- cmhc_region_params_from_census("59933")
  expect_equal(params$geography_type_id, "3")   # CMA
  expect_equal(params$geography_id, "2410")
})
