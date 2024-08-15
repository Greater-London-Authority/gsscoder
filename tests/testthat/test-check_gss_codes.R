
test_that("check_gss_codes passes when expected", {
  expect_no_error(check_gss_codes(test_codes$y2009, gss_year = 2009, expect_complete = TRUE, include_wales = TRUE))
  expect_no_error(check_gss_codes(test_codes$y2012, gss_year = 2012, expect_complete = TRUE, include_wales = TRUE))
  expect_no_error(check_gss_codes(test_codes$y2013, gss_year = 2013, expect_complete = TRUE, include_wales = TRUE))
  expect_no_error(check_gss_codes(test_codes$y2018, gss_year = 2018, expect_complete = TRUE, include_wales = TRUE)) 
  expect_no_error(check_gss_codes(test_codes$y2019, gss_year = 2019, expect_complete = TRUE, include_wales = TRUE))
  expect_no_error(check_gss_codes(test_codes$y2020, gss_year = 2020, expect_complete = TRUE, include_wales = TRUE))
  expect_no_error(check_gss_codes(test_codes$y2021, gss_year = 2021, expect_complete = TRUE, include_wales = TRUE))
  expect_no_error(check_gss_codes(test_codes$y2023, gss_year = 2023, expect_complete = TRUE, include_wales = TRUE))
})

# TODO fix this when check_gss_codes is changed to always give errors and never warnings
# test_that("check_gss_codes fails when expected", {
#   expect_error(check_gss_codes(test_codes$y2009, gss_year = 2018, expect_complete = TRUE, include_wales = TRUE))
# })
