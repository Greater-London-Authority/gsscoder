test_that("get_gss_names produces the expected outputs", {
  expect_equal(get_gss_names(test_codes$y2009, gss_year = 2009), test_codes_names$y2009)
  expect_equal(get_gss_names(test_codes$y2012, gss_year = 2012), test_codes_names$y2012)
  expect_equal(get_gss_names(test_codes$y2013, gss_year = 2013), test_codes_names$y2013)
  expect_equal(get_gss_names(test_codes$y2018, gss_year = 2018), test_codes_names$y2018) 
  expect_equal(get_gss_names(test_codes$y2019, gss_year = 2019), test_codes_names$y2019)
  expect_equal(get_gss_names(test_codes$y2020, gss_year = 2020), test_codes_names$y2020)
  expect_equal(get_gss_names(test_codes$y2021, gss_year = 2021), test_codes_names$y2021)
  expect_equal(get_gss_names(test_codes$y2023, gss_year = 2023), test_codes_names$y2023)
})

