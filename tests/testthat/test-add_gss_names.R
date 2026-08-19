test_that("add_gss_names produces the expected outputs", {
  expect_equal(add_gss_names(.sys_test_codes$y2009, gss_year = 2009), select(.sys_test_codes_names$y2009, gss_code, gss_name, entity))
  expect_equal(add_gss_names(.sys_test_codes$y2012, gss_year = 2012), select(.sys_test_codes_names$y2012, gss_code, gss_name, entity))
  expect_equal(add_gss_names(.sys_test_codes$y2013, gss_year = 2013), select(.sys_test_codes_names$y2013, gss_code, gss_name, entity))
  expect_equal(add_gss_names(.sys_test_codes$y2018, gss_year = 2018), select(.sys_test_codes_names$y2018, gss_code, gss_name, entity)) 
  expect_equal(add_gss_names(.sys_test_codes$y2019, gss_year = 2019), select(.sys_test_codes_names$y2019, gss_code, gss_name, entity))
  expect_equal(add_gss_names(.sys_test_codes$y2020, gss_year = 2020), select(.sys_test_codes_names$y2020, gss_code, gss_name, entity))
  expect_equal(add_gss_names(.sys_test_codes$y2021, gss_year = 2021), select(.sys_test_codes_names$y2021, gss_code, gss_name, entity))
  expect_equal(add_gss_names(.sys_test_codes$y2023, gss_year = 2023), select(.sys_test_codes_names$y2023, gss_code, gss_name, entity))
})

