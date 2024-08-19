
# test_codes is internal package variable stored in R/sysdata.rda and created in data-raw/testthat_data.R

test_that("get_gss_date produces the expected outputs", {
  expect_equal(get_gss_date(test_codes$y2009), 
               list(earliest = as.Date("2009-06-01"), latest = as.Date("2012-03-31")))
  expect_equal(get_gss_date(test_codes$y2012), 
               list(earliest = as.Date("2012-04-01"), latest = as.Date("2013-03-31")))
  expect_equal(get_gss_date(test_codes$y2013), 
               list(earliest = as.Date("2013-04-01"), latest = as.Date("2019-03-31")))
  expect_equal(get_gss_date(test_codes$y2018), 
               list(earliest = as.Date("2013-04-01"), latest = as.Date("2019-03-31"))) # 2018 was only a name change, codes are same as 2013 codes
  expect_equal(get_gss_date(test_codes$y2019), 
               list(earliest = as.Date("2019-04-01"), latest = as.Date("2020-03-31")))
  expect_equal(get_gss_date(test_codes$y2020), 
               list(earliest = as.Date("2020-04-01"), latest = as.Date("2021-03-31")))
  expect_equal(get_gss_date(test_codes$y2021), 
               list(earliest = as.Date("2021-04-01"), latest = as.Date("2023-03-31")))
  expect_equal(get_gss_date(test_codes$y2023), 
               list(earliest = as.Date("2023-04-01"), latest = as.Date(NA)))
})

test_that("get_gss_year produces the expected outputs", {
  expect_equal(get_gss_year(test_codes$y2009), 2009)
  expect_equal(get_gss_year(test_codes$y2012), 2012)
  expect_equal(get_gss_year(test_codes$y2013), 2013)
  expect_equal(get_gss_year(test_codes$y2018), 2013) # 2018 was only a name change, codes are same as 2013 codes
  expect_equal(get_gss_year(test_codes$y2019), 2019)
  expect_equal(get_gss_year(test_codes$y2020), 2020)
  expect_equal(get_gss_year(test_codes$y2021), 2021)
  expect_equal(get_gss_year(test_codes$y2023), 2023)
})

# TODO test for failure if the gss codes arent a consisnent said

# TODO test that the validates fail appropriately

