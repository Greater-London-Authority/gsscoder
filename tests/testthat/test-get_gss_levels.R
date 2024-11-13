

test_data_lad_region <- test_codes_names$y2019 %>%
  filter(entity %in% c("E06", "E07", "E08", "E09", "W06", "E12", "W92"))

test_data_lad_region_country <- test_codes_names$y2019 %>%
  filter(entity %in% c("E06", "E07", "E08", "E09", "W06", "E12", "E92", "W92"))

test_data_lad_country <- test_codes_names$y2019 %>%
  filter(entity %in% c("E06", "E07", "E08", "E09", "W06", "E92", "W92"))

test_data_error <- test_codes_names$y2019 %>%
  filter(entity %in% c("E06", "E07", "E08", "E09", "W06", "E92", "W92")) %>%
  mutate(gss_code = ifelse(gss_code == "E92000001", "S92000003", gss_code))

test_that("get_gss_levels produces expected output", {
  expect_equal(get_gss_levels(test_data_lad_region), c("lad", "region"))
  expect_equal(get_gss_levels(test_data_lad_region_country), c("lad", "region", "country"))
  expect_equal(get_gss_levels(test_data_lad_country), c("lad", "country"))
})

test_that("get_gss_levels fails when expected", {
  expect_error(get_gss_levels(test_data_error))
})