create_lad_region <- function(df) {
  counties <- df %>% 
    filter(entity %in% .sys_entity_levels$county) %>% 
    select(gss_code, parent_cd)
  lads <- df %>% 
    filter(entity %in% .sys_entity_levels$lad) %>%
    left_join(counties, by = c("parent_cd" = "gss_code")) %>%
    mutate(parent_entity = substr(parent_cd, 1, 3),
           parent_cd = ifelse(parent_entity %in% .sys_entity_levels$county, parent_cd.y, parent_cd)) %>%
    select(gss_code, entity, gss_name, region = parent_cd)
  return(lads)
}

create_lad_country <- function(df) {
  lad_country <- df %>%
    filter(entity %in% .sys_entity_levels$lad) %>%
    mutate(country = 
             case_when(substr(gss_code, 1, 1) == "E" ~ "E92000001",
                       substr(gss_code, 1, 1) == "W" ~ "W92000004")) %>%
    select(gss_code, entity, gss_name, country)
  return(lad_country)
}

create_region_country <- function(df) {
  regions <- df %>% 
    filter(entity %in% .sys_entity_levels$region) %>%
    select(gss_code, entity, gss_name, country = parent_cd)
  return(regions)
}

lad_region <- lapply(.sys_test_codes_parents, create_lad_region)
lad_country <- lapply(.sys_test_codes_parents, create_lad_country)
region_country <- lapply(.sys_test_codes_parents, create_region_country)

lad_region_test <- lapply(lad_region, function(x) select(x, -region))
lad_country_test <- lapply(lad_country, function(x) select(x, -country))
region_country_test <- lapply(region_country, function(x) select(x, -country))


test_that("add_higher_geog produces the expected outputs for lad to region", {
  expect_equal(add_higher_geog(lad_region_test$y2009, current_geog = "lad", higher_geog = "region", gss_year = 2009), 
               lad_region$y2009)
  expect_equal(add_higher_geog(lad_region_test$y2012, current_geog = "lad", higher_geog = "region", gss_year = 2012), 
               lad_region$y2012)
  expect_equal(add_higher_geog(lad_region_test$y2013, current_geog = "lad", higher_geog = "region", gss_year = 2013), 
               lad_region$y2013)
  expect_equal(add_higher_geog(lad_region_test$y2018, current_geog = "lad", higher_geog = "region", gss_year = 2018), 
               lad_region$y2018)
  expect_equal(add_higher_geog(lad_region_test$y2019, current_geog = "lad", higher_geog = "region", gss_year = 2019), 
               lad_region$y2019)
  expect_equal(add_higher_geog(lad_region_test$y2020, current_geog = "lad", higher_geog = "region", gss_year = 2020), 
               lad_region$y2020)
  expect_equal(add_higher_geog(lad_region_test$y2021, current_geog = "lad", higher_geog = "region", gss_year = 2021), 
               lad_region$y2021)
  expect_equal(add_higher_geog(lad_region_test$y2023, current_geog = "lad", higher_geog = "region", gss_year = 2023), 
               lad_region$y2023)
})

test_that("add_higher_geog produces the expected outputs for lad to country", {
  expect_equal(add_higher_geog(lad_country_test$y2009, current_geog = "lad", higher_geog = "country", gss_year = 2009), 
               lad_country$y2009)
  expect_equal(add_higher_geog(lad_country_test$y2012, current_geog = "lad", higher_geog = "country", gss_year = 2012), 
               lad_country$y2012)
  expect_equal(add_higher_geog(lad_country_test$y2013, current_geog = "lad", higher_geog = "country", gss_year = 2013), 
               lad_country$y2013)
  expect_equal(add_higher_geog(lad_country_test$y2018, current_geog = "lad", higher_geog = "country", gss_year = 2018), 
               lad_country$y2018)
  expect_equal(add_higher_geog(lad_country_test$y2019, current_geog = "lad", higher_geog = "country", gss_year = 2019), 
               lad_country$y2019)
  expect_equal(add_higher_geog(lad_country_test$y2020, current_geog = "lad", higher_geog = "country", gss_year = 2020), 
               lad_country$y2020)
  expect_equal(add_higher_geog(lad_country_test$y2021, current_geog = "lad", higher_geog = "country", gss_year = 2021), 
               lad_country$y2021)
  expect_equal(add_higher_geog(lad_country_test$y2023, current_geog = "lad", higher_geog = "country", gss_year = 2023), 
               lad_country$y2023)
})

test_that("add_higher_geog produces the expected outputs for region to country", {
  expect_equal(add_higher_geog(region_country_test$y2009, current_geog = "region", higher_geog = "country", gss_year = 2009), 
               region_country$y2009)
  expect_equal(add_higher_geog(region_country_test$y2012, current_geog = "region", higher_geog = "country", gss_year = 2012), 
               region_country$y2012)
  expect_equal(add_higher_geog(region_country_test$y2013, current_geog = "region", higher_geog = "country", gss_year = 2013), 
               region_country$y2013)
  expect_equal(add_higher_geog(region_country_test$y2018, current_geog = "region", higher_geog = "country", gss_year = 2018), 
               region_country$y2018)
  expect_equal(add_higher_geog(region_country_test$y2019, current_geog = "region", higher_geog = "country", gss_year = 2019), 
               region_country$y2019)
  expect_equal(add_higher_geog(region_country_test$y2020, current_geog = "region", higher_geog = "country", gss_year = 2020), 
               region_country$y2020)
  expect_equal(add_higher_geog(region_country_test$y2021, current_geog = "region", higher_geog = "country", gss_year = 2021), 
               region_country$y2021)
  expect_equal(add_higher_geog(region_country_test$y2023, current_geog = "region", higher_geog = "country", gss_year = 2023), 
               region_country$y2023)
})
