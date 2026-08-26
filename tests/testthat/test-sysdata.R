dates <- unique(c(.sys_lad_region_country$start_date, .sys_lad_region_country$end_date)) %>%
  sort()

check_duplicates <- function(date, df, geog, parent_geog) {
  
  df2 <- df %>% 
    select(all_of(c(geog, parent_geog)), start_date, end_date) %>%
    filter(start_date <= date & (end_date >= date | is.na(end_date))) %>%
    select(-start_date, -end_date) %>%
    unique()
  
  if (any(duplicated(df2[[geog]]))) stop(paste0("There are duplicate ", geog, " codes in the parent geogs lookup"))
  
  invisible()
}

check_missing <- function(date, df, geog, parent_geog) {
  
  df2 <- df %>% 
    select(all_of(c(geog, parent_geog)), start_date, end_date)  %>%
    filter(start_date <= date & (end_date >= date | is.na(end_date))) %>%
    select(-start_date, -end_date) %>%
    unique()
  
  check_gss_codes(df2, col_code = geog,
                  gss_date = date,
                  expect_complete = TRUE,
                  include_wales = TRUE)

  invisible()
}

test_that("There are no duplicates in lad_region_country lookup at any point in time", {
  expect_silent(lapply(dates, check_duplicates, .sys_lad_region_country, "lad", "region"))
  expect_silent(lapply(dates, check_duplicates, .sys_lad_region_country, "region", "country"))
})

test_that("There are no missing codes in lad_region_country lookup at any point in time", {
  expect_silent(lapply(dates, check_missing, .sys_lad_region_country, "lad", "region"))
  expect_silent(lapply(dates, check_missing, .sys_lad_region_country, "region", "country"))
})

