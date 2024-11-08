all_codes_dates <- readRDS("data-raw/all_codes_dates.rds")
code_changes <- readRDS("data-raw/code_changes.rds")
database_date <- readRDS("data-raw/database_date.rds")
geog_levels <- readRDS("data-raw/geog_levels.rds")
test_codes <- readRDS("data-raw/test_codes.rds")
test_names <- readRDS("data-raw/test_names.rds")
test_codes_names <- readRDS("data-raw/test_codes_names.rds")


usethis::use_data(all_codes_dates, code_changes, geog_levels, database_date, test_codes, test_names, test_codes_names, internal = TRUE, overwrite = TRUE)
rm(all_codes_dates, code_changes, geog_levels, database_date, test_codes, test_names, test_codes_names)
