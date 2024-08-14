all_lad_codes_dates <- readRDS("data-raw/all_lad_codes_dates.rds")
lad_code_changes <- readRDS("data-raw/lad_code_changes.rds")
database_date <- readRDS("data-raw/database_date.rds")
test_codes <- readRDS("data-raw/test_codes.rds")
test_names <- readRDS("data-raw/test_names.rds")
test_codes_names <- readRDS("data-raw/test_codes_names.rds")


usethis::use_data(all_lad_codes_dates, lad_code_changes, database_date, test_codes, test_names, test_codes_names, internal = TRUE, overwrite = TRUE)
rm(all_lad_codes_dates, lad_code_changes, database_date, test_codes, test_names, test_codes_names)
