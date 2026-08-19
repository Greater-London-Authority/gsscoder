.sys_all_codes_dates <- readRDS("data-raw/all_codes_dates.rds")
.sys_code_changes <- readRDS("data-raw/code_changes.rds")
.sys_database_date <- readRDS("data-raw/database_date.rds")
.sys_geog_levels <- readRDS("data-raw/geog_levels.rds")
.sys_entity_levels <- readRDS("data-raw/entity_levels.rds")
.sys_lad_region_country <- readRDS("data-raw/lad_region_country.rds")

.sys_test_codes <- readRDS("data-raw/test_codes.rds")
.sys_test_codes_names <- readRDS("data-raw/test_codes_names.rds")
.sys_test_codes_parents <- readRDS("data-raw/test_codes_parents.rds")

usethis::use_data(.sys_all_codes_dates, .sys_code_changes, .sys_database_date, .sys_geog_levels, .sys_entity_levels, .sys_lad_region_country,
                  .sys_test_codes, .sys_test_codes_names, .sys_test_codes_parents, internal = TRUE, overwrite = TRUE)
rm(.sys_all_codes_dates, .sys_code_changes, .sys_database_date, .sys_geog_levels, .sys_entity_levels, .sys_lad_region_country,
                  .sys_test_codes, .sys_test_codes_names, .sys_test_codes_parents)
