library(dplyr)

all_codes <- readRDS("data-raw/all_lad_codes_dates.rds") %>%
  mutate(start_date = if_else(start_date == as.Date("2009-01-01"), # change to the day before so that the starting set of codes is given as 2008 (there were more changes later in 2009)
                             as.Date("2008-12-31"), 
                             start_date))

# Code changes happen in these years:
# 2009, 2010, 2012, 2013, 2018, 2019, 2020, 2021, 2023
change_dates <-  all_codes$start_date %>%
  unique() %>%
  sort()

change_years <-  format(as.Date(change_dates, format="%d/%m/%Y"),"%Y") %>%
  unique()

#### CODES ####

get_year_codes <- function(year) {
  end_of_year <- as.Date(paste0(as.character(year), "-12-31"))
  data <- all_codes %>%
    filter(start_date <= end_of_year,
           (is.na(end_date) | end_date >= end_of_year)) %>%
    select(gss_code) %>%
    arrange(gss_code)
  return(data)
}

test_codes <- lapply(change_years, get_year_codes)
names(test_codes) <- paste0("y",change_years)


#### NAMES ####

get_year_names <- function(year) {
  end_of_year <- as.Date(paste0(as.character(year), "-12-31"))
  data <- all_codes %>%
    filter(start_date <= end_of_year,
           (is.na(end_date) | end_date >= end_of_year)) %>%
    select(gss_name) %>%
    arrange(gss_name)
  return(data)
}

test_names <- lapply(change_years, get_year_names)
names(test_names) <- paste0("y",change_years)

saveRDS(test_codes, "data-raw/test_codes.rds")
saveRDS(test_names, "data-raw/test_names.rds")

rm(all_codes, test_codes, test_names, change_dates, change_years, get_year_codes, get_year_names)

