library(dplyr)

all_codes <- readRDS("data-raw/all_codes_dates.rds") %>%
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
names(change_years) <- paste0("y",change_years)


subset_by_date <- function(year, extra_cols, include_counties) {
  end_of_year <- as.Date(paste0(as.character(year), "-12-31"))
  data <- all_codes %>%
    filter(start_date <= end_of_year,
           (is.na(end_date) | end_date >= end_of_year)) %>%
    mutate(entity = substr(gss_code, 1,3)) %>%
    select(gss_code, all_of(extra_cols)) %>%
    #select(gss_code, entity, gss_name, parent_cd) %>%
    arrange(gss_code)
  
  if (!include_counties) {
    data <- data %>%
      filter(!grepl("E10", gss_code))
  }
  
  return(data)
}

test_codes <- lapply(change_years, subset_by_date, extra_cols = c("entity"), include_counties = FALSE)
test_codes_names <- lapply(change_years, subset_by_date, extra_cols = c("entity", "gss_name"), include_counties = FALSE)
test_codes_parents <- lapply(change_years, subset_by_date, extra_cols = c("entity", "gss_name", "parent_cd"), include_counties = TRUE)

saveRDS(test_codes, "data-raw/test_codes.rds")
saveRDS(test_codes_names, "data-raw/test_codes_names.rds")
saveRDS(test_codes_parents, "data-raw/test_codes_parents.rds")

rm(all_codes, change_dates, change_years, subset_by_date,
  test_codes, test_codes_names, test_codes_parents)

