# This script takes the ONS GSS code changes files and prepares them for use in our gss recoding function
# This is currently only for local authorities and covers England and Wales.

# The ONS file needs to be found and downloaded manually on the open geography portal website
# e.g. Code History Database (December 2023) for the UK at https://geoportal.statistics.gov.uk/datasets/3acc892515aa49a8885c2deb734ebd3d/about
# Files should be saved into the data-raw folder before running this script

# The files needed from the zip file are 'Changes.csv' and 'ChangeHistory.csv' which should have the following columns:

#'Changes.csv'
# This shows the to and from for each code or name that was changed.  It only contains codes which have been part of a change

# "GEOGCD"    geography GSS code
# "GEOGNM"    name
# "GEOGNMW"   name in Welsh
# "GEOGCD_P"  previous GSS code
# "GEOGNM_P"  previous name
# "GEOGNMW_P" previous name in Welsh 
# "SI_ID"     ID for the legislation that created the new code
# "SI_TITLE"  title for the legislation that created the new code
# "OPER_DATE" date that the code became operational
# "ENTITYCD"  first part of the code that specifies the geography level that it is describing
# "YEAR"      year that the code became operational

#'ChangeHistory.csv'
# This contains all codes along with their start dates and finish dates if they have been terminated

# "GEOGCD"      geography GSS code  
# "GEOGNM"      name
# "GEOGNMW"     name in Welsh
# "SI_ID"       ID for the legislation that created the new code
# "SI_TITLE"    title for the legislation that created the new code
# "OPER_DATE"   date that the code became operational
# "TERM_DATE"   date that the code was terminated
# "PARENTCD" 
# "ENTITYCD"    first part of the code that specifies the geography level that it is describing
# "OWNER"
# "STATUS"      whether the code is 'Live', 'live' or 'terminated'
# "AREAEHECT"
# "AREACHECT"
# "AREAIHECT"
# "AREALHECT"


library(dplyr)
library(stringr)

code_changes <- read.csv("data-raw/Changes.csv", stringsAsFactors = FALSE)

database_date <- readline(prompt="Enter Code History Database Date (from downloaded zip file name) in format yyyy-mm: ")

while(!grepl("^\\d{4}-\\d{2}$", database_date)) {
  database_date <- readline(prompt = "Unexpected format for Code History Database Date entered. Please try again with format yyyy-mm: ")
}

database_date <- as.Date(paste0(database_date, "-31"))

if (database_date > Sys.Date()) warning("The Code History Database Date given is in the future. This is unlikely to be correct, please double check.")
if (database_date < as.Date("2009-01-01")) stop("The Code History Database Date cannot be any earlier than 2009-01-01")


geogs_of_interest <- c("E06", "E07", "E08", "E09", "W06") # LAD level geographies only
# geogs_of_interest <- c("E06", "E07", "E08", "E09", "W06",
#                       "E10", #"E11", "W10",
#                       "E12",
#                       #"E13",
#                       "E92", "W92", "S92", "N92") # LAD level geographies and above
lad_code_changes <- filter(code_changes, ENTITYCD %in% geogs_of_interest) %>% # LAD level geographies only
  mutate(old_entity = substr(GEOGCD_P, 1, 3)) %>%
  filter(old_entity %in% geogs_of_interest) %>% # many of the code changes in the database are from 2009 when the new 9 digit codes were implemented replacing the old style codes.  We're only interested in changes after this point.
  select(-GEOGNMW, -GEOGNMW_P)

# add columns to specify whether the changes involve merges and/or splits

# merges are defined as multiple codes becoming a single code, so there will be 
# duplicates of the new code in the data

# splits are defined as a single code becoming multiple codes, so there will be 
# duplicates of the old code in the data

# a couple of code changes involve both splits and merges where a part of one LA
# has been moved into another

merges <- count(lad_code_changes, GEOGCD) %>% filter(n>1)
splits <- count(lad_code_changes, GEOGCD_P) %>% filter(n>1)

lad_code_changes <- lad_code_changes %>% mutate(split = ifelse(GEOGCD_P %in% splits$GEOGCD_P, TRUE, FALSE),
                                      merge = ifelse(GEOGCD %in% merges$GEOGCD, TRUE, FALSE),
                                      OPER_DATE = as.Date(OPER_DATE, "%d/%m/%Y")) %>%
  select(changed_to_code = GEOGCD,
         changed_to_name = GEOGNM,
         changed_from_code = GEOGCD_P,
         changed_from_name = GEOGNM_P,
         desc = SI_TITLE,
         entity_type = ENTITYCD,
         date = OPER_DATE,
         year = YEAR,
         split,
         merge)

rm(merges, splits)  

# The 3 splits in 2009 & 2013 were all minor alterations to boundaries.
# For each of these a small part of one LA has been moved into the other and both LAs have had a code change.
# Gateshead to Northumberland = 1 bungalow
# East Hertfordshire to Stevenage = part of a row of houses
# Powys to Merthyr Tydfil = a few buildings
# All can effectively be treated as code changes with no tranfser of population.
# For each boundary change: 1) delete the row which contains the transfer from one LA to another
#                           2) for the code change rows set both split and merge columns to FALSE

descs <- c("The Gateshead and Northumberland (Boundary Change) Order 2013", 
           "The East Hertfordshire and Stevenage (Boundary Change) Order 2013", 
           "The Merthyr Tydfil and Powys (Areas) Order 2009")

lad_code_changes <- lad_code_changes %>%
  filter(!(changed_from_code == "E08000020" & changed_to_code == "E06000057"), # Gateshead to Northumberland
         !(changed_from_code == "E07000097" & changed_to_code == "E07000243"), # East Hertfordshire to Stevenage
         !(changed_from_code == "W06000007" & changed_to_code == "W06000024")) %>% # Powys to Merthyr Tydfil
  mutate(split = ifelse(desc %in% descs, FALSE, split),
         merge = ifelse(desc %in% descs, FALSE, merge))

# check if any other splits have been added since the ones dealt with above
split_rows <- filter(lad_code_changes, split == TRUE)

if(nrow(split_rows != 0)) { 
  print(split_rows)
  warning("There have been new splits added to the code change database. Look at the boundary change legislation to see if these are minor changes which can be converted to simple code changes as done for previous splits") 
}

# TODO check that dates have all been read across OK

all_lad_codes_dates <- read.csv("data-raw/ChangeHistory.csv", stringsAsFactors = FALSE) %>%
  filter(ENTITYCD %in% geogs_of_interest) %>%
  mutate(start_date = as.Date(OPER_DATE, "%d/%m/%Y"),
         end_date = as.Date(TERM_DATE, "%d/%m/%Y")) %>%
  select(gss_code = GEOGCD,
         gss_name = GEOGNM,
         desc = SI_TITLE,
         entity_type = ENTITYCD,
         start_date,
         end_date,
         status = STATUS)

# TODO check for any duplicate gss_codes with overlapping dates. 

saveRDS(all_lad_codes_dates, "data-raw/all_lad_codes_dates.rds")
saveRDS(lad_code_changes, "data-raw/lad_code_changes.rds")
saveRDS(database_date, "data-raw/database_date.rds")


rm(code_changes, all_lad_codes_dates, lad_code_changes, split_rows, descs, database_date, geogs_of_interest)


