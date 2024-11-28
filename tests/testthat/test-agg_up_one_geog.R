library(tidyr)

# data for checking that the country and sex totals stay the same after aggregations
set.seed(88)

la_2023 <- test_codes_names$y2023 %>% filter(entity %in% c("W06", "E06", "E07", "E08", "E09")) %>% select(-entity) %>%
  group_by(gss_code, gss_name) %>%
  expand(sex = c("female", "male")) %>%
  as.data.frame() %>%
  mutate(count = sample.int(100, n(), replace = TRUE))
la_2023_totals <- la_2023 %>%
  mutate(country = substr(gss_code, 1,1)) %>%
  group_by(country, sex) %>%
  summarise(count = sum(count)) %>%
  as.data.frame() %>%
  arrange(country, sex)

region_2023 <- la_2023 %>% agg_up_one_geog(col_name = "gss_name", col_data = "count", gss_year = 2023) 
region_2023_totals <- region_2023 %>%
  mutate(country = substr(gss_code, 1,1)) %>%
  group_by(country, sex) %>%
  summarise(count = sum(count)) %>%
  as.data.frame() %>%
  arrange(country, sex)

country_2023 <- region_2023 %>% agg_up_one_geog(col_name = "gss_name", col_data = "count", gss_year = 2023) 
country_2023_totals <- country_2023 %>%
  mutate(country = substr(gss_code, 1,1)) %>%
  group_by(country, sex) %>%
  summarise(count = sum(count)) %>%
  as.data.frame() %>%
  arrange(country, sex)

# Select a few LAs to create a small dummy data set to check that the aggregations happen as expected when 
# compared to manually aggregated data

lads_gss <- c("E06000001", "E06000002", # region E12000001
  "E07000008", "E07000009", "E07000066", "E07000067", # counties E10000003 & E10000012, region E12000006
  "E08000001", "E08000002", # region E12000002
  "E09000001", "E09000002", # region E12000007
  "W06000001", "W06000002") # (region) W92000004

lads_region <- c(rep("E12000001", 2),
                 rep("E12000006", 4),
                 rep("E12000002", 2),
                 rep("E12000007", 2),
                 rep("W92000004", 2))

lads_counts <- c(1,2, 3,4,
                 5,6,7,8, 9,10,11,12,
                 13,14, 15,16,
                 17,18, 19,20,
                 21,22, 23,24)

lads <- data.frame(gss_code = lads_gss, region = lads_region) %>%
  add_gss_names(gss_year = 2023) %>%
  group_by(gss_code, gss_name, region) %>%
  expand(sex = c("female", "male")) %>%
  as.data.frame()
lads$count <- lads_counts

regions <- lads %>%
  group_by(region, sex) %>%
  summarise(count = sum(count)) %>%
  rename(gss_code = region) %>%
  add_gss_names(gss_year = 2023) %>%
  as.data.frame()

country <- regions %>%
  mutate(country = ifelse(grepl("^E", gss_code), "E92000001", "W92000004")) %>%
  group_by(country, sex) %>%
  summarise(count = sum(count)) %>%
  rename(gss_code = country) %>%
  as.data.frame() %>%
  add_gss_names(gss_year = 2023)

expect_lad <- regions %>% arrange(gss_code, sex)
output_lad <- lads %>%
  select(-region) %>%
  agg_up_one_geog(col_name = "gss_name", col_data = "count", gss_year = 2023) %>%
  arrange(gss_code, sex)

expect_region <- country %>% arrange(gss_code, sex)
output_region <- regions %>%
  agg_up_one_geog(col_name = "gss_name", col_data = "count", gss_year = 2023) %>%
  arrange(gss_code, sex)

test_that("agg_up_one_geog produces the expected outputs", {
  expect_equal(output_lad, expect_lad)
  expect_equal(output_region, expect_region)
  expect_equal(region_2023_totals, la_2023_totals)
  expect_equal(country_2023_totals, la_2023_totals)
  
})
