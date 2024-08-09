library(dplyr)
library(tidyr)

codes <- test_codes # internal package variable stored in R/sysdata.rda and created in data-raw/testthat_data.R


expect_2008 <- expand.grid(gss_code = codes$y2008$gss_code, 
                           sex = c("male", "female"), 
                           age = c(0,1), 
                           stringsAsFactors = FALSE,
                           KEEP.OUT.ATTRS = FALSE)
set.seed(6)
expect_2008$value <- sample(1:100, nrow(expect_2008), replace = TRUE)
expect_2008$value2 <- sample(1:100, nrow(expect_2008), replace = TRUE)

aggregate_data <- function(df) {
  df <- df %>% 
    group_by(gss_code, sex, age) %>%
    summarise(value = sum(value),
              value2 = sum(value2), .groups = "drop") %>%
    as.data.frame() %>%
    arrange(gss_code)
  return(df)
}

expect_2009 <- expect_2008 %>% mutate(
  gss_code = case_when(gss_code == "E07000025"  ~ "E06000053",
                       gss_code == "E07000002" ~ "E06000055",
                       gss_code == "W06000007" ~ "W06000023",
                       gss_code == "W06000017" ~ "W06000024",
                       gss_code %in% c("E07000054", "E07000055", "E07000056", "E07000057", "E07000058", "E07000059", "E07000060") ~ "E06000047",
                       gss_code %in% c("E07000157", "E07000158", "E07000159", "E07000160", "E07000161", "E07000162") ~ "E06000048",
                       gss_code %in% c("E07000017", "E07000014", "E07000015") ~ "E06000049",  
                       gss_code %in% c("E07000013", "E07000016", "E07000018") ~ "E06000050",
                       gss_code %in% c("E07000182", "E07000183", "E07000184", "E07000185", "E07000186") ~ "E06000051",
                       gss_code %in% c("E07000019", "E07000020", "E07000021", "E07000022", "E07000023", "E07000024") ~ "E06000052",
                       gss_code %in% c("E07000230", "E07000231", "E07000232", "E07000233") ~ "E06000054",
                       gss_code %in% c("E07000001", "E07000003") ~ "E06000056",
                       TRUE ~ gss_code)) %>% 
  aggregate_data()

expect_2012 <- expect_2009 %>%
  mutate(gss_code = case_when(gss_code == "E07000100" ~ "E07000240",
                              gss_code == "E07000104" ~ "E07000241",
                              TRUE ~ gss_code)) %>% 
  aggregate_data()

expect_2013 <- expect_2012 %>%
  mutate(gss_code = case_when(gss_code == "E06000048" ~ "E06000057",
                              gss_code == "E07000097" ~ "E07000242",
                              gss_code == "E07000101" ~ "E07000243",
                              gss_code == "E08000020" ~ "E08000037",
                              gss_code == "W06000014" ~ "W06000014",
                              TRUE ~ gss_code)) %>%
  aggregate_data()

expect_2018 <- expect_2013 # 2018 change was only a name change, code stayed the same

expect_2019 <- expect_2018 %>% mutate(
  gss_code = case_when(gss_code %in% c("E07000048", "E06000028", "E06000029") ~ "E06000058",
                       gss_code %in% c("E07000049", "E07000050", "E07000053", "E07000051", "E07000052") ~ "E06000059",
                       gss_code %in% c("E07000205", "E07000206") ~ "E07000244",  
                       gss_code %in% c("E07000201", "E07000204") ~ "E07000245",
                       gss_code %in% c("E07000190", "E07000191") ~ "E07000246",
                       TRUE ~ gss_code)) %>%
  aggregate_data()

expect_2020 <- expect_2019 %>% mutate(
  gss_code = case_when(gss_code %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
                       TRUE ~ gss_code)) %>%
  aggregate_data()

expect_2021 <- expect_2020 %>% mutate(
  gss_code = case_when(gss_code %in% c("E07000150", "E07000152", "E07000153", "E07000156") ~ "E06000061",
                       gss_code %in% c("E07000151", "E07000154", "E07000155") ~ "E06000062",
                       TRUE ~ gss_code)) %>%
  aggregate_data()

expect_2023 <- expect_2021 %>% mutate(
  gss_code = case_when(gss_code %in% c("E07000026", "E07000028", "E07000029") ~ "E06000063",
                       gss_code %in% c("E07000027", "E07000030", "E07000031") ~ "E06000064",
                       gss_code %in% c("E07000163", "E07000164", "E07000165", "E07000166", "E07000167", "E07000168", "E07000169") ~ "E06000065",
                       gss_code %in% c("E07000187", "E07000188", "E07000189", "E07000246") ~ "E06000066",
                       TRUE ~ gss_code)) %>%
  aggregate_data()




output_2009 <- recode_gss(expect_2008,  data_cols = c("value", "value2"), recode_from_year = 2008, recode_to_year = 2009) %>% arrange(gss_code)
output_2012 <- recode_gss(expect_2008,  data_cols = c("value", "value2"), recode_from_year = 2008, recode_to_year = 2012) %>% arrange(gss_code)
output_2013 <- recode_gss(expect_2008,  data_cols = c("value", "value2"), recode_from_year = 2008, recode_to_year = 2013) %>% arrange(gss_code)
output_2018 <- recode_gss(expect_2008,  data_cols = c("value", "value2"), recode_from_year = 2008, recode_to_year = 2018) %>% arrange(gss_code)
output_2019 <- recode_gss(expect_2008,  data_cols = c("value", "value2"), recode_from_year = 2008, recode_to_year = 2019) %>% arrange(gss_code)
output_2020 <- recode_gss(expect_2008,  data_cols = c("value", "value2"), recode_from_year = 2008, recode_to_year = 2020) %>% arrange(gss_code)
output_2021 <- recode_gss(expect_2008,  data_cols = c("value", "value2"), recode_from_year = 2008, recode_to_year = 2021) %>% arrange(gss_code)
output_2023 <- recode_gss(expect_2008,  data_cols = c("value", "value2"), recode_from_year = 2008, recode_to_year = 2023) %>% arrange(gss_code)


test_that("recode_gss_codes produces the expected outputs when rolling forwards", {
  expect_equal(output_2009, expect_2009)
  expect_equal(output_2012, expect_2012)
  expect_equal(output_2013, expect_2013)
  expect_equal(output_2018, expect_2018)
  expect_equal(output_2019, expect_2019)
  expect_equal(output_2020, expect_2020)
  expect_equal(output_2021, expect_2021)
  expect_equal(output_2023, expect_2023)
})


expectbw_2023 <- expand.grid(gss_code = codes$y2023$gss_code,
                             sex = c("male", "female"),
                             age = c(0,1),
                             stringsAsFactors = FALSE,
                             KEEP.OUT.ATTRS = FALSE)
set.seed(6)
expectbw_2023$value <- sample(1:100, nrow(expectbw_2023), replace = TRUE)
expectbw_2023$value2 <- sample(1:100, nrow(expectbw_2023), replace = TRUE)



expectbw_2021 <- expectbw_2023 %>% mutate(
  gss_code = case_when(gss_code == "E06000063" ~ "E07000026, E07000028, E07000029",
                       gss_code == "E06000064" ~ "E07000027, E07000030, E07000031",
                       gss_code == "E06000065" ~ "E07000163, E07000164, E07000165, E07000166, E07000167, E07000168, E07000169",
                       gss_code == "E06000066" ~ "E07000187, E07000188, E07000189, E07000246",
                       TRUE ~ gss_code)) %>%
  arrange(gss_code, sex, age) %>% as.data.frame()


expectbw_2020 <- expectbw_2021 %>% mutate(
  gss_code = case_when(gss_code == "E06000061" ~ "E07000150, E07000152, E07000153, E07000156",
                       gss_code == "E06000062" ~ "E07000151, E07000154, E07000155",
                       TRUE ~ gss_code)) %>%
  arrange(gss_code, sex, age)


expectbw_2019 <- expectbw_2020 %>% mutate(
  gss_code = case_when(gss_code == "E06000060" ~ "E07000004, E07000005, E07000006, E07000007",
                       TRUE ~ gss_code)) %>%
  arrange(gss_code, sex, age)

expectbw_2018 <- expectbw_2019 %>% mutate(
  gss_code = case_when(gss_code == "E06000058" ~ "E07000048, E06000028, E06000029",
                       gss_code == "E06000059" ~ "E07000049, E07000050, E07000053, E07000051, E07000052",
                       gss_code == "E07000244" ~ "E07000205, E07000206",
                       gss_code == "E07000245" ~ "E07000201, E07000204",
                       gss_code == "E07000187, E07000188, E07000189, E07000246" ~ "E07000187, E07000188, E07000189, E07000190, E07000191",
                       TRUE ~ gss_code)) %>%
  arrange(gss_code, sex, age)

expectbw_2013 <- expectbw_2018 # 2018 change was only a name change, code stayed the same



expectbw_2012 <- expectbw_2013 %>%
  mutate(gss_code = case_when(gss_code == "E06000057" ~ "E06000048",
                              gss_code == "E07000242" ~ "E07000097",
                              gss_code == "E07000243" ~ "E07000101",
                              gss_code == "E08000037" ~ "E08000020",
                              gss_code == "W06000014" ~ "W06000014",
                              TRUE ~ gss_code)) %>%
  arrange(gss_code, sex, age)



expectbw_2009 <- expectbw_2012 %>%
  mutate(gss_code = case_when(gss_code == "E07000240" ~ "E07000100",
                              gss_code == "E07000241" ~ "E07000104",
                              TRUE ~ gss_code)) %>%
  arrange(gss_code, sex, age)

expectbw_2008 <- expectbw_2009 %>% mutate(
  gss_code = case_when(gss_code == "E06000053" ~ "E07000025",
                       gss_code == "E06000055" ~ "E07000002",
                       gss_code == "W06000023" ~ "W06000007",
                       gss_code == "W06000024" ~ "W06000017",
                       gss_code == "E06000047" ~ "E07000054, E07000055, E07000056, E07000057, E07000058, E07000059, E07000060",
                       gss_code == "E06000048" ~ "E07000157, E07000158, E07000159, E07000160, E07000161, E07000162",
                       gss_code == "E06000049" ~ "E07000017, E07000014, E07000015",
                       gss_code == "E06000050" ~ "E07000013, E07000016, E07000018",
                       gss_code == "E06000051" ~ "E07000182, E07000183, E07000184, E07000185, E07000186",
                       gss_code == "E06000052" ~ "E07000019, E07000020, E07000021, E07000022, E07000023, E07000024",
                       gss_code == "E06000054" ~ "E07000230, E07000231, E07000232, E07000233",
                       gss_code == "E06000056" ~ "E07000001, E07000003",
                       TRUE ~ gss_code)) %>%
  arrange(gss_code, sex, age)

outputbw_2008 <- recode_gss(expectbw_2023,  data_cols = c("value", "value2"), recode_from_year = 2023, recode_to_year = 2008) %>% arrange(gss_code, sex, age)
outputbw_2009 <- recode_gss(expectbw_2023,  data_cols = c("value", "value2"), recode_from_year = 2023, recode_to_year = 2009) %>% arrange(gss_code, sex, age)
outputbw_2012 <- recode_gss(expectbw_2023,  data_cols = c("value", "value2"), recode_from_year = 2023, recode_to_year = 2012) %>% arrange(gss_code, sex, age)
outputbw_2013 <- recode_gss(expectbw_2023,  data_cols = c("value", "value2"), recode_from_year = 2023, recode_to_year = 2013) %>% arrange(gss_code, sex, age)
outputbw_2018 <- recode_gss(expectbw_2023,  data_cols = c("value", "value2"), recode_from_year = 2023, recode_to_year = 2018) %>% arrange(gss_code, sex, age)
outputbw_2019 <- recode_gss(expectbw_2023,  data_cols = c("value", "value2"), recode_from_year = 2023, recode_to_year = 2019) %>% arrange(gss_code, sex, age)
outputbw_2020 <- recode_gss(expectbw_2023,  data_cols = c("value", "value2"), recode_from_year = 2023, recode_to_year = 2020) %>% arrange(gss_code, sex, age)
outputbw_2021 <- recode_gss(expectbw_2023,  data_cols = c("value", "value2"), recode_from_year = 2023, recode_to_year = 2021) %>% arrange(gss_code, sex, age)

test_that("recode_gss_codes produces the expected outputs when rolling backwards", {
  expect_equal(outputbw_2008, expectbw_2008)
  expect_equal(outputbw_2009, expectbw_2009)
  expect_equal(outputbw_2012, expectbw_2012)
  expect_equal(outputbw_2013, expectbw_2013)
  expect_equal(outputbw_2018, expectbw_2018)
  expect_equal(outputbw_2019, expectbw_2019)
  expect_equal(outputbw_2020, expectbw_2020)
  expect_equal(outputbw_2021, expectbw_2021)
})
