library(dplyr)
library(tidyr)

load("R/sysdata.rda")

df_in_orig <- test_codes_names$y2023 %>% filter(entity %in% c("W06", "E06", "E07", "E08", "E09")) %>% select(-entity) %>%
  group_by(gss_code, gss_name) %>%
  expand(sex = c("female", "male")) %>%
  as.data.frame() %>% 
  mutate(count = sample.int(100, n(), replace = TRUE))

head(df_in_orig)


df_in <- df_in_orig
col_code = "gss_code"
col_name = "gss_name"
