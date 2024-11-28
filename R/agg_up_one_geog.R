#' Aggregate numeric data from one geogrpahy level to the next geography level up
#' 
#' @param df_in A data frame containing gss_codes and data.
#' @param col_code A string. The name of the column which contains gss codes (defaults to
#'   \code{gss_code}).
#' @param col_name A string. The name of a column containing gss names. If there
#' is no name column then this should be set to NA. Defaults to NA 
#' @param col_data A string or character vector. The column(s) that contain the
#' data to be aggregated. Defaults to 'value'
#' @param fun Character. Function to be applied in aggregating data. Either 'sum'
#'   or 'mean'. Default \code{'sum'}.
#' @param gss_date A date object specifying the point in time that the gss codes 
#' were/are operational. Only one of gss_date or gss_year can be defined. Defaults 
#' to \code{NA})
#' @param gss_year  Numeric or Integer. The year at which the gss codes were/are 
#' operational. Equivalent to setting gss_date to 31st December of that year. 
#' Only one of gss_date or gss_year can be defined. Defaults to \code{NA}) 
#' 
#'
#' @return a dataframe with gss codes and values for one level of geography up
#' 
#' @import dplyr
#' @importFrom lubridate is.Date
#' @importFrom assertthat assert_that
#' @import data.table
#' @importFrom dtplyr lazy_dt
#' 
#' @export

# TODO add somewhere that this can't have any other columns than gss_code and gss_name that are on the geography level. 

agg_up_one_geog <- function(df_in, 
                            col_code = "gss_code", 
                            col_name = NA,
                            col_data = "value",
                            fun = "sum",
                            gss_date = NA,
                            gss_year = NA) {
  
  .validate_agg_up_one_geog(df_in, col_code, col_name, col_data, fun, gss_date, gss_year)
  
  # the function preserves the order of the columns
  col_order <- names(df_in)
  
  df_in <- df_in %>%
    rename("gss_code" = !!col_code)
  
  if (!is.na(gss_year)) {
    gss_date = as.Date(paste0(gss_year,"-12-31"))
  }
  
  # If there is a name column it is removed and the new names added at the end
  if (!is.na(col_name)) {
    df_in <- df_in %>% select(-!!col_name)
  }
  
  check_gss_codes(df_in, col_code = "gss_code", gss_date = gss_date, expect_complete = FALSE, geogs = NA)
  
  ####### Get the parent codes ########
  
  parent_codes <- filter(all_codes_dates, gss_code %in% df_in$gss_code) %>%
    filter(start_date <= gss_date & (end_date >= gss_date | is.na(end_date))) %>% # only get rows which were live at the given date
    select(gss_code, parent_cd) %>%
    unique()
  
  # If Wales is in the data as a region, then add Wales as the parent code
  # Assume Wales is a region if there are also E12 codes in the data.
  if (any(grepl("^W92", parent_codes$gss_code)) & 
      any(grepl("^E12", parent_codes$gss_code)) &
      !any(grepl("^W06", parent_codes$gss_code))){
    
    parent_codes <- parent_codes %>%
      mutate(parent_cd = ifelse(gss_code == "W92000004", "W92000004", parent_cd))
    
  }
  
  df <- df_in %>% left_join(parent_codes, by = "gss_code")
  
  # check that there are no missing parent codes
  invalid_parent_codes <- filter(df, !grepl("^[EW]\\d{8}$", parent_cd))
  
  if(nrow(invalid_parent_codes) > 0) {
    print(invalid_parent_codes)
    stop("In agg_up_one_geog, there are gss codes in df_in which do not have valid parent codes (see above).")
  }
  
  
  # Some LAD codes have E10 codes (counties) as parents but the rest have regions.
  # For any codes with E10 as a parent, these will need to be parented again to get from county to region to match the rest of the LADs. 
  df_default <- df %>% filter(!grepl("^E10", parent_cd))
  df_e10 <- df %>% filter(grepl("^E10", parent_cd))
  
  if (nrow(df_e10) != 0) {
    df_e10 <- df_e10 %>%
      select(-gss_code) %>%
      rename(gss_code = parent_cd)
    
    check_gss_codes(df_e10, col_code = "gss_code", gss_date = gss_date, expect_complete = FALSE, geogs = NA)
    
    parent_codes <- filter(all_codes_dates, gss_code %in% df_e10$gss_code) %>%
      filter(start_date <= gss_date & (end_date >= gss_date | is.na(end_date))) %>%
      select(gss_code, parent_cd) %>%
      unique()
    
    df_e10 <- df_e10 %>% left_join(parent_codes, by = "gss_code") %>%
      select(names(df_default))
    
  }
  
  df <- bind_rows(df_default, df_e10) %>%
    select(-gss_code) %>%
    rename(gss_code = parent_cd)
  
  ####### Do the aggregation ########
  
  col_aggregation <- setdiff(names(df), col_data)
  
  if(fun == "sum"){
    df <- df %>%
      lazy_dt() %>%
      group_by(across(!!col_aggregation)) %>%
      summarise_all(.funs = sum) %>%
      as.data.frame()
  }
  
  if(fun == "mean"){
    df <- df %>%
      lazy_dt() %>%
      group_by(across(!!col_aggregation)) %>%
      summarise_all(.funs = mean) %>%
      as.data.frame()
  }
  
  
  ####### Make sure df matches df_in ########
  
  if (!is.na(col_name)) {
    df <- df %>% add_gss_names(col_code = col_code, col_name = col_name, gss_date = gss_date)
  }
  
  
  df <- as.data.frame(df) %>%
    rename(!!col_code := "gss_code") %>%
    select(all_of(col_order))
  
  return(df)
  
  
}

.validate_agg_up_one_geog <-function(df_in, col_code, col_name, col_data, fun, gss_date, gss_year) {
  
  # validate input variable data types
  assertthat::assert_that(is.data.frame(df_in),
                          msg = "in agg_up_one_geog, df_in must be a dataframe")
  
  assertthat::assert_that(is.character(col_code),
                          msg = "in agg_up_one_geog, col_code must be of type character")
  
  assertthat::assert_that(is.na(col_name) | is.character(col_name),
                          msg = "in agg_up_one_geog, col_name must be of type character")
  
  assertthat::assert_that(is.character(col_data),
                          msg = "in agg_up_one_geog, col_data must be of type character")
  
  assertthat::assert_that(is.na(gss_date) | is.Date(gss_date),
                          msg = "in agg_up_one_geog gss_date must be a date object")
  
  assertthat::assert_that(is.na(gss_year) | is.numeric(gss_year) | is.integer(gss_year),
                          msg = "in agg_up_one_geog gss_year must be integer or numeric")
  
  # other validations
  assertthat::assert_that(col_code %in% names(df_in),
                          msg = paste0("in agg_up_one_geog, specified col_code `", col_code,
                                       "` not in input dataframe"))
  
  assertthat::assert_that(is.na(col_name) | col_name %in% names(df_in),
                          msg = paste0("in agg_up_one_geog, specified col_name `", col_name,
                                       "` not in input dataframe"))
  
  for(i in length(col_data)){
    
    assertthat::assert_that(col_data[i] %in% names(df_in),
                            msg = paste0("in agg_up_one_geog, specified col_data'", col_data[i],
                                         "' not in input dataframe"))
  }
  
  assertthat::assert_that(fun %in% c("sum","mean"),
                          msg = "in agg_up_one_geog, fun must be sum or mean")
  
  assertthat::assert_that(is.na(gss_date) | is.na(gss_year),
                          msg = "in agg_up_one_geog only one of gss_date and gss_year can be specified")
  
  assertthat::assert_that(!(is.na(gss_date) & is.na(gss_year)),
                          msg = "in agg_up_one_geog one of gss_date or gss_year must be specified")
  
  if (is.na(col_name)) {
    # check that none of the columns contain geography names
    la_names <- all_codes_dates %>% # all_codes_dates is an internal package data variable stored in R/sysdata.rda
      select(gss_name) %>% unique() %>% pull()
    
    poss_name_cols <- df_in %>% 
      select(where(is.factor)|where(is.character)) %>%
      mutate(across(everything(), as.character)) %>%
      select(where(~any(.x %in% la_names))) %>%
      names()
    
    # TODO make a more intelligent geographic area details check if there is a unique value of any column for each GSS code
    if (length(poss_name_cols > 0)) {warning(paste("in agg_up_one_geog LA names have been detected in the input dataframe. If this is an LA name column please remove it before passing to agg_up_one_geog:", paste(poss_name_cols, collapse = ", ")))}
  }
  
  invisible()
}
