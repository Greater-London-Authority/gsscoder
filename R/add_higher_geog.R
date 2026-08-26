#' Add a column with GSS codes of a higher geography
#'
#' Add a new column to a dataframe containing GSS codes where the new column contains 
#' corresponding GSS codes at a higher geography.
#' 
#' Currently covers LADs, regions, and countries.
#'  
#' The name of the new column will be its geography level e.g. 'region'
#'  
#' The region code for Wales is coded as the country code ("W92000004") 
#' 
#' Throws an error if there are any GSS codes which were not operational at the 
#' given date/year.
#' 
#' @param df_in A data frame containing GSS codes.
#' @param col_code A string. The name of the column which contains gss codes (defaults to
#'   \code{gss_code}).
#' @param current_geog A string specifying the geography level of the \code{col_code} column. 
#' Must be either \code{'lad'} or \code{'region'}.
#' @param higher_geog A string specifying the higher geography level to be added.
#' Must be either \code{'region'} or \code{'country'}.
#' @param gss_date A date object specifying the point in time that the gss codes 
#' were/are operational. One of and only one of gss_date or gss_year must be defined. Defaults 
#' to \code{NA})
#' @param gss_year  Numeric or Integer. The year at which the gss codes were/are 
#' operational. Equivalent to setting gss_date to 31st December of that year. 
#' One of and only one of gss_date or gss_year must be defined. Defaults to \code{NA}) 
#' 
#' @return The input dataframe with a column added which contains the higher level GSS codes. 
#' 
#' @import dplyr
#' @importFrom lubridate is.Date
#' @importFrom assertthat assert_that
#' 
#' @export

add_higher_geog <- function(df_in, 
                            col_code = "gss_code",
                            current_geog = c("lad", "region"),
                            higher_geog = c("region", "country"),
                            gss_date = NA,
                            gss_year = NA) {
  
  .validate_add_higher_geog(df_in, col_code, current_geog, higher_geog, gss_date, gss_year)
  
  col_order <- names(df_in)
  
  df <- df_in
  
  if (!is.na(gss_year)) {
    gss_date = as.Date(paste0(gss_year,"-12-31"))
  }
  
  check_gss_codes(df, col_code = col_code, gss_date = gss_date, expect_complete = FALSE, geogs = current_geog)
  
  lookup <- .sys_lad_region_country %>%
    select(all_of(c(current_geog, higher_geog)), "start_date", "end_date") %>%
    filter(start_date <= gss_date & (end_date >= gss_date | is.na(end_date))) %>%
    select(-start_date, -end_date) %>%
    unique()
  
  df_out <- left_join(df, lookup, by = setNames(current_geog, col_code)) %>%
    as.data.frame() 
  
  # check that df_out has same number of rows as df_in and one more column
  assertthat::assert_that(nrow(df_in) == nrow(df_out),
                          msg = "add_higher_geog output has gone wrong (number of rows)")
  assertthat::assert_that(all.equal(names(df_out), c(names(df_in), higher_geog)),
                          msg = "add_higher_geog output has gone wrong (column names)")
  
  return(df_out)
  
}

.validate_add_higher_geog <-function(df_in, col_code, current_geog, higher_geog, gss_date, gss_year) {
  
  
  # validate input variable data types
  assertthat::assert_that(is.data.frame(df_in),
                          msg = "add_higher_geog, df_in must be a dataframe")
  
  assertthat::assert_that(is.character(col_code),
                          msg = "in add_higher_geog, col_code must be of type character")
  
  assertthat::assert_that(is.character(current_geog),
                          msg = "in add_higher_geog, current_geog must be of type character")
  
  assertthat::assert_that(is.character(higher_geog),
                          msg = "in add_higher_geog, higher_geog must be of type character")
  
  assertthat::assert_that(is.na(gss_date) | is.Date(gss_date),
                          msg = "in add_higher_geog gss_date must be a date object")
  
  assertthat::assert_that(is.na(gss_year) | is.numeric(gss_year) | is.integer(gss_year),
                          msg = "in add_higher_geog gss_year must be integer or numeric")
  
  # other validations
  assertthat::assert_that(col_code %in% names(df_in),
                          msg = paste0("in add_higher_geog, specified col_code `", col_code,
                                       "` not in input dataframe"))
  
  assertthat::assert_that(current_geog %in% c("lad", "region"),
                          msg = paste0("in add_higher_geog, specified current_geog `", current_geog,
                                       "` is not permitted. current_geog must be one of 'lad' or 'region'."))
  
  assertthat::assert_that(higher_geog %in% c("region", "country"),
                          msg = paste0("in add_higher_geog, specified higher_geog `", higher_geog,
                                       "` is not permitted. higher_geog must be one of 'region' or 'country'."))
  
  assertthat::assert_that(is.na(gss_date) | is.na(gss_year),
                          msg = "in add_higher_geog only one of gss_date and gss_year can be specified")
  
  assertthat::assert_that(!(is.na(gss_date) & is.na(gss_year)),
                          msg = "in add_higher_geog one of gss_date or gss_year must be specified")
  
  
  # check that current_gss_level is smaller than target_gss_level
  geog_order = names(.sys_lad_region_country)
  assertthat::assert_that(which(geog_order == current_geog) < which(geog_order == higher_geog),
                          msg = paste0("in add_higher_geog: current_geog must be a smaller geography than higher_geog. current_geog was given as '", current_geog, "' and higher_geog was given as '", higher_geog, "'"))
  rm(geog_order)
  
  invisible()
}

