#' Check that the LA gss codes in a dataframe are consistent with the codes which 
#' were operational at a given date.
#' 
#' Checks that there are no codes in the data which weren't live on that date,
#' and can also check for completeness covering England and optionally Wales.
#'
#' Throws an error if there are codes present which weren't live on the give date,
#' and warns for any missing codes if full_coverage is set to TRUE. 
#' 
#'
#' @param df_in A data frame containing gss_codes and data.
#' @param col_geog A string. The name of the column which contains gss codes 
#' (defaults to \code{gss_code}).
#' @param gss_date A date object specifying the point in time that the gss codes 
#' in the dataframe are being checked against. Only one of gss_date 
#' or gss_year can be defined. Defaults to \code{NA})
#' @param gss_year Numeric or Integer. The year against which to check the gss codes. 
#' Equivalent to setting gss_date to 31st December of that year. Only one of gss_date 
#' or gss_year can be defined. Defaults to \code{NA}) 
#' @param full_coverage Logical. If set to TRUE a warning will be given if there are 
#' codes which are not in df_in but were operational on the date/year given. Defaults to \code{FALSE})
#' @param include_wales Logical. If set to TRUE when full_coverage is TRUE, warnings
#' will be given for missing Welsh codes as well as English ones. Defaults to \code{FALSE})
#' 
#' @return Does't return anything
#' 
#' @import dplyr
#' @importFrom lubridate is.Date
#' 
#' @export


check_gss_codes <- function(df_in, 
                            col_geog = "gss_code",
                            gss_date = NA, 
                            gss_year = NA, 
                            full_coverage = FALSE,
                            include_wales = FALSE) {
  
  .validate_check_gss_codes(df_in, col_geog, gss_date, gss_year, full_coverage, include_wales)
  
  code_dates <- all_lad_codes_dates %>%
    select(-status)
  
  if (is.na(gss_date)) {gss_date <- as.Date(paste0(gss_year, "-12-31"))}
  
  if (include_wales == FALSE) {
    code_dates <- filter(code_dates, !grepl("^W", gss_code))
  }
  
  df_in <- df_in %>%
    rename("gss_code" = !!col_geog)
  
  expected_codes <- filter(code_dates, start_date <= gss_date & (end_date >= gss_date | is.na(end_date)))
  
  unexpected_codes <- filter(df_in, !gss_code %in% expected_codes$gss_code) %>% pull(gss_code) %>% unique()
  unexpected_code_details <- filter(code_dates, gss_code %in% unexpected_codes)
  
  missing_codes <- filter(expected_codes, !gss_code %in% df_in$gss_code)
  
  if (nrow(missing_codes !=0 & full_coverage == TRUE)) { 
    # TODO? Change this to an error instead of warning.  Refactor to include both missing and non-live codes in one error message
    warning(paste0("The gss codes above are missing from the data. They were live on the date given (",gss_date,"). Either fix the data or set 'include_wales' and/or 'full_coverage' params to FALSE", 
                   message(paste(capture.output(missing_codes), collapse = "\n"))))
    
  }
  
  if (nrow(unexpected_code_details != 0)) {
    
    stop(paste0("The gss codes above are present in the data but were not live on the date given (",gss_date,").", message(paste(capture.output(unexpected_code_details), collapse = "\n"))))
    
  }
  
  invisible()
  
}

.validate_check_gss_codes <- function(df_in, col_geog, gss_date, gss_year, full_coverage, include_wales) {
  
  # validate input variable data types
  assertthat::assert_that(is.data.frame(df_in),
                          msg = "in check_gss_codes, df_in must be a dataframe")
  
  assertthat::assert_that(is.character(col_geog),
                          msg = "in check_gss_codes, col_geog must of type character")
  
  assertthat::assert_that(is.na(gss_date) | is.Date(gss_date),
                          msg = "in check_gss_codes gss_date must be a date object")
  
  assertthat::assert_that(is.na(gss_year) | is.numeric(gss_year) | is.integer(gss_year),
                          msg = "in check_gss_codes gss_year must be integer or numeric")
  
  assertthat::assert_that(full_coverage %in% c(TRUE, FALSE),
                          msg = "in check_gss_codes full_coverage must be set to TRUE or FALSE")
  
  assertthat::assert_that(include_wales %in% c(TRUE, FALSE),
                          msg = "in check_gss_codes include_wales must be set to TRUE or FALSE")
  
  
  # other validations
  assertthat::assert_that(col_geog %in% names(df_in),
                          msg = paste0("in check_gss_codes, specified col_geog `", col_geog,
                                       "` not in input dataframe"))
  
  assertthat::assert_that(is.na(gss_date) | is.na(gss_year),
                          msg = "in check_gss_codes only one of gss_date and gss_year can be specified")
  
  assertthat::assert_that(!(is.na(gss_date) & is.na(gss_year)),
                          msg = "in check_gss_codes one of gss_date or gss_year must be specified")
  
 
  database_year <- database_date %>% format('%Y') %>% as.numeric()
  assertthat::assert_that(is.na(gss_year) | (gss_year >= 2009 & gss_year <= database_year),
                          msg = paste0("in check_gss_codes gss_year must be a number between 2009 and ", database_year, ". If your required year is later than ", database_year ," then check if the gsscoder package code change database needs updating"))
  
  assertthat::assert_that(is.na(gss_date) | (gss_date >= as.Date("2009-01-01") & gss_date <= database_date),
                          msg = paste0("in check_gss_codes gss_date must be between 2009-01-01 and ", database_date, ". If your required date is later than ", database_date ," then check if the gsscoder package code change database needs updating"))
  
 
  
  invisible()
}


