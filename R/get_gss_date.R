#' Get the date range that a set of GSS codes were operational
#'
#' Returns the date range during which all of the gss codes in a dataframe were
#' operational
#'
#' Note that 2018 codes will return as 2013 as they are the same as 2013 codes,
#' only some of the gss names have changed and this doesn't check names. 
#'
#' @param df_in A data frame containing gss_codes.
#' @param col_code A string. The name of the column which contains gss codes (defaults to
#'   \code{gss_code}).
#'
#' @return A named list with elements called  \code{earliest} and  \code{latest} which gives the 
#' earliest and latest dates that the gss codeset was operational. 
#' 
#' @import dplyr
#' @importFrom assertthat assert_that

#' 
#' @export

get_gss_date <- function(df_in, 
                         col_code = "gss_code") {
  
  .validate_get_gss_date(df_in, col_code)
  
  
  df <- df_in %>%
    rename("gss_code" = !!col_code)
  
  # get rid of any duplicates in the gss codes.  So far these have all been due to name changes where the code doesn't change
  # Condense to one entry which gives the start and end date of the code no matter what the name was.
  code_dates <- all_lad_codes_dates %>% # all_lad_codes_dates is an internal package variable stored in R/sysdata.rda
    select(gss_code, entity_type, start_date, end_date, status) %>%
    group_by(gss_code) %>%
    mutate(orig_start_date = start_date,
           end_date = max(end_date),
           start_date = min(start_date)) %>%
    filter(max(orig_start_date) == orig_start_date) %>%
    select(-orig_start_date) %>%
    ungroup()
  
  df_codes <- unique(df$gss_code)
  
  code_dates_codes <- filter(code_dates, gss_code %in% df_codes)
  
  earliest_possible <- max(code_dates_codes$start_date, na.rm = TRUE)
  
  if (all(is.na(code_dates_codes$end_date))) {
    latest_possible <- as.Date(NA)
  } else {
    latest_possible <- min(code_dates_codes$end_date, na.rm = TRUE) - 1
  }
  
  if(!is.na(latest_possible) & earliest_possible > latest_possible) stop("the codes in the dataframe were not all operational at the same time")
  
  too_new <- code_dates_codes %>% filter(start_date > latest_possible)
  too_old <- code_dates_codes %>% filter(end_date < earliest_possible)
  if(nrow(too_new) !=0 | nrow(too_old) !=0) stop("the codes in the dataframe were not all operational at the same time")
  
  
  return(list(earliest = earliest_possible, latest = latest_possible))
  
}

.validate_get_gss_date <- function(df_in, col_code) {
  
  # validate input variable data types
  assertthat::assert_that(is.data.frame(df_in),
                          msg = "in get_gss_date, df_in must be a dataframe")
  
  assertthat::assert_that(is.character(col_code),
                          msg = "in get_gss_date, col_code must of type character")
  
  # other validations
  assertthat::assert_that(col_code %in% names(df_in),
                          msg = paste("in get_gss_date, specified col_code", col_code,
                                      "not in input dataframe"))
  
  assertthat::assert_that(all(df_in[[col_code]] %in% all_lad_codes_dates$gss_code),
                          msg = paste0("in get_gss_date the specified col_code,", col_code, ", contains unrecognised gss codes. Either the codes are incorrect or the gsscoder package code change database is not up to date"))
  
  invisible()
}


