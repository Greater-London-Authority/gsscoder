#' Re-code LA GSS Codes to a different year
#'
#' Switches any gss codes to an earlier or later vintage
#'
#' Aggregates data where rows with the same gss code are present
#' 
#' lists the gss codes separated by ", " where the change is from one gss code to many.
#'
#' User must define the column containing the gss_code and the columns which 
#' contain data. Data columns must be numeric or integer and at least one data 
#' column must be present.
#'
#' @param df_in A data frame containing gss_codes and data.
#' @param col_code A string. The column which contains gss codes (defaults to
#'   \code{gss_code}).
#' @param col_dataA string or character vector. The column(s) that contain the
#'   data to be aggregated. Defaults to last column of input dataframe. At least 
#'   one data column must be specified. 
#' @param fun Character Function to be applied in aggregating data. Either 'sum'
#'   or 'mean'. Default \code{'sum'}.
#' @param recode_from_year Numeric. Year of geography of the input dataframe. 
#' @param recode_to_year Numeric. Conform to geography in which year. 
#' @param aggregate_data Logical. If set to true multiple instances of the same
#'   gss code will be aggregated using the function specified in \code{fun} parameter.
#'   Default to \code{TRUE}.
#'
#' @return The input dataframe with gss codes changed and data aggregated
#' 
#'
#' @import dplyr
#' @import data.table
#' @importFrom dtplyr lazy_dt
#' @importFrom assertthat assert_that
#' 
#' @export

recode_gss <- function(df_in,
                       col_code="gss_code",
                       col_data= "value",
                       fun = "sum",
                       recode_from_year,
                       recode_to_year,
                       aggregate_data = TRUE) {
  
  # assign code changes to new variable as it will be altered below. 
  code_changes <- lad_code_changes # lad_code_changes is an internal package data variable stored in R/sysdata.rda
  
  # names are for checking that none of the columns contain LA names
  la_names <- all_lad_codes_dates %>% # all_lad_codes_dates is an internal package data variable stored in R/sysdata.rda
    select(gss_name) %>% unique() %>% pull()
  
  
  
  #validate
  .validate_recode_gss(df_in, col_code, col_data, recode_to_year, recode_from_year, fun, la_names)
  
  
  col_order <- names(df_in)
  
  #prepare input dataframe
  df <- df_in %>%
    as.data.frame() %>%
    rename("gss_code" = !!col_code)
  
  if (recode_to_year < recode_from_year) {
    # reverse the code changes dataframe if the code year is being rolled backwards rather than forwards
    #   * changed_to becomes changed_from and vice versa
    #   * splits become merges and vice versa
    #   * year becomes year-1 as year-1 is the first year that changes when going backwards
    code_changes <- code_changes %>%
      mutate(split2 = ifelse(merge == TRUE, TRUE, FALSE),
             merge2 = ifelse(split == TRUE, TRUE, FALSE)) %>%
      select(-split, -merge) %>%
      rename(split = split2, merge = merge2,
             changed_to_code = changed_from_code, changed_from_code = changed_to_code,
             changed_to_name = changed_from_name, changed_from_name = changed_to_name) %>%
      mutate(year = year - 1)
  }
  
  
  lookup <- data.frame(changed_from_code = character(), changed_to_code = character(), stringsAsFactors = FALSE)
  
  
  for (my_year in recode_from_year:recode_to_year) {
    
    # append any new rows with codes in the data
    new_rows <- filter(code_changes, changed_from_code %in% df$gss_code, year == my_year) %>%
      select(changed_to_code, changed_from_code)
    
    if(nrow(new_rows) != 0){
      lookup <- bind_rows(lookup, new_rows)
    }
    
    # update any rows which are already in the lookup
    update_rows <- filter(code_changes, changed_from_code %in% lookup$changed_to_code, year == my_year) %>%
      select(changed_from_code, changed_to_code)
    
    if (nrow(update_rows) != 0 ) {
      lookup <- lookup %>% left_join(update_rows, by = c("changed_to_code" = "changed_from_code")) %>%
        mutate(changed_to_code = ifelse(!is.na(changed_to_code.y), changed_to_code.y, changed_to_code)) %>%
        select(-changed_to_code.y)
    }
    
  }
  
  lookup <- unique(lookup) %>%
    filter(!changed_from_code == changed_to_code) # remove entries where the code didn't change, e.g. the name changed only
  
  splits_present = any(duplicated(lookup$changed_from_code))
  
  
  # TODO? Add on the 'largest area code' and 'largest area year'?
  if (splits_present) {
    lookup <- lookup %>%
      group_by(changed_from_code) %>%
      mutate(changed_to_code = paste0(changed_to_code, collapse = ", ")) %>%
      # TODO? Add on the 'largest area code' and 'largest area year'? rows below will do that
      #n = n(),
      #largest_area_code = ifelse(n == 1, changed_to_code, changed_from_code),
      #largest_area_year = ifelse(n == 1, recode_to_year, recode_from_year)) %>%
      #select(-n) %>%
      ungroup() %>%
      unique()
    
  }
  
  df <- df %>%
    left_join(lookup, by = c("gss_code" = "changed_from_code")) %>%
    mutate(changed_to_code = ifelse(is.na(changed_to_code), gss_code, changed_to_code)) %>%
    select(-gss_code) %>%
    rename(gss_code = changed_to_code)
  
  # TODO how would this function work if there had been a splits in one area and merges in the other?
  #how does it choose which is the largest area? Actaully a merge just goes to 1 code in the changed to so thats fine. 
  #not sure about how it would work if 1 area has had merges and splits that follow on from each other?
  
  col_aggregation <- setdiff(names(df),col_data)
  
  
  if(aggregate_data){
    
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
    
  }
  
  df <- as.data.frame(df) %>%
    rename(!!col_code := "gss_code") %>%
    select(all_of(col_order)) # output df should have same column order as input df
  
  # TODO: how does this function change the order of the rows in the df? Is that the
  # order we want them to come out in?  Add the output order info to the function 
  # description. 
  
  # TODO: return a tibble or dataframe depending on what was passed.  
  # update test-recode_gss if this change is made. 
  return(df)
  
}


.validate_recode_gss <- function(df_in, col_code, col_data, recode_to_year, recode_from_year, fun, la_names) {
  
  # TODO lots of these assertions are replicated across the different functions.  Can they be pulled out somewhere else and called from here?
  
  # validate input variable data types
  assertthat::assert_that(is.data.frame(df_in),
                          msg = "in recode_gss, df_in must be a dataframe")
  
  assertthat::assert_that(is.character(col_code),
                          msg = "in recode_gss, col_code must be of type character")
  
  assertthat::assert_that(is.character(col_data),
                          msg = "in recode_gss, col_datamust be of type character")
  
  assertthat::assert_that(is.numeric(recode_to_year) | is.integer(recode_to_year),
                          msg = "in recode_gss recode_to_year must be integer or numeric")
  
  assertthat::assert_that(is.numeric(recode_from_year) | is.integer(recode_from_year),
                          msg = "in recode_gss recode_to_year must be integer or numeric")
  
  # other validations
  assertthat::assert_that(fun %in% c("sum","mean"),
                          msg = "in recode_gss, fun must be sum or mean")
  
  assertthat::assert_that(recode_from_year >= 2008,
                          msg = "in recode_gss, recode_from_year must be 2008 or later")
  
  
  database_year <- database_date %>% format('%Y') %>% as.numeric() # database_date is an internal package data variable stored in R/sysdata.rda
  assertthat::assert_that(recode_from_year <= database_year,
                          msg = paste0("in recode_gss, recode_from_year cannot be later than the year of the code change database which is ",
                                       database_year, ". You may need to update the database."))
  
  assertthat::assert_that(recode_to_year >= 2008,
                          msg = "in recode_gss, recode_to_year must be 2008 or later")
  
  assertthat::assert_that(recode_to_year <= database_year,
                          msg = paste0("in recode_gss, recode_to_year cannot be later than the year of the code change database which is ",
                                       database_year, ". You may need to update the database."))
  
  
  for(i in length(col_data)){
    
    #TODO check that df_in is a dataframe
    assertthat::assert_that(col_data[i] %in% names(df_in),
                            msg = paste0("in recode_gss_codes, specified col_data'", col_data[i],
                                        "' not in input dataframe"))
  }
  
  assertthat::assert_that(col_code %in% names(df_in),
                          msg = paste("in recode_gss_codes, specified col_code", col_code,
                                      "not in input dataframe"))
  
  
  poss_name_cols <- df_in %>% 
    select(where(is.factor)|where(is.character)) %>%
    mutate(across(everything(), as.character)) %>%
    select(where(~any(.x %in% la_names))) %>%
    names()
  
  # TODO make a more intelligent geographic area details check if there is a unique value of any column for each GSS code
  
  if (length(poss_name_cols > 0)) {warning(paste("in recode_gss_codes LA names have been detected in the input dataframe. If this is an LA name column please remove it before passing to recode_gss_codes:", paste(poss_name_cols, collapse = ", ")))}
  
  #TODO check that data columns are numeric or integer
  
  invisible()
}

