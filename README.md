# gsscoder

This package provides functions for wrangling ONS LA GSS codes, including
switching from one vintage to another. It currently only works on LA level
codes for England and Wales. 

## Installation

``` r
# To install from github use the devtools function:
# This will install all required dependencies
devtools::install_github("Greater-London-Authority/gsscoder")
```

## Usage

``` r
library(gsscoder)

# create sample data with GSS codes (2019 vintage) and random values
mydata <- gsscoder:::test_codes$y2019 # accesses internal package data (a dataframe with column `gss_code` which contains 2019 GSS codes)
set.seed(8)
mydata$value <- sample(1:100, nrow(mydata), replace = TRUE)

# use gsscoder functions
recode_gss(mydata, col_geog = "gss_code", data_cols = "value, recode_from_year = 2019, recode_to_year = 2023)
```

## Updating package

This package contains lookups created from the ONS code change database. The 
internal package data needs to be updated whenever a new code change database
is released by ONS. To do this follow the instructions in `data-raw/readme.txt`
