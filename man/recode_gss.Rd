% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode_gss.R
\name{recode_gss}
\alias{recode_gss}
\title{Re-code LA GSS Codes to a different year}
\usage{
recode_gss(
  df_in,
  col_code = "gss_code",
  col_data = "value",
  fun = "sum",
  recode_from_year,
  recode_to_year,
  aggregate_data = TRUE
)
}
\arguments{
\item{df_in}{A data frame containing gss_codes and data.}

\item{col_code}{A string. The column which contains gss codes (defaults to
\code{gss_code}).}

\item{fun}{Character Function to be applied in aggregating data. Either 'sum'
or 'mean'. Default \code{'sum'}.}

\item{recode_from_year}{Numeric. Year of geography of the input dataframe.}

\item{recode_to_year}{Numeric. Conform to geography in which year.}

\item{aggregate_data}{Logical. If set to true multiple instances of the same
gss code will be aggregated using the function specified in \code{fun} parameter.
Default to \code{TRUE}.}

\item{col_dataA}{string or character vector. The column(s) that contain the
data to be aggregated. Defaults to last column of input dataframe. At least 
one data column must be specified.}
}
\value{
The input dataframe with gss codes changed and data aggregated
}
\description{
Switches any gss codes to an earlier or later vintage
}
\details{
Aggregates data where rows with the same gss code are present

lists the gss codes separated by ", " where the change is from one gss code to many.

User must define the column containing the gss_code and the columns which 
contain data. Data columns must be numeric or integer and at least one data 
column must be present.
}
