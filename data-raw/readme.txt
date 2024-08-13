ABOUT
This folder contains scripts and instructions to create the various gss code lookups needed in the package and for testing.
This should be done every time a new code history database has been published by ONS (typically May and December)

Workflow of saving rds files from data and then saving all to sysdata.rda as a final step is following best practice suggested by jennybc here:
https://github.com/r-lib/usethis/issues/1091


INSTRUCTIONS
When updating the code history data, copy 'ChangeHistory.csv' and 'Changes.csv' into this folder from the most recent ONS Code History Database download.

Run all R files in numerical order to create the internal package data that contains the code lookups and datasets for the testthat testing

Update the testthat tests to test on the latest code changes