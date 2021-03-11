# ENSURE ALL PACKAGES ARE AVAILABLE
required_packages <- 
  c(
    "magrittr",
    "dplyr",
    "odbc",
    "janitor",
    "lubridate",
    "data.table",
    "openxlsx",
    "haven",
    "readr"
  )

# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
installed_packages <- installed.packages()[,"Package"]
new_packages <- required_packages[!(required_packages %in% installed_packages)]