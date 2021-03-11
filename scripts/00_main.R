### Header ####
### Falls Work
### Glasgow City
### Original Author: antony.clark2@phs.scot
### Original Date: 16/02/2021
### Written to be run on RStudio Server
### Makes use of: work done by Aidan Morrison for Renfrewshire
### Packages required:
### odbc (for SMRA extraction);
### haven (for reading SPSS files);
### reshape2, dplyr, tidylog, tidyr, purrr & janitor (for data manipulation);
### magrittr (for compound assignment pipe-operator %<>%);
### lubridate (for dates);
### readxl & writexl (for reading in and writing out Excel files)
### fst (for loading in source linkage files)
###

# Useful links/references ####
# \\Freddy\Projects\SAF\Subgroups\SMRA Analysis views subgroup
# https://icd.who.int/browse10/2010/en#/W00-W19 #ICD10 falls codes
# https://www.opendata.nhs.scot/dataset/scottish-morbidity-record-completeness/resource/03cf3cb7-41cc-4984-bff6-bbccd5957679
# https://www.isdscotland.org/products-and-Services/Data-Support-and-Monitoring/SMR-Completeness/

# It should not be necessary to set the working directory but
# if user cannot start project via .Rproj file then this is necessary
# to allow relative paths to work
setwd("/conf/LIST_analytics/Glasgow City/Falls/TC_Falls_R_Proj")

source("scripts/required_packages.R",echo=F)

if (length(new_packages)>0) {
  cat(
    paste0(
      "\nOne or more packages need to be installed.\n",
      "Please observe the console and wait for notification\n",
      "that all necessary packages have been installed then\n",
      "rerun all of this script (00_main.R).\n"
    )
    
  )
  source("scripts/library.R",echo = F)
  
} else {
  start_time <- Sys.time()
  source("scripts/library.R")
  # date parameters ####
  from_date <- "2010-04-01"
  to_date <- "2020-03-30"
  
  # dates 3 months before and after
  from_date_3m_prior <- lubridate::as_date(from_date) %m-% months(3)
  to_date_3m_after <- lubridate::as_date(to_date) %m+% months(3)
  from_fy <- get_fy(from_date)
  to_fy <- get_fy(to_date)
  #ymd(from_date) %m+% years(10)
  # source sub scripts ####
  
  source("scripts/01_get_data.R",echo=F)
  source("scripts/02_wrangle_data.R",echo=F)
  source("scripts/03_produce_outputs.R",echo=F)
  end_time <- Sys.time()
  total_execution_time <- end_time - start_time
  cat(paste0(
    "\nTotal execution time for script:\n",
    structure(
      format(total_execution_time),
      names = names(total_execution_time)
    )
  ))
}
