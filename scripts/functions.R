# build library ####
library(readr)
library(odbc)


# plot_with_lables ####
# p is a plot object, l is a mapping between variable names and labels
# https://stackoverflow.com/questions/51238042/key-value-mapping-of-axis-variable-labels-in-ggplot/51239178
plot_with_labels <- function(p, l) {
  p$labels <- lapply(p$labels, function(x) { as.character(l[x]) } )
  return(p)
}

# get data from SMR Oracle DB ####
get_smr_data <- function(query) {
  # get credential from file in unix home
  # pwd_file <- read_csv("~/pwd.csv")
  # usr <- as.character(pwd_file[1, "user"])
  # pwd <- as.character(pwd_file[1, "pwd"])

  # create channel to connect to Oracle DB
  channel <- suppressWarnings(
    dbConnect(
      odbc(),
      dsn = "SMRA",
      uid = rstudioapi::askForPassword("Please enter LDAP username:"),
      pwd = rstudioapi::askForPassword("Please enter LDAP password: "),

      port = "1527",
      host = "nssstats01.csa.scot.nhs.uk",
      SVC = "SMRA.nss.scot.nhs.uk"
    )
  )
  smr_extract <- dbGetQuery(channel, statement = query) %>%
    as_tibble() %>%
    clean_names()
  dbDisconnect(channel)
  return(smr_extract)
}

# hard coded function to get subset of columns from the HSCP-Locality-dz11 lookup file ####
get_dz11_lookup <- function(columns=c("hb2019","hb2019name","hscp2019name","hscp_locality","datazone2011")){
  dir_HSCP_Loc <- "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality"
  file_pattern <- "^HSCP Localities_DZ11_Lookup_.*\\.csv" #^ begins, .* any chars, \\.=.
  dt_HSCP_Loc_DZ11 <- read_csv(file = dir(dir_HSCP_Loc, 
                                        full.names = T, 
                                        pattern = file_pattern))
  # cut down to columns of interest
  return(dt_HSCP_Loc_DZ11 %>% select(all_of(columns)))
}

# add age group colum ####
add_age_group_col <- function(df,age_col,age_group_col,breaks,labels) {
  df %>% mutate(
    {{age_group_col}} := cut(
      {{age_col}},
      breaks = breaks,
      right = FALSE,
      ordered_result = TRUE,
      labels = labels
    )
  ) %>% return()
}
# get financial year from date ####
get_fy <- function(date,fy_start_month = 4){
  
  fy <- ifelse(lubridate::month(date)>=fy_start_month,
               lubridate::year(date),
               lubridate::year(date)-1)
  return(fy)
}
# get_fy(as.Date("2021-03-31"))
# get_fy(as.Date("2021-04-01"))



