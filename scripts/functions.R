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


write_df_to_worksheet <- function(df,wb_path,ws_name,tab_colour="white"){
  # Header style
  cs_col_headers <- openxlsx::createStyle(
    fontName = "Calibri", fontSize = 10, fontColour = "black",
    numFmt = "GENERAL", border = c("top", "bottom"),
    borderColour = c("darkslategrey", "black"),
    borderStyle = c("thin", "medium"),
    fgFill = c("lightslategrey"),
    halign = "center", valign = "center",
    textDecoration = c("bold"), wrapText = T
  )
  
  if (!file.exists(wb_path)){
    wb_obj <- openxlsx::createWorkbook()
  } else {
    wb_obj <- openxlsx::loadWorkbook(wb_path)
  }
  ws_obj <- openxlsx::addWorksheet(
    wb=wb_obj, sheetName = ws_name,
    gridLines = F, tabColour = tab_colour, zoom = 90
  )
  
  setColWidths(wb_obj, ws_obj, 1:ncol(df), 
               widths = floor(255/ncol(df)))
  
  writeData(wb_obj, ws_obj, x=df,
            headerStyle = cs_col_headers
            #name = eval(deparse(enexpr(df)))
  )
  saveWorkbook(wb_obj, wb_path, overwrite = T)
}
