# SMR RAW EXTRACT - ALL SCOTLAND ####
# If data for date range already extracted, load that file
# otherwise, get data from SMR01 directly
smr1_csv_name <-
  paste("ANALYSIS.SMR01_PI", from_date_3m_prior, to_date_3m_after, sep = '_') %>%
  paste0(., ".csv")
smr1_csv_path <- paste0("data/", smr1_csv_name) %>%
  normalizePath() %>%
  suppressWarnings()
# need to be careful with using sprintf of sql
# sprintf can interpret certain sql symbols
# as meaning variables to be subsituted
# need to be appropriately escaped
query <- read_file("sql/query_01.sql") %>% 
  sprintf(., from_date_3m_prior, to_date_3m_after)
# cat(query)
if (file.exists(smr1_csv_path)) {
  # data has previously been extracted
  df_smr1 <- data.table::fread(smr1_csv_path) %>% as_tibble() %>% 
    mutate(admission_date=lubridate::as_datetime(admission_date),
           discharge_date=lubridate::as_datetime(discharge_date))
} else {
  # get data from database directly
  df_smr1 <- get_smr_data(query)
  data.table::fwrite(df_smr1, smr1_csv_path)
}
# df_smr1 %>% select(contains("condition")) %>% View()
remove(smr1_csv_name, smr1_csv_path,
       query) # clean Env.

# get HSCP, Locality, DZ11 lookup ####
df_hscp_loc_dz11 <- get_dz11_lookup()

df_hb_lookup <- df_hscp_loc_dz11 %>% select(hb2019,hb2019name) %>% unique()
remove(get_dz11_lookup, get_smr_data, df_hscp_loc_dz11) # clean env.

# get pop data ####
pop_directory <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates"
hb_2011_2019_file <- "HB2019_pop_est_1981_2019.rds"
df_pop <- readRDS(file = file.path(pop_directory,hb_2011_2019_file)) %>% 
  as.tibble() %>% 
  filter(year>=from_fy,
         year<=to_fy) %>% 
  select(hb2019name,hb2019,fy=year,age,sex,pop) %>%
  mutate(sex=as.character(sex))
  
  
suppressWarnings({
  remove(pop_directory,hb_2011_2019_file) # Clean env  
})

# df_pop_hb_fy_age_sex %>% filter(FY==2019) %>% summarise(pop=sum(pop)) # ==5,463,300 (same as Discovery)

