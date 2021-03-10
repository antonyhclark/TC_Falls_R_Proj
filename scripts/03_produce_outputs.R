# 0) all Scotland, all ages FY19 to check vs. Discovery
# 1) Overall age 65+ admission rates for GGC and Scotland by year for the last X years (5/10 years?)
# 2) GGC admission rate by age groups <65, 65-75, 75-85 and 85+ by year for the last X years
# 3) GGC admission rate by sex and age groups 65-69, 70-74, 75-79, 80-84, 85-89 and 90+ (average over last 3 years)
# 4) Overall age 65+ admission rates for GGC and Scotland by location (code 33-35) (average of last 3 years)
# compare with
# https://viz.nhsnss.scot.nhs.uk/#/site/Discovery/views/DiscoveryLevel1FallsResidence/HSCPComparison?:iid=1
# https://www.isdscotland.org/Products-and-Services/Discovery/Metadata/Falls.pdf

hbres_of_interest <- "NHS Greater Glasgow and Clyde"
comparator_of_interest <- "Scotland"

# Write main dataframe to Excel
#xlsx::write.xlsx(df_pop,"outputs/df_pop.xlsx")
#xlsx::write.xlsx(df_falls,"outputs/df_falls.xlsx")

# Write to Excel, set up ####
wb <- createWorkbook()
cs1 <- CellStyle(wb) + Font(wb, isItalic = TRUE) # rowcolumns
cs2 <- CellStyle(wb) + Font(wb, color = "blue")
cs3 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border() # header


# 0) all Scotland, all ages FY19 to check vs. Discovery ####
Sco_all_ages <- full_join(
  x = df_falls %>%
    mutate(hb2019name = comparator_of_interest) %>%
    group_by(hb2019name, fy) %>%
    summarise(n_falls = sum(n_falls),.groups="drop"),
  y = df_pop %>%
    mutate(hb2019name = comparator_of_interest) %>%
    group_by(hb2019name, fy) %>%
    summarise(pop = sum(pop),.groups="drop")
) %>% mutate(rate=1000*n_falls/pop)

total_pop_FY17_FY19 <- Sco_all_ages %>% filter(fy %in% 2017:2019) %>% summarise(total_pop=sum(pop))
total_falls_FY17_FY19 <- Sco_all_ages %>% filter(fy %in% 2017:2019) %>% summarise(total_falls=sum(n_falls))
GCC_all_ages <- full_join(
  x = df_falls %>%
    filter(hb2019name == hbres_of_interest) %>%
    group_by(hb2019name, fy) %>%
    summarise(n_falls = sum(n_falls),.groups="drop"),
  y = df_pop %>%
    filter(hb2019name == hbres_of_interest) %>%
    group_by(hb2019name, fy) %>%
    summarise(pop = sum(pop),.groups="drop")
) %>% mutate(rate=1000*n_falls/pop)

Sco_GCC_all_ages <- bind_rows(Sco_all_ages,GCC_all_ages)

sheet_Sco_GCC_all_ages <- createSheet(wb, sheetName = "0_Sco_GCC_all_ages")

addDataFrame(
  Sco_GCC_all_ages, sheet_Sco_GCC_all_ages,
  colnamesStyle = cs3, rownamesStyle = cs1,
  colStyle = list(`1` = cs2, `2` = cs2)
)

# 1) Overall age 65+ admission rates for GGC and Scotland ####
# by year for the last X years (5/10 years?) 
Sco_65plus <- full_join(
  x = df_falls %>%
    filter(age>=65) %>% 
    mutate(hb2019name = comparator_of_interest) %>%
    group_by(hb2019name, fy) %>%
    summarise(n_falls = sum(n_falls),.groups="drop"),
  y = df_pop %>%
    filter(age>=65) %>% 
    mutate(hb2019name = comparator_of_interest) %>%
    group_by(hb2019name, fy) %>%
    summarise(pop = sum(pop),.groups="drop")
) %>% mutate(rate=1000*n_falls/pop)

GCC_65plus <- full_join(
  x = df_falls %>%
    filter(age>=65) %>% 
    filter(hb2019name == hbres_of_interest) %>%
    group_by(hb2019name, fy) %>%
    summarise(n_falls = sum(n_falls),.groups="drop"),
  y = df_pop %>%
    filter(age>=65) %>% 
    filter(hb2019name == hbres_of_interest) %>%
    group_by(hb2019name, fy) %>%
    summarise(pop = sum(pop),.groups="drop")
) %>% mutate(rate=1000*n_falls/pop)

Sco_GCC_65plus <- bind_rows(Sco_65plus,GCC_65plus)
sheet_Sco_GCC_65plus <- createSheet(wb, sheetName = "1_Sco_GCC_65plus")
addDataFrame(
  Sco_GCC_65plus,
  sheet_Sco_GCC_65plus,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3,
  rownamesStyle = cs1,
  colStyle = list(`1` = cs2, `2` = cs2)
)

# 2) GGC admission rate by age groups ####
# <65, 65-75, 75-85 and 85+ by year for the last X years 

Sco_age_group_v1 <- full_join(
  x = df_falls %>%
    mutate(hb2019name = comparator_of_interest) %>%
    group_by(hb2019name,fy, age_group1) %>%
    summarise(n_falls = sum(n_falls), .groups = "drop"),
  y = df_pop %>%
    mutate(hb2019name = comparator_of_interest) %>%
    group_by(hb2019name,fy, age_group1) %>%
    summarise(pop = sum(pop), .groups = "drop")
) %>% mutate(rate = 1000 * n_falls / pop)


GCC_age_group_v1 <- full_join(
  x = df_falls %>%
    filter(hb2019name == hbres_of_interest) %>%
    group_by(hb2019name,fy, age_group1) %>%
    summarise(n_falls = sum(n_falls), .groups = "drop"),
  y = df_pop %>%
    filter(hb2019name == hbres_of_interest) %>%
    group_by(hb2019name,fy, age_group1) %>%
    summarise(pop = sum(pop), .groups = "drop")
) %>% mutate(rate = 1000 * n_falls / pop)

Sco_GCC_age_groupv1 <- bind_rows(Sco_age_group_v1, GCC_age_group_v1)
sheet_Sco_GCC_agegroupv1 <- createSheet(wb, sheetName = "2_Sco_GCC_agegroupv1")
addDataFrame(
  Sco_GCC_age_groupv1,
  sheet_Sco_GCC_agegroupv1,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3,
  rownamesStyle = cs1,
  colStyle = list(`1` = cs2, `2` = cs2, `3` = cs2)
)

# 3) GGC admission rate by sex and age groups ####
# 65-69, 70-74, 75-79, 80-84, 85-89 and 90+ (average over last 3 years)

years_of_interest <- 2017:2019

Sco_sex_agegroupv2 <- 
  full_join(
    x = df_falls %>% 
      mutate(hb2019name=comparator_of_interest) %>% 
      filter(fy %in% 2017:2019,age>=65) %>% 
      group_by(hb2019name,sex,age_group2) %>% 
      summarise(n_falls=sum(n_falls),.groups="drop"),
    y = df_pop %>% 
      mutate(hb2019name=comparator_of_interest) %>% 
      filter(fy %in% 2017:2019,age>=65) %>% 
      group_by(hb2019name,sex,age_group2) %>%
      summarise(pop=sum(pop),.groups="drop")
  ) %>% mutate(rate = 1000 * n_falls / pop)

GCC_sex_agegroupv2 <- 
  full_join(
    x = df_falls %>% 
      filter(fy %in% years_of_interest,age>=65,hb2019name==hbres_of_interest) %>% 
      group_by(hb2019name,sex,age_group2) %>% 
      summarise(n_falls=sum(n_falls),.groups="drop"),
    y = df_pop %>% 
      filter(fy %in% 2017:2019,age>=65,hb2019name==hbres_of_interest) %>% 
      group_by(hb2019name,sex,age_group2) %>% 
      summarise(pop=sum(pop),.groups="drop")
  ) %>% mutate(rate = 1000 * n_falls / pop)

Sco_GCC_sex_agegroupv2 <- bind_rows(Sco_sex_agegroupv2,GCC_sex_agegroupv2)
sheet_Sco_GCC_sex_agegroupv2 <- createSheet(wb, sheetName = "3_Sco_GCC_sex_agegroupv2")
addDataFrame(
  Sco_GCC_sex_agegroupv2,
  sheet_Sco_GCC_sex_agegroupv2,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3,
  rownamesStyle = cs1,
  colStyle = list(`1` = cs2, `2` = cs2, `3` = cs2)
)

# 4) Overall age 65+ admission rates for GGC and Scotland ####
# by location (code 33-35) (average of last 3 years)
years_of_interest <- 2017:2019
Sco_65plus_admiss_type <- 
  full_join(
    x = df_falls %>% 
      mutate(hb2019name=comparator_of_interest) %>% 
      filter(fy %in% years_of_interest,age>=65) %>% 
      group_by(hb2019name,admission_type) %>% 
      summarise(n_falls=sum(n_falls),.groups="drop"),
    y = df_pop %>% 
      mutate(hb2019name=comparator_of_interest) %>% 
      filter(fy %in% years_of_interest,age>=65) %>% 
      group_by(hb2019name) %>%
      summarise(pop=sum(pop),.groups="drop")
  ) %>% mutate(rate = 1000 * n_falls / pop)

GCC_65plus_admiss_type <- 
  full_join(
    x = df_falls %>% 
      filter(hb2019name==hbres_of_interest) %>% 
      filter(fy %in% years_of_interest,age>=65) %>% 
      group_by(hb2019name,admission_type) %>% 
      summarise(n_falls=sum(n_falls),.groups="drop"),
    y = df_pop %>% 
      filter(hb2019name==hbres_of_interest) %>% 
      filter(fy %in% years_of_interest,age>=65) %>% 
      group_by(hb2019name) %>%
      summarise(pop=sum(pop),.groups="drop")
  ) %>% mutate(rate = 1000 * n_falls / pop)

Sco_GCC_65plus_admiss_type <- bind_rows(Sco_65plus_admiss_type,GCC_65plus_admiss_type)
sheet_Sco_GCC_65plus_admiss_type <- createSheet(wb, sheetName = "4_sheet_Sco_GCC_65plus_admiss_type")
addDataFrame(
  Sco_GCC_65plus_admiss_type,
  sheet_Sco_GCC_65plus_admiss_type,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3,
  rownamesStyle = cs1,
  colStyle = list(`1` = cs2, `2` = cs2)
)



# 5a) Overall age 65+ admission rates for GGC and Scotland ####
# by location (code 33-35) and age group (v1 - 0-64, 65-74, 75-84, 85)  
# (average of last 3 years)

years_of_interest <- 2017:2019

this_x <- df_falls %>% 
  mutate(hb2019name=comparator_of_interest) %>% 
  filter(fy %in% 2017:2019) %>% 
  group_by(hb2019name,admission_type,age_group1) %>% 
  summarise(n_falls=sum(n_falls),.groups="drop")

this_y <- df_pop %>% 
  mutate(hb2019name=comparator_of_interest) %>% 
  filter(fy %in% 2017:2019) %>% 
  group_by(hb2019name,age_group1) %>%
  summarise(pop=sum(pop),.groups="drop")
# this_x %>% summarise(total_falls=sum(n_falls)) == total_falls_FY17_FY19
# this_y %>% summarise(total_pop=sum(pop)) == total_pop_FY17_FY19

# this_y %>% View()

Sco_admiss_type_agegroupv1 <- 
  full_join(
    x = this_x,
    y = this_y
  ) %>% mutate(rate = 1000 * n_falls / pop)

# Sco_admiss_type_agegroupv1 %>% summarise(total_pop=sum(pop)) %>% `/`(3) == total_pop_FY17_FY19
# Sco_admiss_type_agegroupv1 %>% View()

# https://stackoverflow.com/questions/25956178/proper-idiom-for-adding-zero-count-rows-in-tidyr-dplyr
# df_falls$age_group1 %>% class()
this_x <- df_falls %>% 
  filter(fy %in% years_of_interest,hb2019name==hbres_of_interest) %>% 
  group_by(hb2019name,admission_type,age_group1,.drop = F) %>% 
  summarise(n_falls=sum(n_falls),.groups="drop") %>% 
  mutate(age_group1=as.ordered(age_group1))
# this_x$age_group1 %>% class()
this_y <- df_pop %>% 
  filter(fy %in% 2017:2019,hb2019name==hbres_of_interest) %>% 
  group_by(hb2019name,age_group1) %>% 
  summarise(pop=sum(pop),.groups="drop")


GCC_admiss_type_agegroupv1 <- 
  full_join(x = this_x, y = this_y) %>% 
  mutate(rate = 1000 * n_falls / pop)

Sco_GCC_admiss_type_agegroupv1 <- bind_rows(Sco_admiss_type_agegroupv1,
                                            GCC_admiss_type_agegroupv1)
sheet_Sco_GCC_admiss_type_agegroupv1 <- createSheet(wb, sheetName = "5a_Sco_GCC_admiss_type_agegroupv1")
addDataFrame(
  Sco_GCC_admiss_type_agegroupv1,
  sheet_Sco_GCC_admiss_type_agegroupv1,
  startRow = 1,
  startColumn = 1,
  colnamesStyle = cs3,
  rownamesStyle = cs1,
  colStyle = list(`1` = cs2, `2` = cs2, `3` = cs2)
)


# THIS SHOULD BE AT THE END OF THE SCRIPT ####
saveWorkbook(wb, "outputs/GCC_falls_FY10_FY19v0.1.xlsx")

