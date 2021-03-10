# Aggregate by Link, CIS ####
# If done by dplyr, quite slow data.table much faster
df_smr1_stay <- df_smr1 %>%
  data.table::as.data.table() %>%
  .[,
    list(
      upi = first(upi_number),
      admission_date = first(admission_date),
      discharge_date = last(discharge_date),
      admission_type = first(admission_type),
      main_condition = first(main_condition),
      condition_1 = first(other_condition_1),
      condition_2 = first(other_condition_2),
      condition_3 = first(other_condition_3),
      condition_4 = first(other_condition_4),
      condition_5 = first(other_condition_5),
      datazone2011 = first(datazone_2011),
      council_area_2019 = first(council_area_2019),
      hbres_currentdate = first(hbres_currentdate),
      age = first(age_in_years),
      sex = last(sex),
      length_of_stay = sum(length_of_stay)
    ),
    by = 'link_no,cis_marker'] %>% 
  as.tibble() %>% 
  mutate(fy=get_fy(admission_date)) %>% 
  filter(admission_date>=lubridate::as_date(from_date),
         discharge_date<=lubridate::as_date(to_date))

# Fall flag, create and filter by [tidyr::unite] - fast! ####
# alternative means including at end of script, commented out

# Don't need this code if we can do the filter in SQL (?):
fall_pattern_unite <- ".*W[00-19].*"
df_smr1_stay$fall_flag <- df_smr1_stay %>%
  unite(.,conditions,contains("condition"),sep = ",") %>%
  pull(conditions) %>%
  grepl(x=.,pattern = fall_pattern_unite) %>%
  as.numeric()
df_smr1_stay <- df_smr1_stay %>% filter(fall_flag==1)
#(df_smr1_stay$fall_flag==1) %>% all()
#remove(fall_pattern_unite)
# Filter for admissions with fall as condition ####

#df_smr1_stay %>% tabyl(admission_type) %>% arrange(-percent) %>% mutate(percent=round(percent*100,2))

df_falls <- df_smr1_stay %>%
  group_by(hbres_currentdate, fy, age, sex, admission_type) %>%
  summarise(n_falls = n(), .groups = "drop") %>% 
  mutate(sex=as.character(sex)) %>% # to match data type in pop file
  left_join(df_hb_lookup, by=c("hbres_currentdate"="hb2019")) %>% 
  relocate(hb2019name)


# Add age group cols ####
age_group1_breaks <- c(0, 65, 75, 85, Inf) # [0-65) etc.
age_group1_labs <- c("0-64", "65-74", "75-84", "85 plus")
age_group2_breaks <- c(0, 65, 70, 75, 80, 85, 90, Inf)
age_group2_labs <- c("0-64", "65-69", "70-74",
                     "75-79", "80-84", "85-89", "90 plus")
df_falls %<>% 
  add_age_group_col(age, age_group1, age_group1_breaks, age_group1_labs) %>%
  add_age_group_col(age, age_group2, age_group2_breaks, age_group2_labs)

df_pop %<>% 
  add_age_group_col(age, age_group1, age_group1_breaks, age_group1_labs) %>%
  add_age_group_col(age, age_group2, age_group2_breaks, age_group2_labs)

remove(age_group1_breaks,age_group1_labs,age_group2_breaks,age_group2_labs)