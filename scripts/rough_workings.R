
x <- get_smr_data(
  query = 
    "SELECT * FROM ANALYSIS.SMR01_PI WHERE ROWNUM<=10"
)

x %>% select(contains("res"))
x %>% select(contains("hb"))
x %>% select(contains("condition"))

###############################################################
# Fall flag, create and filter by [purrr] - quite slow :-( ####
# https://icd.who.int/browse10/2010/en#/W00-W19
# Create a flag for falls
# s_purrr <- Sys.time()
# df_smr1_stay <- df_smr1_stay %>%
#   mutate(fall_flag = purrr::pmap_dbl(select(., contains("condition")),
#                                      ~ any(grepl("^W[00-19]", c(
#                                        ...
#                                      )),
#                                      na.rm = TRUE)))
# e_purrr <- Sys.time()
# t_purrr <- e_purrr - s_purrr
# fall_purrr <- df_smr1_stay$fall_flag
# Fall flag, create and filter by [apply] - bit quicker####
# https://icd.who.int/browse10/2010/en#/W00-W19
# Create a flag for falls
# s_apply <- Sys.time()
# fall_pattern <- "^W[00-19]"
# col_indices <- grep(colnames(df_smr1_stay),pattern="condition")
# df_smr1_stay$fall_flag <-
#   apply(df_smr1_stay[, ..col_indices], 1,
#         function(x) {
#           as.numeric(any(grep(fall_pattern, x)))
#         })
# e_apply <- Sys.time()
# t_apply <- e_apply - s_apply
# t_apply
# identical(df_smr1_stay$fall_flag,df_smr1_stay$fall_flag_apply)

# Investigation into doing more with SQL and less in R ####
# start <- Sys.time()
# query <- read_file("sql/query_01_Falls_clausev2.sql") #%>%
#   #sprintf(., from_date, to_date)
# cat(query)
# x <- get_smr_data(query)
# end <- Sys.time()
# end-start
# x %>% nrow()


# Experiments with dplyr type functions
# my_filter <- function(df_falls,fall_filter){
#   df %>% filter({{this_filter}})
# }
# is.null(NULL)
# df_smr1_stay[,.N]
# df_smr1_stay %>% filter() %>% nrow()
# my_filter(df_smr1_stay,age_in_years>=65 & hb2019name==hb_of_interest)
# 
# 
# my_group_by <- function(df,grouping_vector){
#   df %>% group_by(all_of({{grouping_vector}}))
# }
# 
# df_smr1_stay %>% my_group_by(c(FY,datazone_2011))
# 
# get_fall_rates <- function(df_falls,filter_falls,agg_vec,
#                            df_pop,filter_pop,agg_pop){
#   df_falls_rates <- df_falls %>%
#     filter({{filter_falls}}) %>%
#     group_by(across({{agg_vec}})) %>%
#     summarise(n_falls=sum(fall_flag),.groups="drop")
# 
# 
# 
#   return(df_falls_rates)
# }
# 1000*37812/5463300
# 
# df_pop_falls_hb_fy_age_sex %>% filter(FY==2019) %>% summarise(pop=sum(pop,na.rm = T)/3)
# 
# 
# # 0) all Scotland, all ages FY19 to check vs. Discovery
# step1 <- df_pop_falls_hb_fy_age_sex %>%
#   filter(admission_type_33_35 %in% c("33","34","35")) %>% 
#   group_by(hb2019name, FY,age,sex) %>%
#   summarise(
#     n_falls = sum(n_falls, na.rm = T),
#     pop = first(pop),
#     .groups = "drop"
#   ) %>% 
#   filter(FY==2019) %>% 
#   mutate(hb2019name=comparator_of_interest) %>% 
#   group_by(hb2019name, FY) %>%
#   summarise(
#     n_falls = sum(n_falls, na.rm = T),
#     pop = sum(pop, na.rm = T),
#     .groups = "drop"
#   ) %>%
#   mutate(rate = 1000 * n_falls / pop)
# 
# 
# 
# # 1) Overall age 65+ admission rates for GGC and Scotland 
# # by year for the last X years (5/10 years?) ####
# 
# # Scotland
# df_Sco_65plus <-
#   df_pop_falls_hb_fy_age_sex %>%
#   mutate(hb2019name = comparator_of_interest) %>%
#   filter(age >= 65) %>%
#   group_by(hb2019name, FY) %>%
#   summarise(
#     n_falls = sum(n_falls, na.rm = T),
#     pop = sum(pop, na.rm = T),
#     .groups = "drop"
#   ) %>%
#   mutate(rate = 1000 * n_falls / pop)
# 
# # GCC
# df_GCC_65plus <- 
#   df_pop_falls_hb_fy_age_sex %>%
#   #mutate(hb2019name = comparator_of_interest) %>%
#   filter(age >= 65,
#          hb2019name == hb_of_interest) %>%
#   group_by(hb2019name, FY) %>%
#   summarise(
#     n_falls = sum(n_falls, na.rm = T),
#     pop = sum(pop, na.rm = T),
#     .groups = "drop"
#   ) %>%
#   mutate(rate = 1000 * n_falls / pop)
# 
# df_Sco_GCC_65plus <- bind_rows(df_Sco_65plus,df_GCC_65plus)
# df_Sco_GCC_65plus %>% 
#   ggplot(aes(x=FY,y=rate,colour=hb2019name,shape=hb2019name))+
#   geom_point()+
#   geom_line()+
#   theme_classic()+
#   scale_x_continuous(breaks = unique(df_Sco_GCC_65plus$FY))+
#   scale_y_continuous(limits = c(0,10))
# 
# # 2) GGC admission rate by age groups:
# # <65, 65-75, 75-85 and 85+ by year for the last X years ####
# df_pop_falls_hb_fy_age_sex %>% 
#   filter(hb2019name == hb_of_interest) %>% 
#   group_by(hb2019name,FY,age_group1) %>% 
#   summarise(
#     n_falls = sum(n_falls, na.rm = T),
#     pop = sum(pop, na.rm = T),
#     .groups = "drop"
#   ) %>%
#   mutate(rate = 1000 * n_falls / pop) %>% 
#   ggplot(aes(x=FY,y=rate,colour=age_group1,shape=age_group1))+
#   geom_line()+
#   geom_point(size=4)+
#   theme_classic()
# 
# # 3) GGC admission rate by sex and age groups:
# # 65-69, 70-74, 75-79, 80-84, 85-89 and 90+ 
# # (average over last 3 years)
# df_pop_falls_hb_fy_age_sex %>% 
#   filter(hb2019name == hb_of_interest,
#          FY %in% c(2017:2019)) %>% 
#   group_by(hb2019name,sex,age_group2) %>% 
#   summarise(
#     n_falls = sum(n_falls, na.rm = T),
#     pop = sum(pop, na.rm = T),
#     .groups = "drop"
#   ) %>%
#   mutate(rate = 1000 * n_falls / pop) %>% 
#   ggplot(aes(x=age_group2,y=rate,fill=sex))+
#   geom_bar(stat = "identity")+
#   theme_classic()
# 
# 
# # 4) Overall age 65+ admission rates for GGC and Scotland by 
# # location (code 33-35) (average of last 3 years)
# df_pop_falls_hb_fy_age_sex %>%
#   filter(age >= 65,
#          hb2019name == hb_of_interest,
#          FY %in% c(2017:2019)) %>%
#   group_by(hb2019name, admission_type_33_35, sex) %>%
#   summarise(
#     n_falls = sum(n_falls, na.rm = T),
#     pop = sum(pop, na.rm = T),
#     .groups = "drop"
#   ) %>%
#   mutate(rate = 1000 * n_falls / pop) %>%
#   ggplot(aes(x = admission_type_33_35, y = rate, fill = sex)) +
#   geom_bar(stat = "identity") +
#   theme_classic()