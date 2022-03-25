CJ(date = seq(range(owid_epi$date)[1],
              range(owid_epi$date)[2],
              "day"),
   iso3c = unique(owid_epi$iso3c)) %>% 
  left_join(owid_epi %>% 
              dplyr::select(iso3c, date, deaths),
            by = c("iso3c", "date")) %>% 
  left_join(owid_epi %>% 
              dplyr::select(loc, iso3c) %>%
              unique(),
            by = "iso3c") %>% 
  group_by(loc, iso3c) %>% 
  mutate(n_NA = length(which(is.na(deaths)))/length(deaths),
         year = year(date),
         deaths_daily_max = max(deaths, na.rm = T),
         # cases_daily_max = max(cases),
         deaths_tot = sum(deaths, na.rm = T),
         deaths_daily_prop = deaths/deaths_tot,
         deaths_daily_prop_max = max(deaths_daily_prop, na.rm = T)) %>% 
  filter(deaths_daily_max > 10, #) %>% 
         # dplyr::filter(!is.na(deaths_tot)) %>% pull(iso3c) %>% unique %>% length
         # ,
         deaths_daily_prop_max < 0.05) %>% 
  mutate(deaths = if_else(is.na(deaths), 0, deaths)) -> tmp

owid_vac %>% 
  filter(!is.na(total_vaccinations)) %>% 
  left_join(pop %>% 
              mutate(tot = f+m) %>% 
              group_by(iso3c) %>% 
              summarise(tot = sum(tot)*1000),
            by = "iso3c") %>% 
  mutate(cov = total_vaccinations/(2*tot)) %>%
  filter(iso3c %in% members$iso3c) -> tmp_vac

tmp_vac %>% 
  filter(cov > 0.1) %>% 
  group_by(location, iso3c) %>% 
  summarise(t_start = min(date), .groups = "drop") %>% 
  dplyr::select(-location) %>% 
  right_join(tmp %>% 
               dplyr::select(loc, iso3c, date, deaths) %>% 
               dplyr::filter(deaths > 0) %>% 
               mutate(date_min = min(date)) %>% 
               dplyr::filter(date == date_min) %>% 
               ungroup %>% 
               dplyr::select(iso3c, date_min),
             by = "iso3c") %>% 
  mutate(t_start = if_else(is.na(t_start), max(t_start, na.rm = T), t_start)) %>% 
  left_join(tmp %>% 
              dplyr::select(iso3c, loc) %>% 
              unique,
            by = "iso3c") %>% 
  mutate(fw_UL = date_min - ymd("2019-12-01"),
         fw_LL = fw_UL - 90,
         fw_UL = as.numeric(fw_UL),
         fw_LL = as.numeric(fw_LL)) %>% 
  arrange(loc) %>% 
  rownames_to_column(var = "index") %>% 
  mutate(vocw_LL = as.numeric(ymd("2021-01-16")-date_min),
         vocw_UL = as.numeric(ymd("2021-07-15")-date_min))-> stop_fitting

read_rds("data/intermediate/fitted_parameters_4.rds") %>%
  bind_rows() %>%
  setNames(c("r","t0","rr","voc2")) %>%
  bind_cols(stop_fitting %>%
              dplyr::select(iso3c, index, t_start, date_min, loc)) %>%
  rename(t_end_fitting = t_start) %>%
  mutate(t_intro = ymd("2019-12-01") + t0,
         t_intro_voc1 = ymd("2021-01-15"),
         t_intro_voc2 = date_min + voc2,
         t_intro_voc3 = ymd("2021-12-01")) -> fitted_table

fitted_nigeria <- read_rds("data/intermediate/fitted_parameters_5_nigeria.rds")
i <- which(fitted_table$loc == "Nigeria")
fitted_table[i,"r"] <- fitted_nigeria[1]
fitted_table[i,"t0"] <- fitted_nigeria[2]
fitted_table[i,"t_intro"] <- fitted_nigeria[2] + ymd("2019-12-01")
fitted_table[i,"rr"] <- fitted_nigeria[3]
fitted_table[i,"t_intro_voc1"] <- fitted_table[i,"date_min"] + fitted_nigeria[4]
fitted_table[i,"t_intro_voc2"] <- fitted_table[i,"date_min"] + fitted_nigeria[5]
fitted_table[i,]

fitted_ghana <- read_rds("data/intermediate/fitted_parameters_5_ghana.rds")
i <- which(fitted_table$loc == "Ghana")
fitted_table[i,"r"] <- fitted_ghana[1]
fitted_table[i,"t0"] <- fitted_ghana[2]
fitted_table[i,"t_intro"] <- fitted_ghana[2] + ymd("2019-12-01")
fitted_table[i,"rr"] <- fitted_ghana[3]
fitted_table[i,"t_intro_voc1"] <- fitted_table[i,"date_min"] + fitted_ghana[4]
fitted_table[i,"t_intro_voc2"] <- fitted_table[i,"date_min"] + fitted_ghana[5]
fitted_table[i,]

fitted_ethiopia<- read_rds("data/intermediate/fitted_parameters_5_ethiopia.rds")
i <- which(fitted_table$loc == "Ethiopia")
fitted_table[i,"r"] <- fitted_ethiopia[1]
fitted_table[i,"t0"] <- fitted_ethiopia[2]
fitted_table[i,"t_intro"] <- fitted_ethiopia[2] + ymd("2019-12-01")
fitted_table[i,"rr"] <- fitted_ethiopia[3]
fitted_table[i,"t_intro_voc1"] <- fitted_table[i,"date_min"] + fitted_ethiopia[4]
fitted_table[i,"t_intro_voc2"] <- fitted_table[i,"date_min"] + fitted_ethiopia[5]
fitted_table[i,]


fitted_table %<>%
  mutate(rc_severity_1 = 1,
         rc_severity_2 = 1.5,
         rc_severity_3 = 0.5,

         rc_transmissibility_1 = 1,
         rc_transmissibility_2 = 1.5,
         rc_transmissibility_3 = 1.5,

         rc_ve_1 = 1,
         rc_ve_2 = 0.9,
         rc_ve_3 = 0.7,

         rc_transmissibility_1 = if_else(loc %in% c("Ethiopia", "Ghana", "Nigeria"),
                                         1.5,
                                         rc_transmissibility_1))

fitted_table[fitted_table$iso3c=="COD", "loc"] <- "Dem. Republic of the Congo"

# adjust fitted table transmissibility 
fitted_table %<>% 
  data.table %>% 
  mutate(rc_transmissibility_overall = rc_transmissibility_1 * rc_transmissibility_2 * rc_transmissibility_3,
         rc_transmissibility_max = max(rc_transmissibility_overall),
         rc_transmissibility_3 = rc_transmissibility_max/(rc_transmissibility_1 * rc_transmissibility_2),
         rc_transmissibility_overall = rc_transmissibility_1 * rc_transmissibility_2 * rc_transmissibility_3) %>% 
  dplyr::select(-rc_transmissibility_max, -rc_transmissibility_overall)



# write_rds(fitted_table, "data/intermediate/fitted_table.rds")
# 
# fitted_table <- read_rds("data/intermediate/fitted_table.rds")
# CJ(date = seq(range(owid_epi$date)[1],
#               range(owid_epi$date)[2],
#               "day"),
#    iso3c = unique(owid_epi$iso3c)) %>% 
#   left_join(owid_epi %>% 
#               dplyr::select(iso3c, date, deaths),
#             by = c("iso3c", "date")) %>% 
#   left_join(owid_epi %>% 
#               dplyr::select(loc, iso3c) %>%
#               unique(),
#             by = "iso3c") %>% 
#   group_by(loc, iso3c) %>% 
#   mutate(n_NA = length(which(is.na(deaths)))/length(deaths),
#          year = year(date),
#          deaths_daily_max = max(deaths, na.rm = T),
#          # cases_daily_max = max(cases),
#          deaths_tot = sum(deaths, na.rm = T),
#          deaths_daily_prop = deaths/deaths_tot,
#          deaths_daily_prop_max = max(deaths_daily_prop, na.rm = T)) %>% 
#   filter(deaths_daily_max > 10,
#          deaths_daily_prop_max < 0.05) %>% 
#   mutate(deaths = if_else(is.na(deaths), 0, deaths)) -> tmp
# 
# 
# 
# # draw_fit(read_rds("data/intermediate/fitted_parameters_4.rds") %>% .[[1]], 1)
