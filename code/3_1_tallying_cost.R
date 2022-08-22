discount_r <- 0.03

#### costs of vaccination programs ####
ms_cov_all %>% 
  left_join(vac_denom, by = "iso3c") %>% 
  mutate(year_end1 = ymd("2021-12-31"),
         year_end2 = ymd("2022-12-31"),
         days_diff1_dose1 = as.numeric(year_end1 - date_start),
         days_diff1_dose2 = as.numeric(year_end1 - date_start) - 28,
         days_diff2_dose1 = as.numeric(year_end2 - date_start),
         days_diff2_dose2 = as.numeric(year_end2 - date_start) - 28,
         days_diff2_dose1 = days_diff2_dose1 - days_diff1_dose1,
         days_diff2_dose2 = days_diff2_dose2 - days_diff1_dose2,
         cov_achieved_diff1_dose1 = days_diff1_dose1 *r_vac,
         cov_achieved_diff1_dose2 = days_diff1_dose2 *r_vac,
         cov_achieved_diff2_dose1 = days_diff2_dose1 *r_vac,
         cov_achieved_diff2_dose2 = days_diff2_dose2 *r_vac,
         cov_achieved_diff2_dose1 = if_else(cov_achieved_diff1_dose1 + cov_achieved_diff2_dose1 > 0.7,
                                            0.7 - cov_achieved_diff1_dose1,
                                            cov_achieved_diff2_dose1),
         cov_achieved_diff2_dose2 = if_else(cov_achieved_diff1_dose2 + cov_achieved_diff2_dose2 > 0.7,
                                            0.7 - cov_achieved_diff1_dose2,
                                            cov_achieved_diff2_dose2),
         year1_doses = (cov_achieved_diff1_dose1 + cov_achieved_diff1_dose2)*tot*1000,
         year2_doses = (cov_achieved_diff2_dose1 + cov_achieved_diff2_dose2)*tot*1000,
         vac_unit_disc = vac_unit/((1+discount_r)),
         vac_cost = year1_doses*vac_unit + year2_doses*(vac_unit_disc)) %>%
  dplyr::select(date_start, scenario, Type, name, iso3c, r_vac, Rate, vac_cost) %>% 
  dplyr::select(date_start, Type, scenario, name, iso3c, vac_cost) %>% 
  rename(population = name) %>% 
  mutate(Type = tolower(Type)) -> cost_program

ms_cov_all %>% 
  left_join(vac_denom, by = "iso3c") %>% 
  mutate(year_end1 = ymd("2021-12-31"),
         year_end2 = ymd("2022-12-31"),
         year_end3 = ymd("2023-06-30"),
         days_diff1_dose1 = as.numeric(year_end1 - date_start),
         days_diff1_dose2 = as.numeric(year_end1 - date_start) - 28,
         days_diff2_dose1 = as.numeric(year_end2 - date_start),
         days_diff2_dose2 = as.numeric(year_end2 - date_start) - 28,
         days_diff3_dose1 = as.numeric(year_end3 - date_start),
         days_diff3_dose2 = as.numeric(year_end3 - date_start) - 28,
         days_diff2_dose1 = days_diff2_dose1 - days_diff1_dose1,
         days_diff2_dose2 = days_diff2_dose2 - days_diff1_dose2,
         days_diff3_dose1 = days_diff3_dose1 - days_diff1_dose1 - days_diff1_dose1,
         days_diff3_dose2 = days_diff3_dose2 - days_diff1_dose2 - days_diff2_dose2,
         cov_achieved_diff1_dose1 = days_diff1_dose1 *r_vac,
         cov_achieved_diff1_dose2 = days_diff1_dose2 *r_vac,
         cov_achieved_diff2_dose1 = days_diff2_dose1 *r_vac,
         cov_achieved_diff2_dose2 = days_diff2_dose2 *r_vac,
         cov_achieved_diff3_dose1 = days_diff3_dose1 *r_vac,
         cov_achieved_diff3_dose2 = days_diff3_dose2 *r_vac,
         cov_achieved_diff2_dose1 = if_else(cov_achieved_diff1_dose1 + cov_achieved_diff2_dose1 > 0.7,
                                            0.7 - cov_achieved_diff1_dose1,
                                            cov_achieved_diff2_dose1),
         cov_achieved_diff3_dose1 = if_else(cov_achieved_diff1_dose1 + cov_achieved_diff2_dose1 >= 0.7,
                                            0,
                                            cov_achieved_diff3_dose1),
         cov_achieved_dose1_tot = cov_achieved_diff1_dose1 + 
           cov_achieved_diff2_dose1 + 
           cov_achieved_diff3_dose1,
         cov_achieved_diff2_dose2 = if_else(cov_achieved_diff1_dose2 + cov_achieved_diff2_dose2 > 0.7,
                                            0.7 - cov_achieved_diff1_dose2,
                                            cov_achieved_diff2_dose2),
         cov_achieved_diff3_dose2 = if_else(cov_achieved_diff1_dose2 + cov_achieved_diff2_dose2 + cov_achieved_diff3_dose2 >= 0.7,
                                            0,
                                            cov_achieved_diff3_dose2),
         cov_achieved_dose2_tot = cov_achieved_diff1_dose2 + 
           cov_achieved_diff2_dose2 + 
           cov_achieved_diff3_dose2,
         year1_doses = (cov_achieved_diff1_dose1 + cov_achieved_diff1_dose2)*tot*1000,
         year2_doses = (cov_achieved_diff2_dose1 + cov_achieved_diff2_dose2)*tot*1000,
         year3_doses = (cov_achieved_diff3_dose1 + cov_achieved_diff3_dose2)*tot*1000,
         vac_unit_disc = vac_unit/((1+discount_r)),
         vac_unit_disc2 = vac_unit/((1+discount_r)^2),
         vac_cost = year1_doses*vac_unit + year2_doses*(vac_unit_disc) + year3_doses*(vac_unit_disc2) ) %>% 
  dplyr::select(date_start, scenario, Type, name, iso3c, r_vac, Rate, vac_cost) %>% 
  dplyr::select(date_start, Type, scenario, name, iso3c, vac_cost) %>% 
  rename(population = name) %>% 
  mutate(Type = tolower(Type)) -> cost_program_ext

# with SA:low VE, overall vaccine program costs doesn't change
cost_program_low <- cost_program

#### costs of home-based care ####
res$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res$az$non_fatal %>% 
            mutate(Type = "az")) %>% 
  filter(name == "cases") %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(home = as.numeric(home),
         home = home*(108.596 / 107.303),
         home_disc = home/(1+discount_r),
         home_care_cost = if_else(year == 2021, value*0.1*home, value*0.1*home_disc),
         home_care_cost_novac = if_else(year == 2021, novac*0.1*home, novac*0.1*home_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(home_care_cost = sum(home_care_cost),
            home_care_cost_novac = sum(home_care_cost_novac)) -> cost_home_care

res_ext$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_ext$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "cases") %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(home = as.numeric(home),
         home = home*(108.596 / 107.303),
         home_disc = home/(1+discount_r),
         home_care_cost = if_else(year == 2021, value*0.1*home, value*0.1*home_disc),
         home_care_cost_novac = if_else(year == 2021, novac*0.1*home, novac*0.1*home_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(home_care_cost = sum(home_care_cost),
            home_care_cost_novac = sum(home_care_cost_novac)) -> cost_home_care_ext

res_low$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_low$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "cases") %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(home = as.numeric(home),
         home = home*(108.596 / 107.303),
         home_disc = home/(1+discount_r),
         home_care_cost = if_else(year == 2021, value*0.1*home, value*0.1*home_disc),
         home_care_cost_novac = if_else(year == 2021, novac*0.1*home, novac*0.1*home_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(home_care_cost = sum(home_care_cost),
            home_care_cost_novac = sum(home_care_cost_novac)) -> cost_home_care_low

#### costs of treating severe cases ####
res$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "severe_i_all")  %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp* (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         severe_care_cost = if_else(year == 2021, value*9.6*hosp, value*9.6*hosp_disc),
         severe_care_cost_novac = if_else(year == 2021, novac*9.6*hosp, novac*9.6*hosp_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(severe_care_cost = sum(severe_care_cost),
            severe_care_cost_novac = sum(severe_care_cost_novac)) -> cost_severe_care

res_ext$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_ext$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "severe_i_all")  %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp* (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         severe_care_cost = if_else(year == 2021, value*9.6*hosp, value*9.6*hosp_disc),
         severe_care_cost_novac = if_else(year == 2021, novac*9.6*hosp, novac*9.6*hosp_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(severe_care_cost = sum(severe_care_cost),
            severe_care_cost_novac = sum(severe_care_cost_novac)) -> cost_severe_care_ext

res_low$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_low$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "severe_i_all")  %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp* (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         severe_care_cost = if_else(year == 2021, value*9.6*hosp, value*9.6*hosp_disc),
         severe_care_cost_novac = if_else(year == 2021, novac*9.6*hosp, novac*9.6*hosp_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(severe_care_cost = sum(severe_care_cost),
            severe_care_cost_novac = sum(severe_care_cost_novac)) -> cost_severe_care_low

#### costs of treating critical cases #### 
res$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "critical_i_all")  %>% 
  # dplyr::select(-value) %>% pivot_wider(names_from = Type, values_from = novac)
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp * (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         icu = as.numeric(icu),
         icu = icu * (108.596 / 107.303),
         icu_disc = icu/(1+discount_r),
         critical_care_cost = if_else(year == 2021, 
                                      value*(icu)*12.6 + value*(hosp)*8.88,
                                      value*(icu_disc)*12.6 + value*(hosp_disc)*8.88),
         critical_care_cost_novac = if_else(year == 2021,
                                            novac*icu*12.6 + novac*hosp*8.88,
                                            novac*icu_disc*12.6 + novac*hosp_disc*8.88))  %>%
  # filter(population == "Algeria", scenario_id == 1) #%>% 
  # filter(Type == "az") %>% 
  # dplyr::select(-value) %>% pivot_wider(names_from = Type, values_from = novac)
  group_by(scenario_id, population, name, Type) %>% 
  summarise(critical_care_cost = sum(critical_care_cost),
            critical_care_cost_novac = sum(critical_care_cost_novac)) -> cost_critical_care

res_ext$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_ext$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "critical_i_all")  %>% 
  # dplyr::select(-value) %>% pivot_wider(names_from = Type, values_from = novac)
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp * (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         icu = as.numeric(icu),
         icu = icu * (108.596 / 107.303),
         icu_disc = icu/(1+discount_r),
         critical_care_cost = if_else(year == 2021, 
                                      value*(icu)*12.6 + value*(hosp)*8.88,
                                      value*(icu_disc)*12.6 + value*(hosp_disc)*8.88),
         critical_care_cost_novac = if_else(year == 2021,
                                            novac*icu*12.6 + novac*hosp*8.88,
                                            novac*icu_disc*12.6 + novac*hosp_disc*8.88))  %>%
  # filter(population == "Algeria", scenario_id == 1) #%>% 
  # filter(Type == "az") %>% 
  # dplyr::select(-value) %>% pivot_wider(names_from = Type, values_from = novac)
  group_by(scenario_id, population, name, Type) %>% 
  summarise(critical_care_cost = sum(critical_care_cost),
            critical_care_cost_novac = sum(critical_care_cost_novac)) -> cost_critical_care_ext

res_low$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_low$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "critical_i_all")  %>% 
  # dplyr::select(-value) %>% pivot_wider(names_from = Type, values_from = novac)
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp * (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         icu = as.numeric(icu),
         icu = icu * (108.596 / 107.303),
         icu_disc = icu/(1+discount_r),
         critical_care_cost = if_else(year == 2021, 
                                      value*(icu)*12.6 + value*(hosp)*8.88,
                                      value*(icu_disc)*12.6 + value*(hosp_disc)*8.88),
         critical_care_cost_novac = if_else(year == 2021,
                                            novac*icu*12.6 + novac*hosp*8.88,
                                            novac*icu_disc*12.6 + novac*hosp_disc*8.88))  %>%
  # filter(population == "Algeria", scenario_id == 1) #%>% 
  # filter(Type == "az") %>% 
  # dplyr::select(-value) %>% pivot_wider(names_from = Type, values_from = novac)
  group_by(scenario_id, population, name, Type) %>% 
  summarise(critical_care_cost = sum(critical_care_cost),
            critical_care_cost_novac = sum(critical_care_cost_novac)) -> cost_critical_care_low

# res$pfizer$fatal %>% 
#   mutate(Type = "pfizer") %>% 
#   bind_rows(res$az$fatal %>% 
#               mutate(Type = "az")) %>% 
#   group_by(scenario_id, population, year, Type) %>% 
#   summarise(value = sum(value))

#### costs of managing death cases ####
res$pfizer$fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res$az$fatal %>% 
              mutate(Type = "az")) %>% 
  group_by(scenario_id, population, name, Type, year) %>% 
  summarise(value = sum(value),
            novac = sum(novac)) %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(deaths = as.numeric(deaths),
         deaths_disc = deaths/(1+discount_r),
         death_management_cost = if_else(year == 2021, (deaths)*value, value*deaths_disc),
         death_management_novac_cost = if_else(year == 2021, (deaths)*novac, novac*deaths_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(deaths_management_cost = sum(death_management_cost),
            death_management_cost_novac = sum(death_management_novac_cost)) -> cost_deaths_management

res_ext$pfizer$fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_ext$az$fatal %>% 
              mutate(Type = "az")) %>% 
  group_by(scenario_id, population, name, Type, year) %>% 
  summarise(value = sum(value),
            novac = sum(novac)) %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(deaths = as.numeric(deaths),
         deaths_disc = deaths/(1+discount_r),
         death_management_cost = if_else(year == 2021, (deaths)*value, value*deaths_disc),
         death_management_novac_cost = if_else(year == 2021, (deaths)*novac, novac*deaths_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(deaths_management_cost = sum(death_management_cost),
            death_management_cost_novac = sum(death_management_novac_cost)) -> cost_deaths_management_ext

res_low$pfizer$fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_low$az$fatal %>% 
              mutate(Type = "az")) %>% 
  group_by(scenario_id, population, name, Type, year) %>% 
  summarise(value = sum(value),
            novac = sum(novac)) %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(deaths = as.numeric(deaths),
         deaths_disc = deaths/(1+discount_r),
         death_management_cost = if_else(year == 2021, (deaths)*value, value*deaths_disc),
         death_management_novac_cost = if_else(year == 2021, (deaths)*novac, novac*deaths_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(deaths_management_cost = sum(death_management_cost),
            death_management_cost_novac = sum(death_management_novac_cost)) -> cost_deaths_management_low

#### summing up all costs #### 
CJ(scenario_id = 1:36,
   iso3c = fitted_table$iso3c) %>% 
  left_join(members, by = "iso3c") %>% 
  dplyr::select(-name_official) %>%
  rename(population = name_internal) %>% 
  mutate(scenario_id = as.character(scenario_id)) %>% 
  left_join(cost_home_care %>% dplyr::select(-name), by = c("scenario_id", "population")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_severe_care, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_critical_care, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_deaths_management, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>%
  left_join(ms_scenarios, by = "scenario_id") %>% 
  left_join(cost_program, by = c("Type", "scenario", "population", "iso3c", "date_start")) %>% 
  mutate(tot_cost = vac_cost + deaths_management_cost + critical_care_cost + severe_care_cost + home_care_cost,
         tot_cost_novac = death_management_cost_novac + critical_care_cost_novac + severe_care_cost_novac + home_care_cost_novac,
         rr_cost = tot_cost/tot_cost_novac) -> cost_all

CJ(scenario_id = 1:36,
   iso3c = fitted_table$iso3c) %>% 
  left_join(members, by = "iso3c") %>% 
  dplyr::select(-name_official) %>%
  rename(population = name_internal) %>% 
  mutate(scenario_id = as.character(scenario_id)) %>% 
  left_join(cost_home_care_ext %>% dplyr::select(-name), by = c("scenario_id", "population")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_severe_care_ext, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_critical_care_ext, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_deaths_management_ext, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>%
  left_join(ms_scenarios, by = "scenario_id") %>% 
  left_join(cost_program_ext, by = c("Type", "scenario", "population", "iso3c", "date_start")) %>% 
  mutate(tot_cost = vac_cost + deaths_management_cost + critical_care_cost + severe_care_cost + home_care_cost,
         tot_cost_novac = death_management_cost_novac + critical_care_cost_novac + severe_care_cost_novac + home_care_cost_novac,
         rr_cost = tot_cost/tot_cost_novac) -> cost_all_ext

CJ(scenario_id = 1:36,
   iso3c = fitted_table$iso3c) %>% 
  left_join(members, by = "iso3c") %>% 
  dplyr::select(-name_official) %>%
  rename(population = name_internal) %>% 
  mutate(scenario_id = as.character(scenario_id)) %>% 
  left_join(cost_home_care_low %>% dplyr::select(-name), by = c("scenario_id", "population")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_severe_care_low, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_critical_care_low, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_deaths_management_low, by = c("scenario_id", "population", "Type")) %>% 
  dplyr::select(-name) %>%
  left_join(ms_scenarios, by = "scenario_id") %>% 
  left_join(cost_program_low, by = c("Type", "scenario", "population", "iso3c", "date_start")) %>% 
  mutate(tot_cost = vac_cost + deaths_management_cost + critical_care_cost + severe_care_cost + home_care_cost,
         tot_cost_novac = death_management_cost_novac + critical_care_cost_novac + severe_care_cost_novac + home_care_cost_novac,
         rr_cost = tot_cost/tot_cost_novac) -> cost_all_low


