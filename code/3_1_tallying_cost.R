discount_r <- 0.03

ms_cov_all <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/ms_cov_all.rds")

ms_scenarios <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/ms_scenarios.rds") %>% 
  rownames_to_column(var = "scenario_id")

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
         cov_achieved_diff2_dose1 = if_else(cov_achieved_diff1_dose1 + cov_achieved_diff2_dose1 > 0.6,
                                            0.6 - cov_achieved_diff1_dose1,
                                            cov_achieved_diff2_dose1),
         cov_achieved_diff2_dose2 = if_else(cov_achieved_diff1_dose2 + cov_achieved_diff2_dose2 > 0.6,
                                            0.6 - cov_achieved_diff1_dose2,
                                            cov_achieved_diff2_dose2),
         year1_doses = (cov_achieved_diff1_dose1 + cov_achieved_diff1_dose2)*tot*1000,
         year2_doses = (cov_achieved_diff2_dose1 + cov_achieved_diff2_dose2)*tot*1000,
         vac_unit_disc = vac_unit/((1+discount_r)),
         vac_cost = year1_doses*vac_unit + year2_doses*(vac_unit_disc)) %>%
  dplyr::select(date_start, scenario, Type, name, iso3c, r_vac, Rate, vac_cost) %>% 
  dplyr::select(date_start, Type, scenario, name, iso3c, vac_cost) %>% 
  rename(population = name) %>% 
  mutate(Type = tolower(Type)) -> cost_program

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

res$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "severe_i_all")  %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp* (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         severe_care_cost = if_else(year == 2021, value*8*hosp, value*8*hosp_disc),
         severe_care_cost_novac = if_else(year == 2021, novac*8*hosp, novac*8*hosp_disc)) %>% 
  group_by(scenario_id, population, name, Type) %>% 
  summarise(severe_care_cost = sum(severe_care_cost),
            severe_care_cost_novac = sum(severe_care_cost_novac)) -> cost_severe_care

 
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
                                      value*(icu)*10 + value*(hosp)*8,
                                      value*(icu_disc)*10 + value*(hosp_disc)*8),
         critical_care_cost_novac = if_else(year == 2021,
                                            novac*icu*10 + novac*hosp*8,
                                            novac*icu_disc*10 + novac*hosp_disc*8))  %>%
  # filter(population == "Algeria", scenario_id == 1) #%>% 
  # filter(Type == "az") %>% 
  # dplyr::select(-value) %>% pivot_wider(names_from = Type, values_from = novac)
  group_by(scenario_id, population, name, Type) %>% 
  summarise(critical_care_cost = sum(critical_care_cost),
            critical_care_cost_novac = sum(critical_care_cost_novac)) -> cost_critical_care

# res$pfizer$fatal %>% 
#   mutate(Type = "pfizer") %>% 
#   bind_rows(res$az$fatal %>% 
#               mutate(Type = "az")) %>% 
#   group_by(scenario_id, population, year, Type) %>% 
#   summarise(value = sum(value))

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


