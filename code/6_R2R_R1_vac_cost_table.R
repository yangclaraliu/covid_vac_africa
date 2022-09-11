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
  # dplyr::select(date_start, scenario, Type, name, iso3c, r_vac, Rate, vac_cost) %>%
  # dplyr::select(date_start, Type, scenario, name, iso3c, vac_cost) %>%
  rename(population = name) %>%
  mutate(Type = tolower(Type),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast")),
         Type = factor(Type,
                       levels = c("az","pfizer"),
                       labels = c("Viral vector vaccines",
                                  "mRNA vaccines"))) %>%
  filter((date_start == "2021-08-01" & scenario == "medium") |
           date_start == "2021-08-01" & scenario == "fast") %>%
  group_by(date_start, scenario, Type) |> 
  summarise(mu = mean(vac_unit),
            md = median(vac_unit),
            Q1 = quantile(vac_unit, 0.25),
            Q3 = quantile(vac_unit, 0.75),
            LL = min(vac_unit),
            UL = max(vac_unit)) |> 
  arrange(Type, scenario) |> View()
  
