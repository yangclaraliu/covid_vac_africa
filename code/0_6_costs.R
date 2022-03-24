readxl::read_excel(paste0(path_dropbox,
                          "Africa Vaccine Cost 16_03_22.xlsx"),
                   sheet = "Scenarios - Results") %>% 
  pivot_longer(cols = as.character(1:16),
               names_to = "Scenario") %>% 
  left_join(readxl::read_excel(paste0(path_dropbox,
                                      "Africa Vaccine Cost 16_03_22.xlsx"),
                               sheet = "Scenarios - Description") %>% 
              mutate(Scenario = as.character(Scenario)),
            by = "Scenario") -> cost_vaccines



readxl::read_excel(paste0(path_dropbox,
                          "COVID care unit cost - Africa.xlsx"),
                   sheet = 1) %>% 
  .[-c(1:4),c(1,11:14)] %>% 
  setNames(c("cn", "home", "hosp", "icu", "deaths")) -> cost_care

paste0(path_dropbox, "API_SH.XPD.CHEX.PC.CD_DS2_en_csv_v2_3753529.csv") %>% 
  readr::read_csv(.) %>% 
  janitor::remove_empty() %>% 
  rename(iso3c = `Country Code`) %>% 
  filter(iso3c %in% members$iso3c) %>% 
  dplyr::select(iso3c, `2019`) %>% 
  left_join(vac_denom, by = "iso3c") %>% 
  mutate(hc_expenditure = tot*1000*`2019`) -> cost_hc_expenditure

