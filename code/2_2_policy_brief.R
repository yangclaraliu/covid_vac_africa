#### sub analysis on "what could have happened?" ####
owid_vac %>% 
  dplyr::select(iso3c, date, people_fully_vaccinated) %>% 
  filter(!is.na(people_fully_vaccinated)) %>% 
  left_join(pop %>% 
              group_by(iso3c) %>% 
              summarise(tot = (sum(f) + sum(m))*1000),
            by = "iso3c") %>% 
  mutate(cov = people_fully_vaccinated/tot) %>%
  filter(cov >= 0) %>% 
  group_by(iso3c) %>% 
  mutate(date_min = min(date)) %>%
  filter(date == date_min) %>% 
  right_join(fitted_table[,c("iso3c")], by = "iso3c") %>% 
  select(iso3c, date) %>% 
  rename(date_ms1 = date) %>% 
  mutate(date_ms1 = if_else(is.na(date_ms1), 
                            ymd("2021-12-28"), 
                            date_ms1)) -> step1 


owid_vac %>% 
  dplyr::select(iso3c, date, people_fully_vaccinated) %>% 
  filter(!is.na(people_fully_vaccinated)) %>% 
  left_join(pop %>% 
              group_by(iso3c) %>% 
              summarise(tot = (sum(f) + sum(m))*1000),
            by = "iso3c") %>% 
  mutate(cov = people_fully_vaccinated/tot) %>%
  filter(cov >= 0.1) %>% 
  group_by(iso3c) %>% 
  mutate(date_min = min(date)) %>% 
  filter(date_min == date) %>% 
  filter(date_min < ymd("2021-10-01"))

53

owid_vac %>% 
  dplyr::select(iso3c, date, people_fully_vaccinated) %>% 
  filter(!is.na(people_fully_vaccinated)) %>% 
  left_join(pop %>% 
              group_by(iso3c) %>% 
              summarise(tot = (sum(f) + sum(m))*1000),
            by = "iso3c") %>% 
  mutate(cov = people_fully_vaccinated/tot) %>% 
  filter(date <= ymd("2022-01-01")) %>% 
  group_by(iso3c) %>% 
  mutate(date_max = max(date)) %>% 
  filter(date == date_max) %>% 
  right_join(fitted_table[,c("iso3c")], by = "iso3c") %>% 
  select(iso3c, date, cov) %>% 
  rename(date_ms2 = date,
         cov_ms2 = cov) -> step2

step1 %>% 
  left_join(step2, by = "iso3c") %>% 
  mutate(date_ms3 = ymd("2022-06-30"),
         date_ms4 = ymd("2022-12-31"),
         cov_ms1 = 0,
         cov_ms3 = 0.7,
         cov_ms4 = 0.7) -> ms_who

step1 %>% 
  left_join(step2, by = "iso3c") %>% 
  mutate(date_diff = as.numeric(abs(date_ms1 - date_ms2)),
         cov_daily = cov_ms2/date_diff,
         date_ms3 = ymd("2022-12-31"),
         date_ms4 = ymd("2022-12-31"),
         date_diff2 = as.numeric(abs(date_ms3 - date_ms1)),
         cov_ms3 = cov_daily*date_diff2,
         sat = cov_ms3 > 0.7,
         cov_ms3 = if_else(cov_ms3 > 0.7, 0.7, cov_ms3),
         date_ms3 = if_else(sat == T, 
                            date_ms1 + 0.7/cov_daily,
                            date_ms3),
         cov_ms4 = if_else(sat == T, 0.7, as.numeric(NA)),
         cov_ms1 = 0) %>% 
  mutate(cov_ms4 = if_else(sat == F, cov_ms3, cov_ms4)) -> ms_current

ms_who %>% 
  select(starts_with("cov")) %>% 
  pivot_longer(starts_with("cov")) %>% 
  mutate(ms = substr(name, 7,7)) %>% 
  rename(cov = value) %>% 
  dplyr::select(-name) %>% 
  left_join(ms_who %>% 
              select(starts_with("date")) %>% 
              pivot_longer(starts_with("date")) %>% 
              mutate(ms = substr(name, 8,8)) %>% 
              rename(date = value) %>% 
              dplyr::select(-name),
            by = c("iso3c", "ms")) %>% 
  mutate(status = "goal") -> tab1

ms_current %>% 
  select(starts_with("cov_ms")) %>% 
  pivot_longer(starts_with("cov_ms")) %>% 
  mutate(ms = substr(name, 7,7)) %>% 
  rename(cov = value) %>% 
  dplyr::select(-name) %>% 
  left_join(ms_current %>% 
              select(starts_with("date_ms")) %>% 
              pivot_longer(starts_with("date_ms")) %>% 
              mutate(ms = substr(name, 8,8)) %>% 
              rename(date = value) %>% 
              dplyr::select(-name),
            by = c("iso3c", "ms")) %>% 
  mutate(status = "current") -> tab2

tab1 %>% 
  bind_rows(tab2) %>% 
  ggplot(., aes(x = date, y = cov, group = status, color = status)) + 
  geom_line() +
  facet_wrap(~iso3c)

#### build scenarios ####
base <- list()
base[["pfizer"]] <- build_base("pfizer")
base[["az"]] <- build_base("az")

#### run scenarios ####
compile_additional <- function(param_set,
                        ms_tmp = NULL,
                        sim_end = "2022-12-31",
                        fn = NULL){
  df <- list()
  
  for(j in 1:nrow(fitted_table)){
    
    df[[j]] <- list()
    
    date_switch <- c(fitted_table$t_intro_voc1[j],
                     fitted_table$t_intro_voc2[j],
                     fitted_table$t_intro_voc3[j])
    
    
    date_tmp <- c(ms_tmp$date_ms1[j],
                  ms_tmp$date_ms2[j],
                  ms_tmp$date_ms3[j],
                  ms_tmp$date_ms4[j])
    
    cov_tmp <- c(ms_tmp$cov_ms1[j],
                 ms_tmp$cov_ms2[j],
                 ms_tmp$cov_ms3[j],
                 ms_tmp$cov_ms4[j]) 
    
    tmp <- bind_cols(date_tmp, cov_tmp) %>% 
      distinct()
    
    date_tmp <- tmp$...1
    cov_tmp <- tmp$...2
    
    vac_policy(param_set[[j]],
               # these two parameters define the supply conditions
               milestone_date = as.character(date_tmp), 
               milestone_cov = cov_tmp,
               # prioritisation, assume 60+  all prioritised
               priority = c(NA, NA, NA, NA,
                            2,  2,  2,  2,
                            2,  2,  2,  2,
                            1,  1,  1,  1),
               # maximum feasible uptakes
               cov_max = c(rep(0,4),
                           rep(0.6, 8),
                           rep(0.8, 4)),
               supply_delay = 4, # unit = weeks
               dose_interval = 4) -> tmp
      
      # tmp$scenarios[[1]]$daily_vac_scenarios %>%
      #   mutate_at(vars(starts_with("Y")), cumsum) %>%
      #   pivot_longer(starts_with("Y")) %>%
      #   separate(name, into = c("ag","dose")) %>%
      #   ggplot(., aes(x = date, y = value, group = dose, color = dose)) +
      #   geom_line() +
      #   facet_wrap(~ag)
      # 
      tmp %>% 
        .$res %>% 
        .[[1]] %>% 
        cm_simulate %>% 
        .[["dynamics"]] -> tmp
      
      tmp %>% 
        filter(grepl("death|critical|severe|case", compartment)) %>% 
        mutate(date = ymd("2019-12-01") + t + fitted_table$t0[j],
               year = year(date)) %>% 
        # summarise(value = sum(value), .groups = "drop") %>%
        filter((compartment == "death_o") & date <= date_switch[1] |
                 (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
                 (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
                 (compartment == "death_voc3_o") & date > date_switch[3] |
                 
                 (compartment == "severe_p") & date <= date_switch[1] |
                 (compartment == "severe_voc1_p") & date > date_switch[1] & date <= date_switch[2] |
                 (compartment == "severe_voc2_p") & date > date_switch[2] & date <= date_switch[3] |
                 (compartment == "severe_voc3_p") & date > date_switch[3] |
                 
                 (compartment == "severe_i") & date <= date_switch[1] |
                 (compartment == "severe_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
                 (compartment == "severe_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
                 (compartment == "severe_voc3_i") & date > date_switch[3] |
                 
                 (compartment == "critical_p") & date <= date_switch[1] |
                 (compartment == "critical_voc1_p") & date > date_switch[1] & date <= date_switch[2] |
                 (compartment == "critical_voc2_p") & date > date_switch[2] & date <= date_switch[3] |
                 (compartment == "critical_voc3_p") & date > date_switch[3] |
                 
                 (compartment == "critical_i") & date <= date_switch[1] |
                 (compartment == "critical_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
                 (compartment == "critical_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
                 (compartment == "critical_voc3_i") & date > date_switch[3] |
                 compartment == "cases") -> res_tmp 
      
      res_tmp %>% 
        pivot_wider(names_from = compartment,
                    values_from = value) %>% 
        replace(., is.na(.), 0) %>% 
        mutate(severe_p_all = severe_p + severe_voc1_p + severe_voc2_p + severe_voc3_p,
               severe_i_all = severe_i + severe_voc1_i + severe_voc2_i + severe_voc3_i,
               critical_p_all = critical_p + critical_voc1_p + critical_voc2_p + critical_voc3_p,
               critical_i_all = critical_i + critical_voc1_i + critical_voc2_i + critical_voc3_i,
               death_o_all = death_o + death_voc1_o + death_voc2_o + death_voc3_o) %>% 
        dplyr::select(population, date, group, year, cases, severe_p_all, severe_i_all, critical_p_all, critical_i_all, death_o_all) %>% 
        pivot_longer(cols = c(cases, severe_p_all, severe_i_all, critical_p_all, critical_i_all, death_o_all)) %>% 
        mutate(is_death = name == "death_o_all") %>% 
        group_by(is_death) %>% group_split() -> res_tmp
      

      res_tmp[[1]] %>% 
        group_by(population, year, name) %>% 
        summarise(value = sum(value)) -> df[[j]][["non_fatal"]]
      
      res_tmp[[2]] %>% 
        group_by(population, year, name, group) %>% 
        summarise(value = sum(value)) -> df[[j]][["fatal"]]
      
      print(paste0(j, "_",fitted_table$iso3c[j]))
    
    write_rds(df, paste0("data/intermediate/", fn, ".rds"))
  }
}

compile_additional(param_set = base$pfizer,
                   ms_tmp = ms_who,
                   fn = "who_pfizer")

compile_additional(param_set = base$az,
                   ms_tmp = ms_who,
                   fn = "who_az")

compile_additional(param_set = base$pfizer,
                   ms_tmp = ms_current,
                   fn = "current_pfizer")

compile_additional(param_set = base$az,
                   ms_tmp = ms_current,
                   fn = "current_az")
