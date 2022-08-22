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
  mutate(loc = countrycode(iso3c, "iso3c", "country.name")) %>% 
  ggplot(., aes(x = date, y = cov, group = status, color = status)) +
  geom_line() +
  facet_wrap(~loc) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_jama(labels = c("Continue with\nCurrent Efforts",
                              "Push for\nRoll-out Target")) +
  labs(x = "Date",
       y = "Vaccine Coverage",
       color = "")

ggsave("figs/policy_brief_fig0.png", width = 15, height = 10)

# #### build scenarios ####
# base <- list()
# base[["pfizer"]] <- build_base("pfizer")
# base[["az"]] <- build_base("az")
# 
# #### run scenarios ####
# compile_additional <- function(param_set,
#                         ms_tmp = NULL,
#                         sim_end = "2022-12-31",
#                         fn = NULL){
#   df <- list()
#   
#   for(j in 1:nrow(fitted_table)){
#     
#     df[[j]] <- list()
#     
#     date_switch <- c(fitted_table$t_intro_voc1[j],
#                      fitted_table$t_intro_voc2[j],
#                      fitted_table$t_intro_voc3[j])
#     
#     
#     date_tmp <- c(ms_tmp$date_ms1[j],
#                   ms_tmp$date_ms2[j],
#                   ms_tmp$date_ms3[j],
#                   ms_tmp$date_ms4[j])
#     
#     cov_tmp <- c(ms_tmp$cov_ms1[j],
#                  ms_tmp$cov_ms2[j],
#                  ms_tmp$cov_ms3[j],
#                  ms_tmp$cov_ms4[j]) 
#     
#     tmp <- bind_cols(date_tmp, cov_tmp) %>% 
#       distinct()
#     
#     date_tmp <- tmp$...1
#     cov_tmp <- tmp$...2
#     
#     vac_policy(param_set[[j]],
#                # these two parameters define the supply conditions
#                milestone_date = as.character(date_tmp), 
#                milestone_cov = cov_tmp,
#                # prioritisation, assume 60+  all prioritised
#                priority = c(NA, NA, NA, NA,
#                             2,  2,  2,  2,
#                             2,  2,  2,  2,
#                             1,  1,  1,  1),
#                # maximum feasible uptakes
#                cov_max = c(rep(0,4),
#                            rep(0.6, 8),
#                            rep(0.8, 4)),
#                supply_delay = 4, # unit = weeks
#                dose_interval = 4) -> tmp
#       
#       # tmp$scenarios[[1]]$daily_vac_scenarios %>%
#       #   mutate_at(vars(starts_with("Y")), cumsum) %>%
#       #   pivot_longer(starts_with("Y")) %>%
#       #   separate(name, into = c("ag","dose")) %>%
#       #   ggplot(., aes(x = date, y = value, group = dose, color = dose)) +
#       #   geom_line() +
#       #   facet_wrap(~ag)
#       # 
#       tmp %>% 
#         .$res %>% 
#         .[[1]] %>% 
#         cm_simulate %>% 
#         .[["dynamics"]] -> tmp
#       
#       tmp %>% 
#         filter(grepl("death|critical|severe|case", compartment)) %>% 
#         mutate(date = ymd("2019-12-01") + t + fitted_table$t0[j],
#                year = year(date)) %>% 
#         # summarise(value = sum(value), .groups = "drop") %>%
#         filter((compartment == "death_o") & date <= date_switch[1] |
#                  (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
#                  (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
#                  (compartment == "death_voc3_o") & date > date_switch[3] |
#                  
#                  (compartment == "severe_p") & date <= date_switch[1] |
#                  (compartment == "severe_voc1_p") & date > date_switch[1] & date <= date_switch[2] |
#                  (compartment == "severe_voc2_p") & date > date_switch[2] & date <= date_switch[3] |
#                  (compartment == "severe_voc3_p") & date > date_switch[3] |
#                  
#                  (compartment == "severe_i") & date <= date_switch[1] |
#                  (compartment == "severe_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
#                  (compartment == "severe_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
#                  (compartment == "severe_voc3_i") & date > date_switch[3] |
#                  
#                  (compartment == "critical_p") & date <= date_switch[1] |
#                  (compartment == "critical_voc1_p") & date > date_switch[1] & date <= date_switch[2] |
#                  (compartment == "critical_voc2_p") & date > date_switch[2] & date <= date_switch[3] |
#                  (compartment == "critical_voc3_p") & date > date_switch[3] |
#                  
#                  (compartment == "critical_i") & date <= date_switch[1] |
#                  (compartment == "critical_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
#                  (compartment == "critical_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
#                  (compartment == "critical_voc3_i") & date > date_switch[3] |
#                  compartment == "cases") -> res_tmp 
#       
#       res_tmp %>% 
#         pivot_wider(names_from = compartment,
#                     values_from = value) %>% 
#         replace(., is.na(.), 0) %>% 
#         mutate(severe_p_all = severe_p + severe_voc1_p + severe_voc2_p + severe_voc3_p,
#                severe_i_all = severe_i + severe_voc1_i + severe_voc2_i + severe_voc3_i,
#                critical_p_all = critical_p + critical_voc1_p + critical_voc2_p + critical_voc3_p,
#                critical_i_all = critical_i + critical_voc1_i + critical_voc2_i + critical_voc3_i,
#                death_o_all = death_o + death_voc1_o + death_voc2_o + death_voc3_o) %>% 
#         dplyr::select(population, date, group, year, cases, severe_p_all, severe_i_all, critical_p_all, critical_i_all, death_o_all) %>% 
#         pivot_longer(cols = c(cases, severe_p_all, severe_i_all, critical_p_all, critical_i_all, death_o_all)) %>% 
#         mutate(is_death = name == "death_o_all") %>% 
#         group_by(is_death) %>% group_split() -> res_tmp
#       
# 
#       res_tmp[[1]] %>% 
#         group_by(population, year, name) %>% 
#         summarise(value = sum(value)) -> df[[j]][["non_fatal"]]
#       
#       res_tmp[[2]] %>% 
#         group_by(population, year, name, group) %>% 
#         summarise(value = sum(value)) -> df[[j]][["fatal"]]
#       
#       print(paste0(j, "_",fitted_table$iso3c[j]))
#     
#     write_rds(df, paste0("data/intermediate/", fn, ".rds"))
#   }
# }
# 
# compile_additional(param_set = base$pfizer,
#                    ms_tmp = ms_who,
#                    fn = "who_pfizer")
# 
# compile_additional(param_set = base$az,
#                    ms_tmp = ms_who,
#                    fn = "who_az")
# 
# compile_additional(param_set = base$pfizer,
#                    ms_tmp = ms_current,
#                    fn = "current_pfizer")
# 
# compile_additional(param_set = base$az,
#                    ms_tmp = ms_current,
#                    fn = "current_az")

organise_outcomes_add <- function(tmp_pfizer,
                                  tmp_az){
  m <- list()
  
  m[["pfizer"]] <- list()
  
  m[["pfizer"]][["fatal"]] <- lapply(1:nrow(fitted_table), function(x) tmp_pfizer[[x]]$fatal)  %>%
    bind_rows() %>%
    filter(year > 2020)
  
  m[["pfizer"]][["non_fatal"]] <-  lapply(1:nrow(fitted_table), function(x) tmp_pfizer[[x]]$non_fatal)  %>%
    # map(bind_rows, .id = "scenario_id") %>% 
    bind_rows() %>%
    filter(year > 2020)
  
  m[["az"]] <- list()
  
  m[["az"]][["fatal"]] <-  lapply(1:nrow(fitted_table), function(x) tmp_az[[x]]$fatal)  %>%
    # map(bind_rows, .id = "scenario_id") %>% 
    bind_rows() %>%
    filter(year > 2020)
  
  m[["az"]][["non_fatal"]] <-  lapply(1:nrow(fitted_table), function(x) tmp_az[[x]]$non_fatal)  %>%
    # map(bind_rows, .id = "scenario_id") %>% 
    bind_rows() %>%
    filter(year > 2020)
  
  return(m)
}

get_fatal_add <- function(tmp){
  m <- list()
  
  tmp$pfizer$fatal %>% 
    mutate(Type = "pfizer") %>% 
    bind_rows(tmp$az$fatal %>% 
                mutate(Type = "az")) %>% 
    left_join(fitted_table[,c("loc", "iso3c")] %>% 
                rename(population = loc),
              by = "population"
    ) %>% 
    rename(# epi_id = scenario_id,
           country = iso3c,
           age = group,
           deaths = value) %>% 
    mutate(age = factor(age,
                        levels = unique(tmp$pfizer$fatal$group),
                        labels = 1:16),
           epi_id = 1,
           age = as.numeric(age)) %>% 
    ungroup -> m[["all"]]
  
  c(m, 
    m$all %>% 
      dplyr::select(epi_id, 
                    country, year, age, deaths, Type) %>% 
      group_by(Type) %>% group_split() %>% 
      setNames(c("az","pfizer")) %>% 
      map(select, -Type) %>% 
      map(data.table)) -> m
  
  m$all %>% 
    dplyr::select(epi_id, 
      country, year, age, novac) %>% 
    rename(deaths = novac) %>% 
    distinct() %>% 
    data.table() -> m[["novac"]]
  
  return(m)
}

get_non_fatal_add <- function(tmp){
  m <- list()
  
  tmp$pfizer$non_fatal %>% 
    mutate(Type = "pfizer") %>% 
    bind_rows(tmp$az$non_fatal %>% 
                mutate(Type = "az")) %>% 
    filter(!grepl("_p_", name)) %>% 
    dplyr::select(-novac) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    rename(# epi_id = scenario_id,
           country = iso3c,
           icu = critical_i_all,
           non_icu = severe_i_all) %>% 
    mutate(epi_id = 1) %>% 
    ungroup()  -> m[["all"]]
  
  c(m, 
    m$all %>% 
      dplyr::select(epi_id, 
        country, year, cases, non_icu, icu, Type) %>% 
      group_by(Type) %>% group_split() %>% 
      setNames(c("az","pfizer")) %>% 
      map(select, -Type)%>% 
      map(data.table)) -> m
  
  tmp$pfizer$non_fatal %>% 
    mutate(Type = "pfizer") %>% 
    bind_rows(tmp$az$non_fatal %>% 
                mutate(Type = "az")) %>% 
    filter(!grepl("_p_", name)) %>% 
    dplyr::select(-value, -Type) %>%
    distinct() %>% 
    pivot_wider(names_from = name, values_from = novac) %>% 
    rename(# epi_id = scenario_id,
           country = iso3c,
           icu = critical_i_all,
           non_icu = severe_i_all) %>% 
    ungroup() %>% 
    data.table() -> m[["novac"]]
  
  return(m)
}

current_az <- read_rds("data/intermediate/current_az.rds")
current_pfizer <- read_rds("data/intermediate/current_pfizer.rds")

who_az <- read_rds("data/intermediate/who_az.rds")
who_pfizer <- read_rds("data/intermediate/who_pfizer.rds")
res_novac <- read_rds("data/intermediate/res_novac.rds")

outcomes_who <- organise_outcomes_add(tmp_pfizer = who_pfizer, tmp_az = who_az)
outcomes_current <- organise_outcomes_add(tmp_pfizer = current_pfizer, tmp_az = current_az)

outcomes_who <- merge_novac(outcomes_who)
outcomes_current <- merge_novac(outcomes_current)

add_f <- list()
add_f[["who"]] <- get_fatal_add(outcomes_who)
add_f[["current"]] <- get_fatal_add(outcomes_current)

add_nf <- list()
add_nf[["who"]] <- get_non_fatal_add(outcomes_who)
add_nf[["current"]] <- get_non_fatal_add(outcomes_current)

impact_pb <- list()
labs_vac <- c("az", "pfizer", "novac")
# labs_ver <- c("bc", "ext","low")
labs_ver <- c("who", "current")
  
for(j in 1:length(labs_ver)){
  # for(j in 1){
  impact_pb[[labs_ver[j]]] <- list()
  # for(i in 1){
  for(i in 1:length(labs_vac)){
    impact_pb[[labs_ver[j]]][[labs_vac[i]]] <-
      cov_econ_outcomes(
        epi_deaths = add_f[[labs_ver[j]]][[labs_vac[i]]],
        epi_cases = add_nf[[labs_ver[j]]][[labs_vac[i]]] %>% mutate(epi_id = 1),
        econ_scens = econ_scens,
        LT = UNLT,
        POP = UNPOP,
        GDPPC = GDPPC,
        GNIPC = GNIPC
      )
  }
}

#### get program costs ####
ms_who %>% 
  mutate(Elapse1 = (as.numeric(date_ms2 - date_ms1))/30,
         Elapse2 = (as.numeric(date_ms3 - date_ms2))/30,
         Rate1 = 1000000*((cov_ms2 - cov_ms1)/as.numeric(date_ms2 - date_ms1)),
         Rate2 = 1000000*((cov_ms3 - cov_ms2)/as.numeric(date_ms3 - date_ms2))) -> pre_tab_who

predict(model_cost_vaccines,
        newdata = pre_tab_who %>% 
          rename(Elapse = Elapse1,
                 Rate = Rate1) %>% 
          mutate(Type = "AZ" ) %>% 
          .[,c("iso3c","Type","Elapse","Rate")]) -> pre_tab_who[,"vac_unit_cost_az1"]

predict(model_cost_vaccines,
        newdata = pre_tab_who %>% 
          rename(Elapse = Elapse2,
                 Rate = Rate2) %>% 
          mutate(Type = "AZ" ) %>% 
          .[,c("iso3c","Type","Elapse","Rate")]) -> pre_tab_who[,"vac_unit_cost_az2"]

predict(model_cost_vaccines,
        newdata = pre_tab_who %>% 
          rename(Elapse = Elapse1,
                 Rate = Rate1) %>% 
          mutate(Type = "Pfizer" ) %>% 
          .[,c("iso3c","Type","Elapse","Rate")]) -> pre_tab_who[,"vac_unit_cost_pf1"]

predict(model_cost_vaccines,
        newdata = pre_tab_who %>% 
          rename(Elapse = Elapse2,
                 Rate = Rate2) %>% 
          mutate(Type = "Pfizer" ) %>% 
          .[,c("iso3c","Type","Elapse","Rate")]) -> pre_tab_who[,"vac_unit_cost_pf2"]

pre_tab_who %>% 
  left_join(vac_denom, by = "iso3c") %>% 
  mutate(vac_cost_az = cov_ms2*tot*1000*vac_unit_cost_az1 + (cov_ms3 - cov_ms2)*tot*1000*vac_unit_cost_az2,
         vac_cost_pf = cov_ms2*tot*1000*vac_unit_cost_pf1 + (cov_ms3 - cov_ms2)*tot*1000*vac_unit_cost_pf2) %>% 
  dplyr::select(iso3c, vac_cost_az , vac_cost_pf) %>% 
  pivot_longer(cols = starts_with("vac"),
               names_to = "Type") %>% 
  mutate(Type = substr(Type, 10,11),
         Type = if_else(Type == "pf", "pfizer",Type)) -> cost_program_who_pb


ms_current %>% 
  mutate(Elapse1 = (as.numeric(date_ms2 - date_ms1))/30,
         Elapse2 = (as.numeric(date_ms3 - date_ms2))/30,
         Rate1 = 1000000*((cov_ms2 - cov_ms1)/as.numeric(date_ms2 - date_ms1)),
         Rate2 = 1000000*((cov_ms3 - cov_ms2)/as.numeric(date_ms3 - date_ms2))) -> pre_tab_current

predict(model_cost_vaccines,
        newdata = pre_tab_who %>% 
          rename(Elapse = Elapse1,
                 Rate = Rate1) %>% 
          mutate(Type = "AZ" ) %>% 
          .[,c("iso3c","Type","Elapse","Rate")]) -> pre_tab_current[,"vac_unit_cost_az1"]

predict(model_cost_vaccines,
        newdata = pre_tab_who %>% 
          rename(Elapse = Elapse2,
                 Rate = Rate2) %>% 
          mutate(Type = "AZ" ) %>% 
          .[,c("iso3c","Type","Elapse","Rate")]) -> pre_tab_current[,"vac_unit_cost_az2"]

predict(model_cost_vaccines,
        newdata = pre_tab_who %>% 
          rename(Elapse = Elapse1,
                 Rate = Rate1) %>% 
          mutate(Type = "Pfizer" ) %>% 
          .[,c("iso3c","Type","Elapse","Rate")]) -> pre_tab_current[,"vac_unit_cost_pf1"]

predict(model_cost_vaccines,
        newdata = pre_tab_who %>% 
          rename(Elapse = Elapse2,
                 Rate = Rate2) %>% 
          mutate(Type = "Pfizer" ) %>% 
          .[,c("iso3c","Type","Elapse","Rate")]) -> pre_tab_current[,"vac_unit_cost_pf2"]


pre_tab_current %>% 
  left_join(vac_denom, by = "iso3c") %>% 
  mutate(vac_cost_az = cov_ms2*tot*1000*vac_unit_cost_az1*2 + (cov_ms3 - cov_ms2)*tot*1000*vac_unit_cost_az2*2,
         vac_cost_pf = cov_ms2*tot*1000*vac_unit_cost_pf1*2 + (cov_ms3 - cov_ms2)*tot*1000*vac_unit_cost_pf2*2) %>% 
  dplyr::select(iso3c, vac_cost_az , vac_cost_pf) %>% 
  pivot_longer(cols = starts_with("vac"),
               names_to = "Type") %>% 
  mutate(Type = substr(Type, 10,11),
         Type = if_else(Type == "pf", "pfizer",Type)) -> cost_program_current_pb

#### get home based care costs ####
outcomes_who$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(outcomes_who$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "cases") %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(home = as.numeric(home),
         home = home*(108.596 / 107.303),
         home_disc = home/(1+discount_r),
         home_care_cost = if_else(year == 2021, value*0.1*home, value*0.1*home_disc),
         home_care_cost_novac = if_else(year == 2021, novac*0.1*home, novac*0.1*home_disc)) %>% 
  group_by(population, name, Type, iso3c) %>% 
  summarise(home_care_cost = sum(home_care_cost),
            home_care_cost_novac = sum(home_care_cost_novac)) -> cost_home_care_who_pb


outcomes_current$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(outcomes_current$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "cases") %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(home = as.numeric(home),
         home = home*(108.596 / 107.303),
         home_disc = home/(1+discount_r),
         home_care_cost = if_else(year == 2021, value*0.1*home, value*0.1*home_disc),
         home_care_cost_novac = if_else(year == 2021, novac*0.1*home, novac*0.1*home_disc)) %>% 
  group_by(population, name, Type, iso3c) %>% 
  summarise(home_care_cost = sum(home_care_cost),
            home_care_cost_novac = sum(home_care_cost_novac)) -> cost_home_care_current_pb

#### get severe care costs #### 
outcomes_who$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(outcomes_who$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "severe_i_all")  %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp* (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         severe_care_cost = if_else(year == 2021, value*9.6*hosp, value*9.6*hosp_disc),
         severe_care_cost_novac = if_else(year == 2021, novac*9.6*hosp, novac*9.6*hosp_disc)) %>% 
  group_by(population, name, Type, iso3c) %>% 
  summarise(severe_care_cost = sum(severe_care_cost),
            severe_care_cost_novac = sum(severe_care_cost_novac)) -> cost_severe_care_who_pb

outcomes_current$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(outcomes_current$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(name == "severe_i_all")  %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(hosp = as.numeric(hosp),
         hosp = hosp* (108.596 / 107.303),
         hosp_disc = hosp/(1+discount_r),
         severe_care_cost = if_else(year == 2021, value*9.6*hosp, value*9.6*hosp_disc),
         severe_care_cost_novac = if_else(year == 2021, novac*9.6*hosp, novac*9.6*hosp_disc)) %>% 
  group_by(population, name, Type, iso3c) %>% 
  summarise(severe_care_cost = sum(severe_care_cost),
            severe_care_cost_novac = sum(severe_care_cost_novac)) -> cost_severe_care_current_pb

#### get icu care costs ####

outcomes_who$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(outcomes_who$az$non_fatal %>% 
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
  group_by(population, name, Type, iso3c) %>% 
  summarise(critical_care_cost = sum(critical_care_cost),
            critical_care_cost_novac = sum(critical_care_cost_novac)) -> cost_critical_care_who_pb

outcomes_current$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(outcomes_current$az$non_fatal %>% 
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
  group_by(population, name, Type, iso3c) %>% 
  summarise(critical_care_cost = sum(critical_care_cost),
            critical_care_cost_novac = sum(critical_care_cost_novac)) -> cost_critical_care_current_pb

#### death case management costs ####
outcomes_who$pfizer$fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(outcomes_who$az$fatal %>% 
              mutate(Type = "az")) %>% 
  group_by(population, name, Type, year) %>% 
  summarise(value = sum(value),
            novac = sum(novac)) %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(deaths = as.numeric(deaths),
         deaths_disc = deaths/(1+discount_r),
         death_management_cost = if_else(year == 2021, (deaths)*value, value*deaths_disc),
         death_management_novac_cost = if_else(year == 2021, (deaths)*novac, novac*deaths_disc)) %>% 
  group_by(population, name, Type) %>% 
  summarise(deaths_management_cost = sum(death_management_cost),
            death_management_cost_novac = sum(death_management_novac_cost)) %>% 
  mutate(iso3c = countrycode(population, "country.name", "iso3c")) -> cost_deaths_management_who_pb

outcomes_current$pfizer$fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(outcomes_current$az$fatal %>% 
              mutate(Type = "az")) %>% 
  group_by(population, name, Type, year) %>% 
  summarise(value = sum(value),
            novac = sum(novac)) %>% 
  left_join(cost_care, by = c("population" = "cn")) %>% 
  mutate(deaths = as.numeric(deaths),
         deaths_disc = deaths/(1+discount_r),
         death_management_cost = if_else(year == 2021, (deaths)*value, value*deaths_disc),
         death_management_novac_cost = if_else(year == 2021, (deaths)*novac, novac*deaths_disc)) %>% 
  group_by(population, name, Type) %>% 
  summarise(deaths_management_cost = sum(death_management_cost),
            death_management_cost_novac = sum(death_management_novac_cost)) %>% 
  mutate(iso3c = countrycode(population, "country.name", "iso3c")) -> cost_deaths_management_current_pb

#### sum costs ####
cost_program_who_pb %>% 
  rename(vac_cost = value) %>% 
  left_join(cost_home_care_who_pb, by = c("iso3c", "Type")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_severe_care_who_pb, by = c("iso3c", "Type", "population")) %>% 
  dplyr::select(-name)  %>% 
  left_join(cost_critical_care_who_pb, by = c("iso3c", "Type", "population")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_deaths_management_who_pb, by = c("iso3c", "Type", "population")) %>% 
  dplyr::select(-name) %>% 
  mutate(tot_cost = vac_cost + deaths_management_cost + critical_care_cost + severe_care_cost + home_care_cost,
         tot_cost_novac = death_management_cost_novac + critical_care_cost_novac + severe_care_cost_novac + home_care_cost_novac,
         rr_cost = tot_cost/tot_cost_novac) -> cost_who_pb


cost_program_current_pb %>% 
  rename(vac_cost = value) %>% 
  left_join(cost_home_care_current_pb, by = c("iso3c", "Type")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_severe_care_current_pb, by = c("iso3c", "Type", "population")) %>% 
  dplyr::select(-name)  %>% 
  left_join(cost_critical_care_current_pb, by = c("iso3c", "Type", "population")) %>% 
  dplyr::select(-name) %>% 
  left_join(cost_deaths_management_current_pb, by = c("iso3c", "Type", "population")) %>% 
  dplyr::select(-name) %>% 
  mutate(tot_cost = vac_cost + deaths_management_cost + critical_care_cost + severe_care_cost + home_care_cost,
         tot_cost_novac = death_management_cost_novac + critical_care_cost_novac + severe_care_cost_novac + home_care_cost_novac,
         rr_cost = tot_cost/tot_cost_novac) -> cost_current_pb


#### merge impact with costs ####
impact_pb$who %>% 
  map(select, econ_id, country, dalys) %>% 
  bind_rows(.id = "Type") %>% 
  pivot_wider(names_from = Type, values_from = dalys) %>% 
  pivot_longer(cols = c("az","pfizer"),
               names_to = "Type",
               values_to = "dalys") %>% 
  rename(dalys_novac = novac,
         iso3c = country) %>% 
  left_join(cost_who_pb, by = c("iso3c","Type")) %>% 
  left_join(impact_pb$who$az %>% select(country, GDPPC_2020_USD) %>% distinct %>% rename(iso3c = country), 
            by = "iso3c") %>% 
  mutate(diff_health = dalys_novac - dalys,
         diff_cost = tot_cost- tot_cost_novac,
         ICER = diff_cost/diff_health,
         ICER_scaled = ICER/GDPPC_2020_USD,
         ICER_scaled_bin = if_else(ICER_scaled >= GDP_p, F, T)) -> ICER_who

impact_pb$current %>% 
  map(select, econ_id, country, dalys) %>% 
  bind_rows(.id = "Type") %>% 
  pivot_wider(names_from = Type, values_from = dalys) %>% 
  pivot_longer(cols = c("az","pfizer"),
               names_to = "Type",
               values_to = "dalys") %>% 
  rename(dalys_novac = novac,
         iso3c = country) %>% 
  left_join(cost_current_pb, by = c("iso3c","Type")) %>% 
  left_join(impact_pb$current$az %>% select(country, GDPPC_2020_USD) %>% distinct %>% rename(iso3c = country), 
            by = "iso3c") %>% 
  mutate(diff_health = dalys_novac - dalys,
         diff_cost = tot_cost- tot_cost_novac,
         ICER = diff_cost/diff_health,
         ICER_scaled = ICER/GDPPC_2020_USD,
         ICER_scaled_bin = if_else(ICER_scaled >= GDP_p, F, T)) -> ICER_current

head(ICER_current)

ICER_who %>% 
  filter(econ_id == 1) %>% 
  dplyr::select(iso3c, Type, ICER_scaled) %>% 
  rename(ICER_who = ICER_scaled) %>% 
  left_join(ICER_current %>% 
              filter(econ_id == 1) %>% 
              dplyr::select(iso3c, Type, ICER_scaled) %>% 
              rename(ICER_current = ICER_scaled),
            by = c("iso3c", "Type")) %>% 
  mutate(better = ICER_current < ICER_who,
         
         trans = (ICER_current < 0.5 & ICER_who > 0.5)|(ICER_current > 0.5 & ICER_who < 0.5)) %>% 
  filter(trans == T)
  mutate(CE_who = ICER_who < 0.5, CE_current = ICER_current < 0.5)  %>% group_by(Type) %>% summarise(who = sum(CE_who), current = sum(CE_current))
  
  pivot_longer(cols = starts_with("ICER", ignore.case = F)) %>% 
  mutate(name = gsub("ICER_","",name),
         Type = factor(Type, levels = c("az", "pfizer"),
                       labels = c("Viral Vector Vaccines",
                                  "mRNA Vaccines"))) %>%
  ggplot(., aes(x = name, y = value, group = interaction(iso3c, Type), color = trans)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Type) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  theme_bw() +
  theme(legend.position = "top") +
  custom_theme +
  labs(color = "Willingness-to-pay Threshold Cross-over",
       y = "Normalised ICER",
       x = "Roll-out Scenarios") +
  ggsci::scale_color_jama() +
  scale_x_discrete(breaks = c("current", "who"),
                   labels = c("Continue with\nCurrent Efforts",
                              "Push for\nRoll-out Target"))

ggsave("figs/policy_brief_fig1.png", width = 10, height = 5)

impact_pb$who %>% bind_rows(.id = "Type") %>% mutate(version = "who") %>% 
  bind_rows(impact_pb$current %>% bind_rows(.id = "Type") %>% mutate(version = "current")) %>% 
  dplyr::select(version, dalys, epi_id, econ_id, country, Type) %>%
  distinct() %>% 
  pivot_wider(names_from = version,
              values_from = dalys) %>% 
  filter(econ_id == 1) %>% 
  .[,3:6] %>% 
  group_by(Type) %>% 
  group_split() -> tmp

tmp[[1]] %>% 
  left_join(tmp[[2]] %>% 
              dplyr::select(-current, -Type) %>% 
              rename(novac = who),
            by = "country") %>% 
  bind_rows(tmp[[3]] %>% 
              left_join(tmp[[2]] %>% 
                          dplyr::select(-current, -Type) %>% 
                          rename(novac = who),
                        by = "country")) %>% 
  mutate(who_reduction = (novac - who)/novac,
         rr = who/current,
         current_reduction = (novac - current)/novac,
         Type = factor(Type,
                       levels = c("az","pfizer"),
                       labels = c("Viral Vector Vaccines",
                                  "mRNA Vaccines"))) %>% 
  # dplyr::select(-who, -current, -novac) %>%
  rename(iso3c = country) %>% 
  left_join(ms_current[,c("iso3c","cov_daily")], by = "iso3c") %>% 
  ggplot(., aes(x = current_reduction, y = who_reduction, fill = cov_daily)) +
  geom_point(aes(pch = Type), size = 4, alpha = 0.9) +
  scale_shape_manual(values = c(21,24))+
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  theme_bw() +
  viridis::scale_fill_viridis(option = "mako") +
  labs(shape = "Vaccine Type",
       fill = "Daily Vaccination Rate(%)",
       x = "Proportion DALYs Reduction incurred given current efforts",
       y = "Proportion DALYs Reduction incurred given coverage targets") +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm"),
        legend.box = "vertical") +
  custom_theme +
  guides(linetype = guide_legend(nrow = 2)) 

ggsave("figs/policy_brief_fig3.png", width = 10, height = 10)
