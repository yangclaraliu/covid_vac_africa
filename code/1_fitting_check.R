read_rds("data/intermediate/fitted_parameters_4.rds") %>% 
  bind_rows() %>% 
  setNames(c("r","t0","rr","voc2")) %>% 
  bind_cols(stop_fitting %>% 
              dplyr::select(iso3c, index, t_start, date_min, loc)) %>% 
  rename(t_end_fitting = t_start) %>% 
  mutate(t_intro = ymd("2019-12-01") + t0,
         t_intro_voc1 = ymd("2021-01-15"),
         t_intro_voc2 = date_min + voc2,
         t_intro_voc3 = ymd("2021-12-15")) -> fitted_table

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
         rc_severity_3 = 1,
         
         rc_transmissibility_1 = 1, 
         rc_transmissibility_2 = 1.5, 
         rc_transmissibility_3 = 1,
         
         rc_ve_1 = 1, 
         rc_ve_2 = 0.8, 
         rc_ve_3 = 1,
         
         rc_transmissibility_1 = if_else(loc %in% c("Ethiopia", "Ghana", "Nigeria"),
                                         1.5,
                                         rc_transmissibility_1))

fitted_table[fitted_table$iso3c=="COD", "loc"] <- "Dem. Republic of the Congo"

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
  filter(deaths_daily_max > 10,
         deaths_daily_prop_max < 0.05) %>% 
  mutate(deaths = if_else(is.na(deaths), 0, deaths)) -> tmp

p <- list()
for(i in 1:nrow(fitted_table)){
  gen_country_basics(country = fitted_table$loc[i],
                     waning_nat = 52*7*3,
                     R0_assumed = fitted_table$r[i],
                     date_start = as.character(ymd("2019-12-01") + fitted_table$t0[i]),
                     processes = burden_processes,
                     deterministic = T) %>%
    update_vac_char(.,
                    ve_i   = ve$ve_i_o[1],  # infection blocking VE post 1 dose
                    v2e_i  = ve$ve_i_o[2],  # infection blocking VE post 2 doses
                    ve_d   = ve$ve_d[1],    # clinical fraction among breakthrough post 1 dose
                    v2e_d  = ve$ve_d[2],    # clinical fraction among breakthrough post 2 doses
                    wv = 1/360) %>% 
    change_VOC(.,
               date_switch = c(as.character(fitted_table$t_intro_voc1[i]), 
                               as.character(fitted_table$t_intro_voc2[i]),
                               as.character(fitted_table$t_intro_voc3[i])),
               rc_severity = c(fitted_table$rc_severity_1[i], 
                               fitted_table$rc_severity_2[i], 
                               fitted_table$rc_severity_3[i]), 
               rc_transmissibility = c(fitted_table$rc_transmissibility_1[i], 
                                       fitted_table$rc_transmissibility_2[i], 
                                       fitted_table$rc_transmissibility_3[i]), 
               rc_ve = c(fitted_table$rc_ve_1[i], 
                         fitted_table$rc_ve_2[i], 
                         fitted_table$rc_ve_3[i]) # relative in ve against infection
    ) %>% 
    cm_simulate() %>% 
    .[["dynamics"]] %>% 
    filter(grepl("death", compartment)) %>% 
    group_by(t, compartment) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    mutate(date = ymd("2019-12-01") + fitted_table$t0[i] + t) %>% 
    filter(date <= fitted_table$t_end_fitting[i]) %>% 
    pivot_wider(names_from = compartment, values_from = value) %>% 
    mutate(deaths_sim = case_when(date <= (ymd(fitted_table$t_intro_voc1[i])) ~ death_o,
                                  
                                  date > (ymd(fitted_table$t_intro_voc1[i])) & 
                                    date <=  (ymd(fitted_table$t_intro_voc2[i])) ~ death_voc1_o,
                                  
                                  date > (ymd(fitted_table$t_intro_voc2[i])) & 
                                    date <=  (ymd(fitted_table$t_intro_voc3[i])) ~ death_voc2_o,
                                  
                                  date > (ymd(fitted_table$t_intro_voc3[i])) ~ death_voc3_o),
           scaled = deaths_sim*fitted_table$rr[i],
           date = as.character(date)) %>% 
    left_join(tmp %>% 
                filter(iso3c == fitted_table$iso3c[i]) %>% 
                ungroup %>% 
                dplyr::select(date, deaths) %>% 
                mutate(date = as.character(date)), 
              by = "date") %>% 
    mutate(deaths = if_else(is.na(deaths), 0, deaths),
           date = ymd(date))  -> res
  
  
  res %>% 
    ggplot(., aes(x = date)) +
    geom_point(aes(y = scaled)) +
    geom_line(aes(y = deaths)) #-> p[[i]]
}


draw_fit(read_rds("data/intermediate/fitted_parameters_4.rds") %>% .[[1]], 1)
