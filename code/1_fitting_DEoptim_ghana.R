stop_fitting %>% 
  filter(loc %in% c("Ghana")) %>% 
  mutate(voc1_LL = as.numeric(ymd("2020-12-01")-date_min),
         voc1_UL = as.numeric(ymd("2021-01-01")-date_min),
         voc2_LL = as.numeric(ymd("2021-06-01")-date_min),
         voc2_UL = as.numeric(ymd("2021-07-31")-date_min)) -> stop_fitting_ghana

fit_func_ghana <- function(input){
  
  tmp_epi <- tmp %>% 
    filter(iso3c == stop_fitting_ghana$iso3c[1]) %>% 
    ungroup %>% 
    dplyr::select(date, deaths, iso3c) %>% 
    mutate(date = as.character(date))
  
  suppressWarnings(
    gen_country_basics(country = stop_fitting_ghana$loc[1],
                       waning_nat = 52*7*3,
                       R0_assumed = as.numeric(input[1]),
                       date_start = as.character(ymd("2019-12-01") + input[2]),
                       date_end =   max(ymd(tmp_epi$date)),
                       processes = burden_processes,
                       deterministic = T) %>%
      update_vac_char(.,
                      ve_i   = ve$ve_i_o[1],  # infection blocking VE post 1 dose
                      v2e_i  = ve$ve_i_o[2],  # infection blocking VE post 2 doses
                      ve_d   = ve$ve_d[1],    # clinical fraction among breakthrough post 1 dose
                      v2e_d  = ve$ve_d[2],    # clinical fraction among breakthrough post 2 doses
                      wv = 1/360) %>% 
      change_VOC(.,
                 date_switch = c(as.character(ymd(stop_fitting_ghana$date_min[1]) + input[4]), 
                                 as.character(ymd(stop_fitting_ghana$date_min[1]) + input[5]), 
                                 "2021-12-15"),
                 rc_severity = c(1, 1.5, 1), # relative change in ihr and ifr
                 rc_transmissibility = c(1.5, 1.5, 1), # relative change in transmissibility via 
                 # u, uv and uv2
                 rc_ve = c(1, 0.8, 1) # relative in ve against infection
      ) %>% 
      cm_simulate() %>% 
      .[["dynamics"]] %>% 
      filter(grepl("death", compartment)) %>% 
      group_by(t, compartment) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      mutate(date = ymd("2019-12-01") + input[2] + t) %>% 
      filter(date <= stop_fitting_ghana$t_start[1]) %>%
      pivot_wider(names_from = compartment,
                  values_from = value) %>% 
      mutate(deaths_sim = case_when(date <= (ymd(stop_fitting_ghana$date_min[1]) + input[4]) ~ death_o,
                                    date > (ymd(stop_fitting_ghana$date_min[1]) + input[4]) & 
                                      date <=  (ymd(stop_fitting_ghana$date_min[1]) + input[5])~ death_voc1_o,
                                    date > (ymd(stop_fitting_ghana$date_min[1]) + input[5]) & date <= "2021-12-15" ~ death_voc2_o,
                                    date > "2021-12-15" ~ death_voc3_o),
             scaled = deaths_sim*as.numeric(input[3]),
             date = as.character(date)) %>% 
      left_join(tmp_epi, 
                by = "date") %>% 
      mutate(deaths = if_else(is.na(deaths), 0, deaths),
             deaths = round(deaths, 0),
             ll = dpois(deaths, scaled, log = T)) %>% 
      pull(ll) %>% sum -> a
  )
  
  return(-a)
  
}

DEoptim(fn = fit_func_ghana,
        lower = c(1, 0, 0.01, stop_fitting_ghana$voc1_LL[1], stop_fitting_ghana$voc2_LL[1]),
        upper = c(5, 396, 1, stop_fitting_ghana$voc1_UL[1], stop_fitting_ghana$voc2_UL[1]),
        control = controlDE) -> out_ghana

draw_fit_ghana <- function(opt, index = 1){
  suppressWarnings(
    gen_country_basics(country = stop_fitting_ghana$loc[index],
                       waning_nat = 52*7*3,
                       R0_assumed = as.numeric(opt[1]),
                       date_start = as.character(ymd("2019-12-01") + opt[2]),
                       processes = burden_processes,
                       deterministic = T) %>%
      update_vac_char(.,
                      ve_i   = ve$ve_i_o[1],  # infection blocking VE post 1 dose
                      v2e_i  = ve$ve_i_o[2],  # infection blocking VE post 2 doses
                      ve_d   = ve$ve_d[1],    # clinical fraction among breakthrough post 1 dose
                      v2e_d  = ve$ve_d[2],    # clinical fraction among breakthrough post 2 doses
                      wv = 1/360) %>% 
      change_VOC(.,
                 date_switch = c(as.character(ymd(stop_fitting_ghana$date_min[index]) + opt[4]), 
                                 as.character(ymd(stop_fitting_ghana$date_min[index]) + opt[5]), 
                                 "2021-12-15"),
                 rc_severity = c(1, 1.5, 1), # relative change in ihr and ifr
                 rc_transmissibility = c(1.5, 1.5, 1), # relative change in transmissibility via 
                 # u, uv and uv2
                 rc_ve = c(1, 0.8, 1) # relative in ve against infection
      ) %>% 
      cm_simulate() %>% 
      .[["dynamics"]] %>% 
      filter(grepl("death", compartment)) %>% 
      group_by(t, compartment) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      mutate(date = ymd("2019-12-01") + opt[2] + t) %>% 
      filter(date <= stop_fitting_ghana$t_start[index]) %>%
      pivot_wider(names_from = compartment, values_from = value) %>% 
      mutate(deaths_sim = case_when(date <= (ymd(stop_fitting_ghana$date_min[index]) + opt[4]) ~ death_o,
                                    date > (ymd(stop_fitting_ghana$date_min[index]) + opt[4]) & 
                                      date <=  (ymd(stop_fitting_ghana$date_min[index]) + opt[5])~ death_voc1_o,
                                    date > (ymd(stop_fitting_ghana$date_min[index]) + opt[5]) & date <= "2021-12-15" ~ death_voc2_o,
                                    date > "2021-12-15" ~ death_voc3_o),
             scaled = deaths_sim*as.numeric(opt[3]),
             date = as.character(date)) %>% 
      left_join(tmp %>% 
                  filter(iso3c == stop_fitting_ghana$iso3c[index],
                         date <= stop_fitting_ghana$t_start[index]) %>% 
                  ungroup %>% 
                  dplyr::select(date, deaths) %>% 
                  mutate(date = as.character(date)), 
                by = "date") %>% 
      mutate(deaths = if_else(is.na(deaths), 0, deaths),
             date = ymd(date)) -> res
  )
  
  res %>% 
    ggplot(., aes(x = date)) +
    geom_point(aes(y = scaled)) +
    geom_line(aes(y = deaths))
}


p <- draw_fit_ghana(opt = out_ghana$optim$bestmem)

write_rds(file = "data/intermediate/fitted_parameters_5_ghana.rds",
          x = out_ghana$optim$bestmem)
