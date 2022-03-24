require(DEoptim)

# VOC_start <- "2021-06-15"

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

owid_vac %>% 
  filter(!is.na(total_vaccinations)) %>% 
  left_join(pop %>% 
              mutate(tot = f+m) %>% 
              group_by(iso3c) %>% 
              summarise(tot = sum(tot)*1000),
            by = "iso3c") %>% 
  mutate(cov = total_vaccinations/(2*tot)) %>%
  filter(iso3c %in% tmp$iso3c) -> tmp_vac

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

stop_fitting[stop_fitting$iso3c=="COD", "loc"] <- "Dem. Republic of the Congo"

fit_func <- function(input){
  
  tmp_epi <- tmp %>% 
    filter(iso3c == stop_fitting$iso3c[index]) %>% 
    ungroup %>% 
    dplyr::select(date, deaths, iso3c) %>% 
    mutate(date = as.character(date))

  suppressWarnings(
    gen_country_basics(country = stop_fitting$loc[index],
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
                 date_switch = c("2021-01-15", 
                                 as.character(ymd(stop_fitting$date_min[index]) + input[4]), 
                                 "2021-12-15"),
                 rc_severity = c(1, 1.5, 1), # relative change in ihr and ifr
                 rc_transmissibility = c(1, 1.5, 1), # relative change in transmissibility via 
                 # u, uv and uv2
                 rc_ve = c(1, 0.8, 1) # relative in ve against infection
                 ) %>% 
      cm_simulate() %>% 
      .[["dynamics"]] %>% 
      filter(grepl("death", compartment)) %>% 
      group_by(t, compartment) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      mutate(date = ymd("2019-12-01") + input[2] + t) %>% 
      filter(date <= stop_fitting$t_start[index]) %>%
      pivot_wider(names_from = compartment,
                  values_from = value) %>% 
      mutate(deaths_sim = case_when(date <= "2021-01-15" ~ death_o,
                                    date > "2021-01-15" & date <= (ymd(stop_fitting$date_min[index]) + input[4]) ~ death_voc1_o,
                                    date > (ymd(stop_fitting$date_min[index]) + input[4]) & date <= "2021-12-15" ~ death_voc2_o,
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

draw_fit <- function(opt, index){
  suppressWarnings(
    gen_country_basics(country = stop_fitting$loc[index],
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
                 date_switch = c("2021-01-15", 
                                 as.character(ymd(stop_fitting$date_min[index]) + opt[4]), 
                                 "2021-12-15"),
                 rc_severity = c(1, 1.5, 1), # relative change in ihr and ifr
                 rc_transmissibility = c(1, 1.5, 1), # relative change in transmissibility via 
                 # u, uv and uv2
                 rc_ve = c(1, 0.8, 1) # relative in ve against infection
                 ) %>% 
      cm_simulate() %>% 
      .[["dynamics"]] %>% 
      filter(grepl("death", compartment)) %>% 
      group_by(t, compartment) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      mutate(date = ymd("2019-12-01") + opt[2] + t) %>% 
      filter(date <= stop_fitting$t_start[index]) %>%
      pivot_wider(names_from = compartment, values_from = value) %>% 
      mutate(deaths_sim = case_when(date <= "2021-01-15" ~ death_o,
                                    date > "2021-01-15" & date <= (ymd(stop_fitting$date_min[index]) + opt[4]) ~ death_voc1_o,
                                    date > (ymd(stop_fitting$date_min[index]) + opt[4]) & date <= "2021-12-15" ~ death_voc2_o,
                                    date > "2021-12-15" ~ death_voc3_o),
             scaled = deaths_sim*as.numeric(opt[3]),
             date = as.character(date)) %>% 
      left_join(tmp %>% 
                  filter(iso3c == stop_fitting$iso3c[index],
                         date <= stop_fitting$t_start[index]) %>% 
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

controlDE <- list(reltol=1e-8, steptol=20, itermax = 400, trace = 10,
                  parallelType = 2)

fitted_params <- list()
for(index in 2:nrow(stop_fitting)){
  print(stop_fitting$loc[index])
  DEoptim(fn = fit_func,
          # lower = c(1, stop_fitting$fw_LL[index], 0.01),
          # upper = c(5, stop_fitting$fw_UL[index], 1),
          lower = c(1, 0, 0.01, stop_fitting$vocw_LL[index]),
          upper = c(5, 396, 1, stop_fitting$vocw_UL[index]),
          control = controlDE) -> out

  input_tmp <- fitted_params[[index]] <- out$optim$bestmem

  p_tmp <- draw_fit(input_tmp,
                    index)

  write_rds(file = "data/intermediate/fitted_parameters_4.rds",
            x = fitted_params)

  fn_tmp <- paste0("figs/intermediate/fitting_20220313/",
                   stop_fitting$index[index],"_",stop_fitting$loc[index],
                   ".png")
  
  ggsave(fn_tmp, p_tmp)
  rm(index, out, input_tmp)
}

draw_fit(read_rds("data/intermediate/fitted_parameters_4.rds") %>% .[[1]], 1)

