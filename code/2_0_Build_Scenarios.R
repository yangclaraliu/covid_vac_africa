#### base parameters ####
build_base <- function(brand = NULL,
                       low = F,
                       sim_end = "2022-12-31"){
  
  tmp <- list()
  
  if(brand == "pfizer" & low == F)  {
    ve_tmp <- ve_pfizer
    processes_tmp <- burden_processes_pfizer
  }
  
  if(brand == "az" & low == F)  {
    ve_tmp <- ve_az
    processes_tmp <- burden_processes_az
  }
  
  if(brand == "pfizer" & low == T)  {
    ve_tmp <- ve_pfizer_low
    processes_tmp <- burden_processes_pfizer_low
  }
  
  if(brand == "az" & low == T)  {
    ve_tmp <- ve_az_low
    processes_tmp <- burden_processes_az_low
  }
  
  for(i in 1:nrow(fitted_table)){
    tmp[[i]] <- 
      gen_country_basics(country = fitted_table$loc[i],
                         waning_nat = 52*7*3,
                         R0_assumed  = fitted_table$r[i],
                         date_start = as.character(ymd("2019-12-01") + fitted_table$t0[i]),
                         date_end = sim_end,
                         processes = processes_tmp,
                         deterministic = TRUE) %>%
      update_vac_char(.,
                      ve_i   = ve_tmp$ve_i_o[1],  # infection blocking VE post 1 dose
                      v2e_i  = ve_tmp$ve_i_o[2],  # infection blocking VE post 2 doses
                      ve_d   = ve_tmp$ve_d[1],    # clinical fraction among breakthrough post 1 dose
                      v2e_d  = ve_tmp$ve_d[2],    # clinical fraction among breakthrough post 2 doses
                      wv = 1/360) %>% 
      change_VOC(., 
                 date_switch = c(fitted_table$t_intro_voc1[i], 
                                 fitted_table$t_intro_voc2[i], 
                                 fitted_table$t_intro_voc3[i]) %>% 
                   as.character,
                 rc_severity = c(fitted_table$rc_severity_1[i], 
                                 fitted_table$rc_severity_2[i],
                                 fitted_table$rc_severity_3[i]), 
                 rc_transmissibility = c(fitted_table$rc_transmissibility_1[i], 
                                         fitted_table$rc_transmissibility_2[i], 
                                         fitted_table$rc_transmissibility_3[i]), 
                 rc_ve = c(fitted_table$rc_ve_1[i], 
                           fitted_table$rc_ve_2[i], 
                           fitted_table$rc_ve_3[i]),
                 VE = ve_tmp) 
    
  }
  return(tmp)
}

base <- list()
base[["pfizer"]] <- build_base("pfizer")
base[["az"]] <- build_base("az")
# base[["pfizer_ext"]] <- build_base("pfizer", sim_end = "2023-06-30")
# base[["az_ext"]] <- build_base("az", sim_end = "2023-06-30")

# base_low <- list()
# base_low[["pfizer"]] <- build_base("pfizer", low = T)
# base_low[["az"]] <- build_base("az", low = T)
# base[["pfizer_ext"]] <- build_base("pfizer", sim_end = "2023-06-30")
# base[["az_ext"]] <- build_base("az", sim_end = "2023-06-30")

compile_base <- function(param_set){
  param_set %>% map(cm_simulate) %>% map(~.[["dynamics"]]) %>% 
    setNames(fitted_table$t0) %>% 
    bind_rows(.id = "t0") %>% 
    mutate(t0 = as.numeric(t0),
           date = ymd("2019-12-01") + t + t0) %>% 
    filter(grepl("case|critical|severe|death", compartment)) %>% 
    pivot_wider(names_from = compartment,
                values_from = value) %>% 
    left_join(fitted_table %>% 
                mutate_at(vars(starts_with("t_intro_voc")), ymd), 
              by = c("population" = "loc")) %>% 
    mutate(severe_p_all = case_when(
      date <= t_intro_voc1 ~ severe_p,
      date > t_intro_voc1 & date <= t_intro_voc2 ~ severe_voc1_p,
      date > t_intro_voc2 & date <= t_intro_voc3 ~ severe_voc2_p,
      date > t_intro_voc3 ~ severe_voc3_p
    ),
    severe_i_all = case_when(
      date <= t_intro_voc1 ~ severe_i,
      date > t_intro_voc1 & date <= t_intro_voc2 ~ severe_voc1_i,
      date > t_intro_voc2 & date <= t_intro_voc3 ~ severe_voc2_i,
      date > t_intro_voc3 ~ severe_voc3_i
    ),
    
    critical_p_all = case_when(
      date <= t_intro_voc1 ~ critical_p,
      date > t_intro_voc1 & date <= t_intro_voc2 ~ critical_voc1_p,
      date > t_intro_voc2 & date <= t_intro_voc3 ~ critical_voc2_p,
      date > t_intro_voc3 ~ critical_voc3_p
    ),
    critical_i_all = case_when(
      date <= t_intro_voc1 ~ critical_i,
      date > t_intro_voc1 & date <= t_intro_voc2 ~ critical_voc1_i,
      date > t_intro_voc2 & date <= t_intro_voc3 ~ critical_voc2_i,
      date > t_intro_voc3 ~ critical_voc3_i
    ),
    
    death_o_all = case_when(
      date <= t_intro_voc1 ~ death_o,
      date > t_intro_voc1 & date <= t_intro_voc2 ~ death_voc1_o,
      date > t_intro_voc2 & date <= t_intro_voc3 ~ death_voc2_o,
      date > t_intro_voc3 ~ death_voc3_o
    )) %>% 
    dplyr::select(population, iso3c, date, group, cases, severe_p_all, severe_i_all, critical_p_all, critical_i_all, death_o_all) %>% 
    pivot_longer(cols = c(cases, severe_p_all, severe_i_all, critical_p_all, critical_i_all, death_o_all)) %>% 
    mutate(is_death = name == "death_o_all",
           year = year(date)) %>% 
    group_by(is_death) %>% group_split() -> tmp
  
  res_tmp <- list()
  
  res_tmp[[1]] <- tmp[[1]] %>% 
    group_by(population, iso3c, year, name, group) %>% 
    summarise(value = sum(value))
  
  res_tmp[[2]] <- tmp[[2]] %>% 
    group_by(population, iso3c, year, name, group) %>% 
    summarise(novac = sum(value))
  
  return(res_tmp)
}

res_novac <- list()
res_novac[["pfizer"]] <- compile_base(base$pfizer)
res_novac[["az"]] <- compile_base(base$az)
# res_novac[["pfizer_ext"]] <- compile_base(base$pfizer_ext)
# res_novac[["az_ext"]] <- compile_base(base$az_ext)

write_rds(res_novac, "data/intermediate/res_novac_grouped.rds")

# res_novac <- list()
# for(j in 1:length(params_step1)){
#  cm_simulate(params_step1[[j]]) %>% 
#     .[["dynamics"]] -> res_novac[[j]]
# }

ms_cov_all %>% 
  dplyr::select(date_start, scenario, date_vac_end, date_vac_end_ext, cov, cov_ext) %>% 
  distinct() -> ms_scenarios

compile_strategy <- function(param_set,
                             sim_end = "2022-12-31",
                             fn = NULL){
  df <- list()
  
  for(j in 1:nrow(fitted_table)){
    df[[j]] <- list()
    date_switch <- c(fitted_table$t_intro_voc1[j],
                     fitted_table$t_intro_voc2[j],
                     fitted_table$t_intro_voc3[j])

    for(i in 1:nrow(ms_scenarios)){
      date_tmp <- c(ms_scenarios$date_start[i],
                    ms_scenarios$date_vac_end[i])
      if(date_tmp[length(date_tmp)] != sim_end){
        date_tmp <- c(date_tmp, sim_end)
      }
      
      cov_tmp <- c(0, ms_scenarios$cov[i])
      if(length(cov_tmp) < length(date_tmp)){
        cov_tmp <- c(cov_tmp, 0.7)
      }
      
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
      
      df[[j]][[i]] <- list()
      res_tmp[[1]] %>% 
        group_by(population, year, name, group) %>% 
        summarise(value = sum(value)) -> df[[j]][[i]][["non_fatal"]]
      
      res_tmp[[2]] %>% 
        group_by(population, year, name, group) %>% 
        summarise(value = sum(value)) -> df[[j]][[i]][["fatal"]]
      
      print(paste0(fitted_table$iso3c[j], ", ", i))
    }
    write_rds(df, paste0("data/intermediate/", fn, "_grouped.rds"))
  }
}

compile_strategy(base$pfizer, fn = "params_grid_pfizer_70")
compile_strategy(base$az, fn = "params_grid_az_70")

# compile_strategy(base$pfizer_ext, fn = "params_grid_pfizer_70_ext")
# compile_strategy(base$az_ext, fn = "params_grid_az_70_ext")

# compile_strategy(base_low$pfizer, fn = "params_grid_pfizer_70_low")
# compile_strategy(base_low$az, fn = "params_grid_az_70_low")

# res %>% 
#   map(bind_rows, .id = "ms_cov_index") %>% 
#   bind_rows() %>% 
#   mutate(tag = substr(compartment, 1, 4)) %>% 
#   filter(date >= ymd("2021-01-01")) %>% 
#   group_by(ms_cov_index, population, tag) %>% 
#   summarise(value = sum(value), .groups = "drop") %>% 
#   mutate(iso3c = countrycode(population, "country.name", "iso3c"))-> tmp
# 
# tmp %>% 
#   left_join(seg2%>% 
#               mutate(tag = substr(compartment, 1, 4)) %>% 
#               group_by(population, iso3c, tag) %>% 
#               summarise(novac  = sum(novac), .groups = "drop"),
#             by = c("iso3c", "tag", "population")) %>% 
#   left_join(ms_cov_all %>% 
#               rownames_to_column(var = "ms_cov_index"),
#             by = "ms_cov_index") %>% 
#   left_join(seg3, by = c("iso3c", "speed", "start_vac", "ms_cov")) %>% 
#   mutate(effect_relative = (1 - value/novac)*100,
#          effect_relative_adj = effect_relative*1000000/doses,
#          start_vac = factor(start_vac),
#          tag = factor(tag,
#                       levels = c("deat","hosp","case"),
#                       labels = c("Deaths","Severe Cases", "Infections")),
#          speed = factor(speed, levels = c("slow", "medium", "fast"),
#                         labels = paste(round(speed_median$vac_rate_per_million),
#                                        "doses/million-day"))) %>% 
#   ggplot(., aes(x = start_vac, y = effect_relative_adj, color = speed)) +
#   geom_boxplot() +
#   geom_point() +
#   geom_smooth(aes(group = interaction(speed, tag), fill = speed))+
#   scale_x_discrete(breaks = factor(unique(ms_cov_all$start_vac)),
#                    labels = month.abb) +
#   facet_grid(speed ~ tag) +
#   theme_bw() +
#   labs(x = "2021\nProgram Start Date",
#        color = "Roll-out Speed",
#        fill = "Roll-out Speed",
#        y = "% Reduction Compared to No Vaccine Used, Per Million Doses") +
#   theme(axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         legend.position = "top",
#         strip.text = element_text(size = 14)) +
#   ggsci::scale_color_futurama() +
#   ggsci::scale_fill_futurama() # +
#   # scale_y_continuous(labels = scientific_10)
# 
# ggsave("figs/AfHEA/relative_effect_adj.png", width = 15, height = 10)
# 
# 
# for(i in 1:nrow(model_selected)){
#   res[[i]] %>% 
#     setNames(1:length(res[[i]])) %>% 
#     bind_rows(.id = "cov_index") %>% 
#     filter(grepl("death|case|hosp", compartment)) %>% 
#     group_by(cov_index, t, population, compartment) %>% 
#     summarise(value = sum(value), .groups = "drop") %>% 
#     left_join(ms_cov_all %>%
#                 rownames_to_column(var = "cov_index"),
#               by = "cov_index") %>% 
#     rename(t_internal = t) %>% 
#     left_join(model_selected, by = c("population" = "country_name")) %>% 
#     rename(t_start = t) %>% 
#     mutate(date = ymd("2019-12-01") + t_start + t_internal) %>% 
#     filter((compartment == "death_o") & date <= date_switch[1] |
#              (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
#              (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
#              (compartment == "death_voc3_o") & date > date_switch[3] |
#              (compartment == "hosp_i") & date <= date_switch[1] |
#              (compartment == "hosp_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
#              (compartment == "hosp_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
#              (compartment == "hosp_voc3_i") & date > date_switch[3] |
#              compartment == "cases") -> tmp
#   
#   
#   tmp %>% 
#     mutate(tag = substr(compartment, 1, 4),
#            start_vac = factor(start_vac)) %>% 
#     filter(tag == "deat",
#            date >= "2021-01-01") %>% 
#     ggplot(., aes(x = date, y = value, group = cov_index, color = start_vac)) +
#     geom_line() +
#     facet_wrap(~speed, ncol = 1) +
#     labs(title = model_selected$country_name[i])
#   
#   ggsave(paste0("figs/intermediate/sim_ts/", model_selected$country_name[i],".png"),
#          width = 15, height = 10)
#   
# }
# 
# res_novac %>% 
#   bind_rows()
# 
# res[[6]][[1]] %>% 
#   filter(!grepl("death|hosp|foi|case|subclinical", compartment)) %>% 
#   group_by(t, compartment) %>% 
#   summarise(value = sum(value)) %>% 
#   ggplot(., aes(x = t, y = value, group = compartment, color = compartment)) +
#   geom_line() +
#   facet_wrap(~compartment, scales = "free")
# 
# vac_policy(params_step1[[j]],
#            # these two parameters define the supply conditions
#            milestone_date = c(as.character(ms_cov_all$start_vac[i]),"2022-06-30"), 
#            milestone_cov = c(0, ms_cov_all$ms_cov[i]),
#            # prioritisation, assume 60+  all prioritised
#            priority = c(NA, NA, NA, NA,
#                         2,  2,  2,  2,
#                         2,  2,  2,  2,
#                         1,  1,  1,  1),
#            # maximum feasible uptakes
#            cov_max = c(rep(0,4),
#                        rep(0.7, 8),
#                        rep(0.9, 4)),
#            supply_delay = 4, # unit = weeks
#            dose_interval = 4) -> test
# 
# test$scenarios[[1]]$daily_vac_scenarios %>% 
#   arrange(date) %>% 
#   mutate_at(vars(starts_with("Y")), cumsum) %>% 
#   select(-starts_with("supply")) %>% 
#   pivot_longer(starts_with("Y")) %>% 
#   separate(name, into = c("ag", "dose")) %>% 
#   ggplot(., aes(x = date, y = value, group = dose)) +
#   geom_line() +
#   facet_wrap(~ag) 
# 
# test$scenarios[[1]]$daily_vac_scenarios %>%
#   arrange(date) %>% 
#   mutate_at(vars(starts_with("Y")), cumsum) %>% 
#   select(-starts_with("supply")) %>% 
#   pivot_longer(starts_with("Y")) %>% 
#   separate(name, into = c("ag", "dose")) %>% 
#   ggplot(., aes(x = date, y = value, group = dose)) +
#   geom_line() +
#   facet_wrap(~ag)
# 
# test$scenarios[[1]]$pending %>% 
#   filter(status == "complete") %>% 
#   pull(elapse) 
# 
# j = 4
# p <- list()
# for(j in 1:nrow(model_selected)){
#   res[[j]][[3]]$scenarios[[1]]$daily_vac_scenarios %>%
#     arrange(date) %>% 
#     mutate_at(vars(starts_with("Y")), cumsum) %>% 
#     select(-starts_with("supply")) %>% 
#     pivot_longer(starts_with("Y")) %>% 
#     separate(name, into = c("ag", "dose")) %>% 
#     ggplot(., aes(x = date, y = value, group = dose)) +
#     geom_line() +
#     facet_wrap(~ag) +
#     labs(title = model_selected$country_name[j]) -> p[[j]]
# }
# 
# 
# 
# 
# 
# 
# c(1:3,55:57, 118:120) %>% 
#   map(~test[[.]]$res[[1]]) %>% 
#   map(cm_simulate) %>% 
#   map(~.[["dynamics"]]) %>% 
#   setNames(c("Jan", "May", "Oct")) %>% 
#   bind_rows(.id = "start_month") %>% 
#   group_by(start_month, t, compartment) %>%
#   filter(grepl("cases|hosp|death", compartment)) %>% 
#   summarise(value = sum(value)) %>% 
#   mutate(date = ymd("2019-12-01") + model_selected$t[1] + t) -> res
# 
# 
# c(2,56,119) %>% 
#   map(~test[[.]]$param) %>% 
#   map(cm_simulate) %>% 
#   map(~.[["dynamics"]]) %>% 
# setNames(c("Jan", "May", "Oct")) %>% 
#   bind_rows(.id = "start_month") %>% 
#   group_by(start_month, t, compartment) %>%
#   filter(grepl("cases|hosp|death", compartment)) %>% 
#   summarise(value = sum(value)) %>% 
#   mutate(date = ymd("2019-12-01") + model_selected$t[1] + t) -> res_novac
# 
# res %>% mutate(vac = T) %>% 
#   bind_rows(res_novac %>% mutate(vac = F)) %>% 
#   mutate(start_month = if_else(vac == F, "NO VAC", start_month)) %>% 
#   distinct() %>% 
#   filter((compartment == "death_o") & date <= date_switch[1] |
#          (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2]|
#            (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3]|
#            (compartment == "death_voc3_o") & date > date_switch[3]) %>% 
#   filter(date <= "2022-08-30") %>% 
#   group_by(start_month) %>% summarise(value = sum(value))
#   ggplot(., aes(x = date, y = value, group = start_month, color = start_month)) +
#   geom_line()
# 
# #### aggregate estimate ####
#   
# lapply(1:nrow(model_selected), function(x){
#   res[[x]] %>% 
#     setNames(1:length(res[[x]])) %>% 
#     bind_rows(.id = "cov_index") %>% 
#     filter(grepl("death|case|hosp", compartment)) %>% 
#     group_by(cov_index, t, population, compartment) %>% 
#     summarise(value = sum(value), .groups = "drop") %>% 
#     left_join(ms_cov_all %>%
#                 rownames_to_column(var = "cov_index"),
#               by = "cov_index") %>% 
#     rename(t_internal = t) %>% 
#     left_join(model_selected, by = c("population" = "country_name")) %>% 
#     rename(t_start = t) %>% 
#     mutate(date = ymd("2019-12-01") + t_start + t_internal) %>% 
#     filter((compartment == "death_o") & date <= date_switch[1] |
#              (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
#              (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
#              (compartment == "death_voc3_o") & date > date_switch[3]
#              # (compartment == "hosp_i") & date <= date_switch[1] |
#              # (compartment == "hosp_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
#              # (compartment == "hosp_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
#              # (compartment == "hosp_voc3_i") & date > date_switch[3] |
#              # compartment == "cases"
#            ) %>% 
#     filter(date >= "2021-01-01") %>% 
#     mutate(tag = substr(compartment, 1, 4)) %>% 
#     group_by(speed, start_vac, r, t_start, ur, iso3c, tag) %>% 
#     summarise(value = sum(value), .groups = "drop") 
# }) %>% 
#     bind_rows() -> seg1
# 
# res_novac %>% 
#   map(group_by, population, t, compartment) %>% 
#   map(summarise, value = sum(value)) %>% 
#   bind_rows() %>% 
#   left_join(model_selected, by = c("population" = "country_name")) %>% 
#   rename(t_internal = `t.x`,
#          t_start = `t.y`) %>% 
#   mutate(date = ymd("2019-12-01") + t_internal + t_start) %>% 
#   filter((compartment == "death_o") & date <= date_switch[1] |
#            (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
#            (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
#            (compartment == "death_voc3_o") & date > date_switch[3] |
#            (compartment == "hosp_i") & date <= date_switch[1] |
#            (compartment == "hosp_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
#            (compartment == "hosp_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
#            (compartment == "hosp_voc3_i") & date > date_switch[3] |
#            compartment == "cases") %>% 
#   filter(date >= "2021-01-01") %>% 
#   group_by(population, iso3c, compartment) %>% 
#   summarise(novac = sum(value)) -> seg2
# 
# 
# 
# seg1 %>% 
#   mutate(start_vac = if_else(start_vac == "2021-11-26", ymd("2021-12-03"), start_vac)) %>% 
#   # filter(start_vac %in% ymd(c("2021-01-01", "2021-05-07", "2021-10-01"))) %>% 
#   left_join(seg2, by = "iso3c") %>% 
#   left_join(seg3, by = c("iso3c", "speed", "start_vac")) %>% 
#   mutate(effect = (novac - value)/novac,
#          effect_adjusted = effect/doses) %>% 
#   mutate(effect = (novac - value)/novac,
#          start_vac = factor(start_vac)) %>% 
#   ggplot(., aes(x = start_vac, y = effect)) +
#   geom_boxplot() +
#   facet_wrap(~speed)
# 
# params_step1[[1]]