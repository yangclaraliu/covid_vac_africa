draw_strategy <- function(param_set,
                             sim_end = "2022-12-31",
                             fn = NULL){
  df <- list()
  i = 23
  
  for(j in 1:nrow(fitted_table)){
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
      
      data.frame(size = param_set[[j]]$pop[[1]]$size) |> 
        rownames_to_column(var = "age") |> 
        mutate(age = parse_number(age),
               cat = case_when(age <= 12 & age > 4 ~ "Adults", 
                               age > 12 ~ "Older Adults")) |> 
        filter(!is.na(cat)) |> 
        group_by(cat) |> 
        summarise(size = sum(size)) -> tmp_pop
      
      tmp$scenarios[[1]]$daily_vac_scenarios |> 
        select(-starts_with("supply"), phase) |> 
        mutate_at(vars(starts_with("Y", ignore.case = F)), cumsum) |> 
        pivot_longer(cols = starts_with("Y", ignore.case = F)) |> 
        separate(name, into = c("age", "dose")) |> 
        mutate(age = parse_number(age),
               cat = case_when(age <= 12 & age > 4 ~ "Adults", 
                               age > 12 ~ "Older Adults")) |> 
        filter(!is.na(cat),
               date >= "2021-01-01") |> 
        group_by(date, cat, dose) |> 
        summarise(value = sum(value)) |> 
        left_join(tmp_pop, by = "cat") |> 
        mutate(p = value/size,
               p = case_when(cat == "Adults" & p > 0.6 ~ 0.6,
                             cat == "Older Adults" & p > 0.8 ~ 0.8,
                             TRUE ~ p)) -> df[[j]]
      print(j)
  }
  return(df)
}

draw_pfizer_grouped <- draw_strategy(base$pfizer)


draw_pfizer_grouped |> 
  set_names(fitted_table$iso3c) |> 
  bind_rows(.id = "iso3c") |> 
  filter(dose == "d1") |> 
  ggplot(aes(x = date, y = p, group = interaction(iso3c, cat), color = cat)) +
  geom_line() +
  labs(x = "Date",
       y = "Vaccine Coverage",
       title = "d1, Scenario: 2022-08-01, medium",
       color = "Population") +
  theme_cowplot() +
  custom_theme +
  scale_color_futurama() +
  theme(legend.position = "top")

ggsave("figs/R2R_R1/rollout_grouped.png", width = 10, height = 8)
# ms_scenarios %>% 
#   dplyr::select(-date_vac_end_ext, -cov_ext) %>% 
#   mutate(sim_end = if_else(date_vac_end < "2022-12-31", "2022-12-31", as.character(NA)),
#          cov_end = if_else(is.na(sim_end), as.numeric(NA), 0.7),
#          scenario = factor(scenario, levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast"))) %>% 
#   ggplot(.) +
#   geom_segment(aes(x = date_start, xend = date_vac_end, y = 0, yend = cov, color = scenario), size = 0.3) + 
#   geom_segment(aes(x = date_vac_end, xend = ymd(sim_end), y = 0.7, yend = 0.7, color = scenario), size = 0.3) +
#   scale_color_futurama() +
#   theme_bw() +
#   labs(x = "Vaccination program start date",
#        y = "Vaccine coverage\n(two-dose, assumed)",
#        color = "Vaccine roll-out rate") +
#   theme(legend.position = "top") +
#   custom_theme +
#   lims(y = c(0,1)) -> p_ro_scenarios