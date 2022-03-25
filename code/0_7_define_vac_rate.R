owid_vac %>% 
  dplyr::select(iso3c, date, daily_vaccinations_per_million) %>% 
  filter(!is.na(daily_vaccinations_per_million)) %>% 
  group_by(iso3c) %>% group_split() %>% map(~replace(., is.na(.), 0)) %>% 
  map(arrange, date) %>% map(mutate, cumulative_doses_per_million = cumsum(daily_vaccinations_per_million)) -> tmp

tmp %>% map(pull, iso3c) %>% map(unique) %>% unlist -> tmp_cn

tmp %>% 
  map(~lm(cumulative_doses_per_million ~ date, data = .)) %>% 
  map(summary) %>% map(~.$coefficients) %>% map(data.frame) %>%
  setNames(tmp_cn) %>% 
  bind_rows(.id = "iso3c") %>% 
  rownames_to_column(var = "metric") %>% 
  filter(grepl("date", metric)) %>% 
  mutate(nt_speed = ntile(Estimate, 3)) %>% 
  dplyr::select(iso3c, nt_speed, Estimate ) -> tmp_speed

# tmp %>% bind_rows() %>% 
#   left_join(tmp_speed, by = "iso3c") %>% 
#   mutate(nt_speed = factor(nt_speed, levels = 1:3, labels = speed_labels)) %>% 
#   ggplot(., aes(x = date, 
#                 y = cumulative_doses_per_million/1000000, 
#                 group = iso3c,
#                 color = nt_speed)) +
#   geom_line(alpha = 0.7) +
#   # facet_wrap(~nt_speed) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   labs(x = "",
#        y = "Cumulative Doses/Person") +
#   ggsci::scale_color_futurama()-> p

# p
# ggsave("figs/define_speed_AfHEA.png", width = 6, height = 6)

tmp_speed %>% 
  mutate(nt_speed = factor(nt_speed, levels = 1:3, labels = speed_labels)) %>% 
  group_by(nt_speed) %>% #group_split() %>% map(pull, Estimate) %>%
  summarise(vac_rate_per_million = median(Estimate)) -> speed_median

# CJ(date = seq(ymd("2021-01-01"), ymd("2022-12-30"), "day"),
#    nt_speed = speed_labels) %>%
#   left_join(speed_median, by = "nt_speed") %>% 
#   mutate(day_since = as.numeric(date - ymd("2021-01-01")),
#          cov = day_since * vac_rate_per_million/(10000*2)) %>% 
#   # filter(date == "2022-12-30")
#   ggplot(., aes(x = date, y = cov, group = nt_speed, color = nt_speed)) +
#   geom_line() +
#   theme_bw() +
#   labs(color = "Roll-out Speed") +
#   theme(legend.position = "top") -> p

# ggsave("figs/median_speed.png", width = 8, height = 8)

ms_date_all <- data.frame(date_start =
                            seq(ymd("2021-01-01"), ymd("2021-12-15"), by = "month"))

CJ(date_start = seq(ymd("2021-01-01"), ymd("2021-12-15"), by = "month"),
   scenario = c("slow", "medium", "fast"),
   Type = c("Pfizer","AZ"),
   iso3c = fitted_table$iso3c) %>% 
  mutate(r_vac = case_when(scenario == "slow" ~ 275/(1000000*2),
                           scenario == "medium" ~ 826/(1000000*2),
                           scenario == "fast" ~ 2066/(1000000*2)),
         Rate = case_when(scenario == "slow" ~ 275,
                           scenario == "medium" ~ 826,
                           scenario == "fast" ~ 2066),
         sat_days = 0.6/r_vac,
         date_end = ymd("2022-12-31"),
         date_end_ext = ymd("2023-06-30"),
         window_allowed = date_end - date_start,
         window_allowed_ext = date_end_ext - date_start,
         sat = sat_days <= window_allowed,
         sat_ext = sat_days <= window_allowed_ext,
         Elapse = if_else(sat == T, 
                          round(sat_days/30),
                          round(as.numeric(window_allowed)/30)),
         Elapse_extend = if_else(sat_ext == T, 
                          round(sat_days/30),
                          round(as.numeric(window_allowed_ext)/30)),
         date_vac_end = if_else(sat == T,
                                date_start + sat_days,
                                ymd("2022-12-31")),
         date_vac_end_ext = if_else(sat_ext == T,
                                date_start + sat_days,
                                ymd("2023-06-30")),
         cov = if_else(sat == T,
                       0.6,
                       r_vac*as.numeric(window_allowed)),
         cov_ext = if_else(sat_ext == T,
                           0.6,
                           r_vac*as.numeric(window_allowed_ext))) -> ms_cov_all

predict(model_cost_vaccines, 
        newdata = ms_cov_all[,c("iso3c","Type","Elapse","Rate")]) -> vac_unit

predict(model_cost_vaccines, 
        newdata = ms_cov_all[,c("iso3c","Type","Elapse_extend","Rate")] %>% 
          rename(Elapse = Elapse_extend)) -> vac_unit_ext


ms_cov_all %<>% 
  mutate(vac_unit = vac_unit,
         vac_unit_ext = vac_unit_ext)


# ms_cov_all <- lapply(1:nrow(ms_date_all), function(x) {draw_supply(start_vac = ms_date_all$date_start[x])}) %>% 
#   bind_rows()
# 
# lapply(1:nrow(model_selected), function(x) ms_cov_all %>% mutate(iso3c = model_selected$iso3c[x])) %>% 
#   bind_rows() %>% 
#   left_join(pop %>% 
#               group_by(iso3c) %>% 
#               summarise(tot = (sum(f) + sum(m))*1000),
#             by = "iso3c") %>% 
#   data.table() %>% 
#   mutate(doses = tot*ms_cov*2) -> seg3
# 
# 
# seg3 %>% 
#   filter(iso3c == "DZA") %>% 
#   mutate(date_end = ymd("2022-12-31"),
#          speed = factor(speed, levels = c("slow", "medium", "fast"),
#                         labels = paste(round(speed_median$vac_rate_per_million),
#                                        "doses/million-day")))  %>% 
#   ggplot(.) +
#   geom_segment(aes(x = start_vac, xend = date_end, y = 0, yend = ms_cov, color = speed)) +
#   ggsci::scale_color_futurama() +
#   theme_bw() +
#   theme(legend.position = "top") +
#   labs(color = "Roll-out Speed",
#        x = "Program Start Date",
#        y = "Intended Coverage") 
# 
# ggsave("figs/AfHEA/scenario_viz.png", width = 8, height = 8)

