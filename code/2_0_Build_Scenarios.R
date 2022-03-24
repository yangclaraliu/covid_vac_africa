fitted_parameters %>% 
  bind_rows() %>% 
  mutate(country_name = stop_fitting$loc,
         iso3c = stop_fitting$iso3c) %>% 
  rename(t = par2,
         r = par1,
         ur = par3) -> model_selected

model_selected[grep("congo", model_selected$country_name, ignore.case = T),"country_name"] <- "Dem. Republic of the Congo"

date_switch <- c("2021-01-15", "2021-04-15", "2021-12-15")
params_step1 <- list()

for(i in 1:nrow(model_selected)){
  params_step1[[i]] <- 
  gen_country_basics(country = model_selected$country_name[i],
                     waning_nat = 52*7*3,
                     R0_assumed  = model_selected$r[i],
                     date_start = as.character(ymd("2019-12-01") + model_selected$t[i]),
                     date_end = "2022-12-31",
                     processes = burden_processes,
                     deterministic = TRUE) %>%
    update_vac_char(.,
                    ve_i   = ve$ve_i_o[1],  # infection blocking VE post 1 dose
                    v2e_i  = ve$ve_i_o[2],  # infection blocking VE post 2 doses
                    ve_d   = ve$ve_d[1],    # clinical fraction among breakthrough post 1 dose
                    v2e_d  = ve$ve_d[2],    # clinical fraction among breakthrough post 2 doses
                    wv = 1/360) %>% 
    change_VOC(., 
               date_switch = c("2021-01-15", "2021-04-15", "2021-12-15"),
               rc_severity = c(1, 1.5,0.5), # relative change in ihr and ifr
               rc_transmissibility = c(1, 1.5, 1.5), # relative change in transmissibility via u, uv and uv2
               rc_ve = c(1, 0.8, 0.5)) # relative in ve against infection)

}

res_novac <- list()
for(j in 1:length(params_step1)){
 cm_simulate(params_step1[[j]]) %>% 
    .[["dynamics"]] -> res_novac[[j]]
}

res <- list()
for(j in 1:length(params_step1)){
  res[[j]] <- list()
  # Jan, May, July, Oct, Dec
  for(i in 1:nrow(ms_cov_all)){
    vac_policy(params_step1[[j]],
               # these two parameters define the supply conditions
               milestone_date = c(as.character(ms_cov_all$start_vac[i]),"2022-12-31"), 
               milestone_cov = c(0, ms_cov_all$ms_cov[i]),
               # prioritisation, assume 60+  all prioritised
               priority = c(NA, NA, NA, NA,
                            2,  2,  2,  2,
                            2,  2,  2,  2,
                            1,  1,  1,  1),
               # maximum feasible uptakes
               cov_max = c(rep(0,4),
                           rep(0.7, 8),
                           rep(0.9, 4)),
               supply_delay = 4, # unit = weeks
               dose_interval = 4) %>% 
      .$res %>% 
      .[[1]] %>% 
      cm_simulate %>% 
      .[["dynamics"]] %>% 
      group_by(t, population, compartment) %>% 
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(date = ymd("2019-12-01") + t + model_selected$t[j]) %>% 
      # filter(grepl("case|hosp|death", compartment)) %>% 
      filter((compartment == "death_o") & date <= date_switch[1] |
               (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
               (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
               (compartment == "death_voc3_o") & date > date_switch[3] |
               (compartment == "hosp_i") & date <= date_switch[1] |
               (compartment == "hosp_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
               (compartment == "hosp_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
               (compartment == "hosp_voc3_i") & date > date_switch[3] |
               compartment == "cases") -> res[[j]][[i]]
    
    print(paste0(model_selected$country_name[j], ", ", i))
  }
  write_rds(res, "data/intermediate/params_grid.rds")
}

res %>% 
  map(bind_rows, .id = "ms_cov_index") %>% 
  bind_rows() %>% 
  mutate(tag = substr(compartment, 1, 4)) %>% 
  filter(date >= ymd("2021-01-01")) %>% 
  group_by(ms_cov_index, population, tag) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  mutate(iso3c = countrycode(population, "country.name", "iso3c"))-> tmp

tmp %>% 
  left_join(seg2%>% 
              mutate(tag = substr(compartment, 1, 4)) %>% 
              group_by(population, iso3c, tag) %>% 
              summarise(novac  = sum(novac), .groups = "drop"),
            by = c("iso3c", "tag", "population")) %>% 
  left_join(ms_cov_all %>% 
              rownames_to_column(var = "ms_cov_index"),
            by = "ms_cov_index") %>% 
  left_join(seg3, by = c("iso3c", "speed", "start_vac", "ms_cov")) %>% 
  mutate(effect_relative = (1 - value/novac)*100,
         effect_relative_adj = effect_relative*1000000/doses,
         start_vac = factor(start_vac),
         tag = factor(tag,
                      levels = c("deat","hosp","case"),
                      labels = c("Deaths","Severe Cases", "Infections")),
         speed = factor(speed, levels = c("slow", "medium", "fast"),
                        labels = paste(round(speed_median$vac_rate_per_million),
                                       "doses/million-day"))) %>% 
  ggplot(., aes(x = start_vac, y = effect_relative_adj, color = speed)) +
  geom_boxplot() +
  geom_point() +
  geom_smooth(aes(group = interaction(speed, tag), fill = speed))+
  scale_x_discrete(breaks = factor(unique(ms_cov_all$start_vac)),
                   labels = month.abb) +
  facet_grid(speed ~ tag) +
  theme_bw() +
  labs(x = "2021\nProgram Start Date",
       color = "Roll-out Speed",
       fill = "Roll-out Speed",
       y = "% Reduction Compared to No Vaccine Used, Per Million Doses") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "top",
        strip.text = element_text(size = 14)) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() # +
  # scale_y_continuous(labels = scientific_10)

ggsave("figs/AfHEA/relative_effect_adj.png", width = 15, height = 10)


for(i in 1:nrow(model_selected)){
  res[[i]] %>% 
    setNames(1:length(res[[i]])) %>% 
    bind_rows(.id = "cov_index") %>% 
    filter(grepl("death|case|hosp", compartment)) %>% 
    group_by(cov_index, t, population, compartment) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    left_join(ms_cov_all %>%
                rownames_to_column(var = "cov_index"),
              by = "cov_index") %>% 
    rename(t_internal = t) %>% 
    left_join(model_selected, by = c("population" = "country_name")) %>% 
    rename(t_start = t) %>% 
    mutate(date = ymd("2019-12-01") + t_start + t_internal) %>% 
    filter((compartment == "death_o") & date <= date_switch[1] |
             (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
             (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
             (compartment == "death_voc3_o") & date > date_switch[3] |
             (compartment == "hosp_i") & date <= date_switch[1] |
             (compartment == "hosp_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
             (compartment == "hosp_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
             (compartment == "hosp_voc3_i") & date > date_switch[3] |
             compartment == "cases") -> tmp
  
  
  tmp %>% 
    mutate(tag = substr(compartment, 1, 4),
           start_vac = factor(start_vac)) %>% 
    filter(tag == "deat",
           date >= "2021-01-01") %>% 
    ggplot(., aes(x = date, y = value, group = cov_index, color = start_vac)) +
    geom_line() +
    facet_wrap(~speed, ncol = 1) +
    labs(title = model_selected$country_name[i])
  
  ggsave(paste0("figs/intermediate/sim_ts/", model_selected$country_name[i],".png"),
         width = 15, height = 10)
  
}

res_novac %>% 
  bind_rows()

res[[6]][[1]] %>% 
  filter(!grepl("death|hosp|foi|case|subclinical", compartment)) %>% 
  group_by(t, compartment) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(., aes(x = t, y = value, group = compartment, color = compartment)) +
  geom_line() +
  facet_wrap(~compartment, scales = "free")

vac_policy(params_step1[[j]],
           # these two parameters define the supply conditions
           milestone_date = c(as.character(ms_cov_all$start_vac[i]),"2022-06-30"), 
           milestone_cov = c(0, ms_cov_all$ms_cov[i]),
           # prioritisation, assume 60+  all prioritised
           priority = c(NA, NA, NA, NA,
                        2,  2,  2,  2,
                        2,  2,  2,  2,
                        1,  1,  1,  1),
           # maximum feasible uptakes
           cov_max = c(rep(0,4),
                       rep(0.7, 8),
                       rep(0.9, 4)),
           supply_delay = 4, # unit = weeks
           dose_interval = 4) -> test

test$scenarios[[1]]$daily_vac_scenarios %>% 
  arrange(date) %>% 
  mutate_at(vars(starts_with("Y")), cumsum) %>% 
  select(-starts_with("supply")) %>% 
  pivot_longer(starts_with("Y")) %>% 
  separate(name, into = c("ag", "dose")) %>% 
  ggplot(., aes(x = date, y = value, group = dose)) +
  geom_line() +
  facet_wrap(~ag) 






test$scenarios[[1]]$daily_vac_scenarios %>%
  arrange(date) %>% 
  mutate_at(vars(starts_with("Y")), cumsum) %>% 
  select(-starts_with("supply")) %>% 
  pivot_longer(starts_with("Y")) %>% 
  separate(name, into = c("ag", "dose")) %>% 
  ggplot(., aes(x = date, y = value, group = dose)) +
  geom_line() +
  facet_wrap(~ag)

test$scenarios[[1]]$pending %>% 
  filter(status == "complete") %>% 
  pull(elapse) 

j = 4
p <- list()
for(j in 1:nrow(model_selected)){
  res[[j]][[3]]$scenarios[[1]]$daily_vac_scenarios %>%
    arrange(date) %>% 
    mutate_at(vars(starts_with("Y")), cumsum) %>% 
    select(-starts_with("supply")) %>% 
    pivot_longer(starts_with("Y")) %>% 
    separate(name, into = c("ag", "dose")) %>% 
    ggplot(., aes(x = date, y = value, group = dose)) +
    geom_line() +
    facet_wrap(~ag) +
    labs(title = model_selected$country_name[j]) -> p[[j]]
}






c(1:3,55:57, 118:120) %>% 
  map(~test[[.]]$res[[1]]) %>% 
  map(cm_simulate) %>% 
  map(~.[["dynamics"]]) %>% 
  setNames(c("Jan", "May", "Oct")) %>% 
  bind_rows(.id = "start_month") %>% 
  group_by(start_month, t, compartment) %>%
  filter(grepl("cases|hosp|death", compartment)) %>% 
  summarise(value = sum(value)) %>% 
  mutate(date = ymd("2019-12-01") + model_selected$t[1] + t) -> res


c(2,56,119) %>% 
  map(~test[[.]]$param) %>% 
  map(cm_simulate) %>% 
  map(~.[["dynamics"]]) %>% 
setNames(c("Jan", "May", "Oct")) %>% 
  bind_rows(.id = "start_month") %>% 
  group_by(start_month, t, compartment) %>%
  filter(grepl("cases|hosp|death", compartment)) %>% 
  summarise(value = sum(value)) %>% 
  mutate(date = ymd("2019-12-01") + model_selected$t[1] + t) -> res_novac

res %>% mutate(vac = T) %>% 
  bind_rows(res_novac %>% mutate(vac = F)) %>% 
  mutate(start_month = if_else(vac == F, "NO VAC", start_month)) %>% 
  distinct() %>% 
  filter((compartment == "death_o") & date <= date_switch[1] |
         (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2]|
           (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3]|
           (compartment == "death_voc3_o") & date > date_switch[3]) %>% 
  filter(date <= "2022-08-30") %>% 
  group_by(start_month) %>% summarise(value = sum(value))
  ggplot(., aes(x = date, y = value, group = start_month, color = start_month)) +
  geom_line()

#### aggregate estimate ####
  
lapply(1:nrow(model_selected), function(x){
  res[[x]] %>% 
    setNames(1:length(res[[x]])) %>% 
    bind_rows(.id = "cov_index") %>% 
    filter(grepl("death|case|hosp", compartment)) %>% 
    group_by(cov_index, t, population, compartment) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    left_join(ms_cov_all %>%
                rownames_to_column(var = "cov_index"),
              by = "cov_index") %>% 
    rename(t_internal = t) %>% 
    left_join(model_selected, by = c("population" = "country_name")) %>% 
    rename(t_start = t) %>% 
    mutate(date = ymd("2019-12-01") + t_start + t_internal) %>% 
    filter((compartment == "death_o") & date <= date_switch[1] |
             (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
             (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
             (compartment == "death_voc3_o") & date > date_switch[3]
             # (compartment == "hosp_i") & date <= date_switch[1] |
             # (compartment == "hosp_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
             # (compartment == "hosp_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
             # (compartment == "hosp_voc3_i") & date > date_switch[3] |
             # compartment == "cases"
           ) %>% 
    filter(date >= "2021-01-01") %>% 
    mutate(tag = substr(compartment, 1, 4)) %>% 
    group_by(speed, start_vac, r, t_start, ur, iso3c, tag) %>% 
    summarise(value = sum(value), .groups = "drop") 
}) %>% 
    bind_rows() -> seg1

res_novac %>% 
  map(group_by, population, t, compartment) %>% 
  map(summarise, value = sum(value)) %>% 
  bind_rows() %>% 
  left_join(model_selected, by = c("population" = "country_name")) %>% 
  rename(t_internal = `t.x`,
         t_start = `t.y`) %>% 
  mutate(date = ymd("2019-12-01") + t_internal + t_start) %>% 
  filter((compartment == "death_o") & date <= date_switch[1] |
           (compartment == "death_voc1_o") & date > date_switch[1] & date <= date_switch[2] |
           (compartment == "death_voc2_o") & date > date_switch[2] & date <= date_switch[3] |
           (compartment == "death_voc3_o") & date > date_switch[3] |
           (compartment == "hosp_i") & date <= date_switch[1] |
           (compartment == "hosp_voc1_i") & date > date_switch[1] & date <= date_switch[2] |
           (compartment == "hosp_voc2_i") & date > date_switch[2] & date <= date_switch[3] |
           (compartment == "hosp_voc3_i") & date > date_switch[3] |
           compartment == "cases") %>% 
  filter(date >= "2021-01-01") %>% 
  group_by(population, iso3c, compartment) %>% 
  summarise(novac = sum(value)) -> seg2



seg1 %>% 
  mutate(start_vac = if_else(start_vac == "2021-11-26", ymd("2021-12-03"), start_vac)) %>% 
  # filter(start_vac %in% ymd(c("2021-01-01", "2021-05-07", "2021-10-01"))) %>% 
  left_join(seg2, by = "iso3c") %>% 
  left_join(seg3, by = c("iso3c", "speed", "start_vac")) %>% 
  mutate(effect = (novac - value)/novac,
         effect_adjusted = effect/doses) %>% 
  mutate(effect = (novac - value)/novac,
         start_vac = factor(start_vac)) %>% 
  ggplot(., aes(x = start_vac, y = effect)) +
  geom_boxplot() +
  facet_wrap(~speed)

params_step1[[1]]