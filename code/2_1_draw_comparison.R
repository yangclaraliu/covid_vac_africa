
# lapply(res[[j]], "[[", "fatal") %>% 
#   bind_rows(.id = "scenario_id") %>% 
#   left_join(ms_scenarios %>% 
#               rownames_to_column(var = "scenario_id"),
#             by = "scenario_id") %>%
#   dplyr::select(scenario, value, date_start, group) %>% 
#   # pivot_wider(names_from = scenario, values_from = value) %>% 
#   filter(year == 2021) %>% 
#   ggplot(., aes(x = date_start, y = value, group = scenario)) +
#   geom_line() +
#   facet_wrap(~group, scales = "free")
# 
#   lapply(res[[j]], "[[", "fatal") %>% 
#     map(tail)
#   
#   lapply(res[[j]], "[[", "fatal") %>% 
#     bind_rows(.id = "scenario_id") %>%
#     filter(year > 2020) %>% 
#     group_by(scenario_id) %>% 
#     summarise(value = sum(value))
#     
#   pivot_wider(names_from = scenario_id,
#                 values_from = value) %>% View()
  
  
params_grid_az <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_az.rds")
params_grid_pfizer <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_pfizer_v2.rds")

res <- list()

res[["pfizer"]] <- list()

res[["pfizer"]][["fatal"]] <- lapply(1:nrow(fitted_table), function(x) lapply(params_grid_pfizer[[x]], "[[", "fatal")) %>% 
  map(bind_rows, .id = "scenario_id") %>% 
  bind_rows() %>% 
  filter(year > 2020) 

res[["pfizer"]][["non_fatal"]] <- lapply(1:nrow(fitted_table), function(x) lapply(params_grid_pfizer[[x]], "[[", "non_fatal")) %>% 
  map(bind_rows, .id = "scenario_id") %>% 
  bind_rows() %>% 
  filter(year > 2020) 


res[["az"]] <- list()

res[["az"]][["fatal"]] <- lapply(1:nrow(fitted_table), function(x) lapply(params_grid_az[[x]], "[[", "fatal")) %>% 
  map(bind_rows, .id = "scenario_id") %>% 
  bind_rows() %>% 
  filter(year > 2020) 

res[["az"]][["non_fatal"]] <- lapply(1:nrow(fitted_table), function(x) lapply(params_grid_az[[x]], "[[", "non_fatal")) %>% 
  map(bind_rows, .id = "scenario_id") %>% 
  bind_rows() %>% 
  filter(year > 2020) 

#### merge in baseline ####
res$pfizer$non_fatal %<>%
  left_join(res_novac$pfizer[[1]] %>% 
              rename(novac = value),
            by = c("name", "population", "year")) 

res$pfizer$fatal %<>% 
  left_join(res_novac$pfizer[[2]],
            by = c("name", "population", "group", "year")) 


res$az$non_fatal %<>%
  left_join(res_novac$az[[1]] %>% 
              rename(novac = value),
            by = c("name", "population", "year")) 

res$az$fatal %<>% 
  left_join(res_novac$az[[2]],
            by = c("name", "population", "group", "year")) 

#### compare
res$az%>% 
  bind_rows(.id = "outcome_type") %>% 
  dplyr::select(-iso3c) %>% 
  group_by(scenario_id, population, name) %>% 
  summarise(value = sum(value),
            novac = sum(novac)) %>% 
  filter(!grepl("_p_", name)) %>% 
  left_join(ms_scenarios, # %>% 
              # rownames_to_column(var = "scenario_id"),
            by = "scenario_id") %>% 
  mutate(scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast")),
         name = factor(name,
                       levels = c("cases", "severe_i_all", "critical_i_all", "death_o_all"),
                       labels = c("All Cases", "Severe Cases", "Critical Cases", "Deaths"))) %>% 
  # mutate(date_start = factor(date_start,
  #                            levels = as.character(seq(ymd("2021-01-01"),ymd("2021-12-01"),"month")))) %>% 
  ggplot(., aes(group = interaction(date_start, scenario), y = 1-value/novac, x = date_start, color = scenario)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~name, nrow = 4) +
  theme_bw() +
  # lims(y = c(0, 0.9)) +
  theme(legend.position = "top") +
  labs(color = "",
       x = "Vaccine Roll-out Start Date",
       y = "Relative Reduction") +
  scale_color_futurama() 

ggsave("figs/epi_outcomes_vertical.png", width = 15, height = 8)

res$az%>% 
  bind_rows(.id = "outcome_type") %>% 
  dplyr::select(-iso3c) %>% 
  group_by(scenario_id, population, name) %>% 
  summarise(value = sum(value),
            novac = sum(novac)) %>% 
  filter(!grepl("_p_", name)) %>% 
  left_join(ms_scenarios, # %>% 
            # rownames_to_column(var = "scenario_id"),
            by = "scenario_id") %>% 
  mutate(scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast")),
         name = factor(name,
                       levels = c("cases", "severe_i_all", "critical_i_all", "death_o_all"),
                       labels = c("All Cases", "Severe Cases", "Critical Cases", "Deaths"))) %>% 
  # mutate(date_start = factor(date_start,
  #                            levels = as.character(seq(ymd("2021-01-01"),ymd("2021-12-01"),"month")))) %>% 
  ggplot(., aes(y = 1-value/novac, x = date_start, color = scenario)) +
  geom_boxplot(aes(group = interaction(scenario, date_start)), outlier.shape = NA) +
  # geom_point() +
  geom_smooth(aes(fill = scenario)) +
  # geom_boxplot(outlier.shape = NA) +
  facet_grid(~name) +
  
  theme_bw() +
  # lims(y = c(0, 0.9)) +
  theme(legend.position = "top") +
  labs(color = "",
       x = "Vaccine Roll-out Start Date",
       y = "Relative Reduction") +
  scale_color_futurama() +
  scale_fill_futurama(guide = "none") 

ggsave("figs/epi_outcomes_horizontal.png", width = 15, height = 8)
