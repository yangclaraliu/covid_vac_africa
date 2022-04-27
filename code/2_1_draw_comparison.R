
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
  
#   
# params_grid_az <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_az_70.rds")
# params_grid_pfizer <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_pfizer_70.rds")
# res_novac <- read_rds("data/intermediate/res_novac.rds")
# 
# params_grid_az_ext <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_az_70_ext.rds")
# params_grid_pfizer_ext <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_pfizer_70_ext.rds")
# 
# params_grid_az_low <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_az_70_low.rds")
# params_grid_pfizer_low <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_pfizer_70_low.rds")
# 

#### organise outcomes ####
organise_outcomes <- function(tmp_pfizer,
                              tmp_az){
  m <- list()
  
  m[["pfizer"]] <- list()
  
  m[["pfizer"]][["fatal"]] <- lapply(1:nrow(fitted_table), function(x) lapply(tmp_pfizer[[x]], "[[", "fatal"))  %>%
    map(bind_rows, .id = "scenario_id") %>% 
    bind_rows() %>%
    filter(year > 2020)
  
  m[["pfizer"]][["non_fatal"]] <- lapply(1:nrow(fitted_table), function(x) lapply(tmp_pfizer[[x]], "[[", "non_fatal")) %>%
    map(bind_rows, .id = "scenario_id") %>% 
    bind_rows() %>%
    filter(year > 2020)
  
  m[["az"]] <- list()
  
  m[["az"]][["fatal"]] <- lapply(1:nrow(fitted_table), function(x) lapply(tmp_az[[x]], "[[", "fatal")) %>%
    map(bind_rows, .id = "scenario_id") %>% 
    bind_rows() %>%
    filter(year > 2020)
  
  m[["az"]][["non_fatal"]] <- lapply(1:nrow(fitted_table), function(x) lapply(tmp_az[[x]], "[[", "non_fatal")) %>%
    map(bind_rows, .id = "scenario_id") %>% 
    bind_rows() %>%
    filter(year > 2020)
  
  return(m)
}

# res <- organise_outcomes(tmp_pfizer = params_grid_pfizer, 
#                          tmp_az = params_grid_az)
# res_ext <-  organise_outcomes(tmp_pfizer = params_grid_pfizer_ext, 
#                               tmp_az = params_grid_az_ext)
# res_low <-  organise_outcomes(tmp_pfizer = params_grid_pfizer_low, 
#                               tmp_az = params_grid_az_low)

#### merge novac for baseline ####
merge_novac <- function(tmp){
  require(magrittr)
  tmp$pfizer$non_fatal %<>%
    left_join(res_novac$pfizer[[1]] %>%
                rename(novac = value),
              by = c("name", "population", "year"))
  
  tmp$pfizer$fatal %<>%
    left_join(res_novac$pfizer[[2]],
              by = c("name", "population", "group", "year"))
  
  
  tmp$az$non_fatal %<>%
    left_join(res_novac$az[[1]] %>%
                rename(novac = value),
              by = c("name", "population", "year"))
  
  tmp$az$fatal %<>%
    left_join(res_novac$az[[2]],
              by = c("name", "population", "group", "year"))
  
  return(tmp)
}

# res <- merge_novac(res)
# res_ext <- merge_novac(res_ext)
# res_low <- merge_novac(res_low)

# write_rds(res, "data/res.rds")
# write_rds(res_ext, "data/res_ext.rds")
# write_rds(res_low, "data/res_low.rds")
res <- read_rds("data/res.rds")
res_ext <- read_rds("data/res_ext.rds")
res_low <- read_rds("data/res_low.rds")

#### compare
# res$az %>%
#   bind_rows(.id = "outcome_type") %>%
#   dplyr::select(-iso3c) %>%
#   group_by(scenario_id, population, name) %>%
#   summarise(value = sum(value),
#             novac = sum(novac)) %>%
#   filter(!grepl("_p_", name)) %>%
#   left_join(ms_scenarios, # %>%
#                # rownames_to_column(var = "scenario_id"),
#             by = "scenario_id") %>%
#   mutate(scenario = factor(scenario,
#                            levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast")),
#          name = factor(name,
#                        levels = c("cases", "severe_i_all", "critical_i_all", "death_o_all"),
#                        labels = c("All Cases", "Severe Cases", "Critical Cases", "Deaths"))) %>%
#   filter(name %in% c("All Cases", "Deaths")) %>%
#   mutate(rr = 1 - value/novac) %>% 
#   #### INLINE STATS ####
#   # filter(date_start == "2021-08-01") %>% group_by(name, scenario) %>% 
#   # filter(scenario == "Medium", date_start %in% c(ymd("2021-01-01"), ymd("2021-08-01"))) %>% 
#   # group_by(name, date_start) %>% 
#   # summarise(Q1 = quantile(rr, 0.25)*100,
#   #           md = median(rr)*100,
#   #           mean = mean(rr)*100,
#   #           Q3 = quantile(rr, 0.75)*100) %>% View()
# 
#   # mutate(date_start = factor(date_start,
#   #                            levels = as.character(seq(ymd("2021-01-01"),ymd("2021-12-01"),"month")))) %>%
#   ggplot(., aes(group = interaction(date_start, scenario), y = 1-value/novac, x = date_start, color = scenario)) +
#   geom_boxplot(outlier.shape = 1) +
#   geom_vline(xintercept = seq(ymd("2019-12-15"), ymd("2022-01-15"), by = "month"), linetype = 2) +
#   facet_wrap(~name, nrow = 4) +
#   theme_bw() +
#   # lims(y = c(0, 0.9)) +
#   theme(legend.position = "top",
#         strip.background = element_rect(fill = NA, color = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   labs(color = "Vaccine Roll-out Rate",
#        x = "Vaccine Roll-out Starting Date",
#        y = "Relative Reduction\n(1-outcome w/ vaccine/outcome w/o vaccine)") +
#   scale_color_futurama() +
#   custom_theme

# ggsave("figs/epi_outcomes_vertical_v4.png", width = 15, height = 8)
# ggsave("figs/fig2.png", width = 15, height = 8)

# res_ext$az%>%
#   bind_rows(.id = "outcome_type") %>%
#   dplyr::select(-iso3c) %>%
#   group_by(scenario_id, population, name) %>%
#   summarise(value = sum(value),
#             novac = sum(novac)) %>%
#   filter(!grepl("_p_", name)) %>%
#   left_join(ms_scenarios, # %>%
#             # rownames_to_column(var = "scenario_id"),
#             by = "scenario_id") %>%
#   mutate(scenario = factor(scenario,
#                            levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast")),
#          name = factor(name,
#                        levels = c("cases", "severe_i_all", "critical_i_all", "death_o_all"),
#                        labels = c("All Cases", "Severe Cases", "Critical Cases", "Deaths"))) %>%
#   filter(name %in% c("All Cases", "Deaths"))%>%
#   # mutate(date_start = factor(date_start,
#   #                            levels = as.character(seq(ymd("2021-01-01"),ymd("2021-12-01"),"month")))) %>%
#   ggplot(., aes(x = (1-value/novac),
#                 y = date_start,
#                 group = interaction(date_start, scenario),
#                 fill = scenario,
#                 color = scenario)) +
#   geom_density_ridges() +
#   # geom_boxplot(aes(group = interaction(scenario, date_start)), outlier.shape = NA) +
#   # geom_point() +
#   # geom_smooth(aes(fill = scenario)) +
#   # geom_boxplot(outlier.shape = NA) +
#   facet_grid(scenario~name) +
#   theme_bw() +
#   # lims(y = c(0, 0.9)) +
#   theme(legend.position = "top",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   #geom_vline(xintercept = seq(ymd("2019-12-15"), ymd("2022-01-15"), by = "month"), linetype = 2) +
#   labs(color = "",
#        fill = "",
#        y = "Vaccine Roll-out Start Date",
#        x = "Relative Reduction\n1 - Outcomes w/ Vaccines / Outcomes w/o Vaccines") +
#   scale_color_futurama() +
#   scale_fill_futurama(alpha = 0.1) +
#   custom_theme

# ggsave("figs/epi_outcomes_horizontal_v3_2.png", width = 15, height = 8)


