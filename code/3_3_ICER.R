tab <- list()

cost_all %>% 
  filter(Type == "az") %>% 
  left_join(impact$az %>% 
              rename(scenario_id = epi_id), 
            by = c("scenario_id", "iso3c" = "country")) -> tab[["az"]]

cost_all %>% 
  filter(Type == "pfizer") %>% 
  left_join(impact$pfizer %>% 
              rename(scenario_id = epi_id), 
            by = c("scenario_id", "iso3c" = "country")) -> tab[["pfizer"]]

impact$novac %>% 
  rename(scenario_id = epi_id) %>% 
  dplyr::select(scenario_id, econ_id, country, dalys,GDPPC_2020_USD) %>% 
  rename(dalys_novac = dalys) -> tab[["novac"]]

ICER <- list()

tab$az %>% 
  dplyr::select(scenario_id, econ_id, iso3c, Type, tot_cost, tot_cost_novac, dalys) %>% 
  distinct() %>% 
  left_join(tab[["novac"]],
            by = c("scenario_id",
                   "econ_id",
                   "iso3c" = "country")) %>% 
  left_join(ms_scenarios, by = "scenario_id") %>% 
  filter(econ_id == 1) %>% 
  mutate(diff_health = dalys_novac - dalys,
         diff_cost = tot_cost- tot_cost_novac,
         ICER = diff_cost/diff_health,
         ICER_scaled = ICER/GDPPC_2020_USD,
         ICER_scaled_bin = if_else(ICER_scaled >= 1, F, T)) -> ICER[["az"]]

tab$pfizer %>% 
  dplyr::select(scenario_id, econ_id, iso3c, Type, tot_cost, tot_cost_novac, dalys) %>% 
  distinct() %>% 
  left_join(tab[["novac"]],
            by = c("scenario_id",
                   "econ_id",
                   "iso3c" = "country")) %>% 
  left_join(ms_scenarios, by = "scenario_id") %>% 
  filter(econ_id == 1) %>% 
  mutate(diff_health = dalys_novac - dalys,
         diff_cost = tot_cost- tot_cost_novac,
         ICER = diff_cost/diff_health,
         ICER_scaled = ICER/GDPPC_2020_USD,
         ICER_scaled_bin = if_else(ICER_scaled >= 0.5, F, T)) -> ICER[["pfizer"]]

ICER %>% 
  ggplot(., aes(x = diff_health, y = diff_cost, color = ICER_scaled_bin)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_grid(scenario ~ date_start)

ICER$pfizer %>% 
  ggplot(., aes(x = date_start, y = iso3c, fill = ICER_scaled_bin)) +
  geom_tile() +
  facet_wrap(~scenario)
