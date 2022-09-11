impact <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/impact.rds")
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
            by = c("scenario_id", "iso3c" = "country")) -> tab[["pf"]]

impact$novac %>% 
  rename(scenario_id = epi_id) %>% 
  # dplyr::select(scenario_id, econ_id, country, ylds, ylls, dalys,GDPPC_2020_USD) %>% 
  rename(dalys_novac = dalys) -> tab[["novac"]]


tmp <- list()

tab[["az"]] %>% 
    # dplyr::select(scenario_id, econ_id, iso3c, Type, tot_cost, tot_cost_novac, dalys) %>%
  dplyr::select(-ylls,-ylds,-vsl,-human_capital) |> 
    # distinct() %>% 
    left_join(tab[["novac"]],
              by = c("scenario_id",
                     "econ_id",
                     "GDPPC_2020_USD",

                     "iso3c" = "country")) %>% 
    left_join(ms_scenarios,#  |> 
              # rownames_to_column(var = "scenario_id"), 
              by = c("scenario_id",
                     "cov",
                     "cov_ext",
                     "scenario",
                     "date_start",
                     "date_vac_end",
                     "date_vac_end_ext")) %>% 
    # filter(econ_id == 1) %>% 
    mutate(diff_health = dalys_novac - dalys,
           diff_cost = tot_cost- tot_cost_novac,
           ICER = diff_cost/diff_health,
           ICER_scaled = ICER/GDPPC_2020_USD,
           
           ICER_05 = 0.5*GDPPC_2020_USD,
           price_reduction_05 = if_else(ICER_scaled <= 0.5, F, T),
           target_diff_cost_05 = ICER_05*diff_health,
           cost_diff_05 = target_diff_cost_05 - diff_cost,
           price_reduction_05_vac = cost_diff_05/vac_cost,
           
           ICER_04 = 0.4*GDPPC_2020_USD,
           price_reduction_04 = if_else(ICER_scaled <= 0.4, F, T),
           target_diff_cost_04 = ICER_04*diff_health,
           cost_diff_04 = target_diff_cost_04 - diff_cost,
           price_reduction_04_vac = cost_diff_04/vac_cost,
           
           ICER_03 = 0.3*GDPPC_2020_USD,
           price_reduction_03 = if_else(ICER_scaled <= 0.3, F, T),
           target_diff_cost_03 = ICER_03*diff_health,
           cost_diff_03 = target_diff_cost_03 - diff_cost,
           price_reduction_03_vac = cost_diff_03/vac_cost,
           
           ICER_02 = 0.2*GDPPC_2020_USD,
           price_reduction_02 = if_else(ICER_scaled <= 0.2, F, T),
           target_diff_cost_02 = ICER_02*diff_health,
           cost_diff_02 = target_diff_cost_02 - diff_cost,
           price_reduction_02_vac = cost_diff_02/vac_cost,
           
           ICER_01 = 0.1*GDPPC_2020_USD,
           price_reduction_01 = if_else(ICER_scaled <= 0.1, F, T),
           target_diff_cost_01 = ICER_01*diff_health,
           cost_diff_01 = target_diff_cost_01 - diff_cost,
           price_reduction_01_vac = cost_diff_01/vac_cost) -> tmp[["az"]]

tab[["pf"]] %>% 
  # dplyr::select(scenario_id, econ_id, iso3c, Type, tot_cost, tot_cost_novac, dalys) %>%
  dplyr::select(-ylls,-ylds,-vsl,-human_capital) |> 
  # distinct() %>% 
  left_join(tab[["novac"]],
            by = c("scenario_id",
                   "econ_id",
                   "GDPPC_2020_USD",
                   
                   "iso3c" = "country")) %>% 
  left_join(ms_scenarios,#  |> 
            # rownames_to_column(var = "scenario_id"), 
            by = c("scenario_id",
                   "cov",
                   "cov_ext",
                   "scenario",
                   "date_start",
                   "date_vac_end",
                   "date_vac_end_ext")) %>% 
  # filter(econ_id == 1) %>% 
  mutate(diff_health = dalys_novac - dalys,
         diff_cost = tot_cost- tot_cost_novac,
         ICER = diff_cost/diff_health,
         ICER_scaled = ICER/GDPPC_2020_USD,
         
         ICER_05 = 0.5*GDPPC_2020_USD,
         price_reduction_05 = if_else(ICER_scaled <= 0.5, F, T),
         target_diff_cost_05 = ICER_05*diff_health,
         cost_diff_05 = target_diff_cost_05 - diff_cost,
         price_reduction_05_vac = cost_diff_05/vac_cost,
         
         ICER_04 = 0.4*GDPPC_2020_USD,
         price_reduction_04 = if_else(ICER_scaled <= 0.4, F, T),
         target_diff_cost_04 = ICER_04*diff_health,
         cost_diff_04 = target_diff_cost_04 - diff_cost,
         price_reduction_04_vac = cost_diff_04/vac_cost,
         
         ICER_03 = 0.3*GDPPC_2020_USD,
         price_reduction_03 = if_else(ICER_scaled <= 0.3, F, T),
         target_diff_cost_03 = ICER_03*diff_health,
         cost_diff_03 = target_diff_cost_03 - diff_cost,
         price_reduction_03_vac = cost_diff_03/vac_cost,
         
         ICER_02 = 0.2*GDPPC_2020_USD,
         price_reduction_02 = if_else(ICER_scaled <= 0.2, F, T),
         target_diff_cost_02 = ICER_02*diff_health,
         cost_diff_02 = target_diff_cost_02 - diff_cost,
         price_reduction_02_vac = cost_diff_02/vac_cost,
         
         ICER_01 = 0.1*GDPPC_2020_USD,
         price_reduction_01 = if_else(ICER_scaled <= 0.1, F, T),
         target_diff_cost_01 = ICER_01*diff_health,
         cost_diff_01 = target_diff_cost_01 - diff_cost,
         price_reduction_01_vac = cost_diff_01/vac_cost) -> tmp[["pf"]]

tmp |> 
  bind_rows(.id = "Type") |> 
  # filter(date_start == "2021-08-01") |> 
  filter(econ_id == 1) |> 
  dplyr::select(starts_with("price_reduction") & ends_with("_vac"), Type, scenario, date_start) |> 
  pivot_longer(cols = starts_with("price")) |> 
  mutate(name = parse_number(name)/10,
         name = factor(name, levels = seq(0.1,0.5,0.1)),
         value = if_else(value > 0, 0, value),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccines",
                                  "mRNA vaccines"))) |>
  group_by(Type, name) |> 
  mutate(value_md = median(value),
         value_mu = mean(value)) |> 
  ungroup() |> 
  dplyr::select(Type, name, date_start, scenario, value_md, value_mu) |>
  distinct() |>
  group_by(Type) |> group_split() |> map(pull, value_mu) |> map(range)
  
  ggplot() +
  geom_density(aes(x = value, group = name, color = name)) +
  geom_vline(aes(xintercept = value_mu,
                 color = name),
             linetype = 2,
             size = 2) +
  facet_wrap(~Type, scales = "free", ncol = 1) +
  theme_cowplot() +
  custom_theme +
  labs(y = "Density",
       x = "% reduction in vaccine unit costs required",
       color = "Mean % reduction in vaccine unit costs required\ntargeting different willingness-to-pay thresholds",
       title = "Start date = pooled, Roll-out rate = pooled"
       ) +
  theme(legend.position = "top") +
  scale_color_lancet()

ggsave("figs/R2R_R1/target_price_reduction_all.png",
       width = 10, height = 10)
