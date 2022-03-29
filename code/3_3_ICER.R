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
  # filter(econ_id == 1) %>% 
  mutate(diff_health = dalys_novac - dalys,
         diff_cost = tot_cost- tot_cost_novac,
         ICER = diff_cost/diff_health,
         ICER_scaled = ICER/GDPPC_2020_USD,
         ICER_scaled_bin = if_else(ICER_scaled >= 1, F, T)) -> ICER[["az"]]

# tab$pfizer %>% 
#   dplyr::select(scenario_id, econ_id, iso3c, Type, tot_cost, tot_cost_novac, dalys) %>% 
#   distinct() %>% 
#   left_join(tab[["novac"]],
#             by = c("scenario_id",
#                    "econ_id",
#                    "iso3c" = "country")) %>% 
#   left_join(ms_scenarios, by = "scenario_id") %>% 
#   # filter(econ_id == 1) %>% 
#   mutate(diff_health = dalys_novac - dalys,
#          diff_cost = tot_cost - tot_cost_novac,
#          ICER = diff_cost/diff_health,
#          ICER_scaled = ICER/GDPPC_2020_USD,
#          ICER_scaled_bin = if_else(ICER_scaled >= 0.5, F, T)) -> ICER[["pfizer"]]
# 
# ICER$az %>%
#   group_by(scenario_id, econ_id) %>% 
#   mutate(GDP_rank = rank(GDPPC_2020_USD),
#          GDP_label = paste0(GDP_rank, "_",iso3c)) -> tmp 
# 
# tmp %>% 
#   ungroup %>% 
#   select(iso3c, GDP_rank, GDP_label) %>% 
#   arrange(GDP_rank) %>% 
#   distinct() %>%  
#   pull(GDP_label) -> tmp_labels
# 
# tmp %>% 
#   mutate(GDP_label = factor(GDP_label, levels = tmp_labels)) %>% 
#   ggplot(., aes(x = diff_health, y = diff_cost, color = date_start)) +
#   geom_point(aes(pch = scenario)) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~GDP_label, scales = "free") +
#   geom_abline(aes(intercept = 0, slope = GDPPC_2020_USD*0.5))

ICER$az %>% 
  mutate(Type = "AZ") %>% 
  bind_rows(ICER$pfizer %>% 
              mutate(Type = "Pfizer")) %>% 
  group_by(date_start, econ_id, scenario, Type) %>% 
  summarise(ICER_CE = sum(ICER_scaled_bin)) %>% 
  mutate(ICER_CE_prop = ICER_CE/nrow(fitted_table),
         scenario = factor(scenario, levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast")),
         econ_id = factor(econ_id, levels = 1:3,
                          labels = c("Discount Rate = 0.03\nSMR = 1",
                                     "Discount Rate = 0\nSMR = 1",
                                     "Discount Rate = 0.03\nSMR = 1.5"))) %>% 
  ggplot(., aes(x = date_start, y = ICER_CE_prop, group = scenario, color = scenario)) +
  geom_line() +
  geom_smooth(alpha = 0.5, aes(fill = scenario)) +
  facet_grid(econ_id~Type) +
  labs(x = "Vaccine Roll-out Start Date",
      y = "Proportion of Countries with Cost-Effective Strategies") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(color = "", fill= "") +
  scale_color_futurama() +
  scale_fill_futurama()

ggsave("figs/prop_CE.png", height = 10, width = 15)
