#### response to reviewers: R1####

##### are we penalising shorter programs? ####
res$pfizer$fatal |> 
  group_by(scenario_id, population, name) |> 
  summarise(value = sum(value),
            novac = sum(novac)) |> 
  mutate(vac_name = "mrna") |> 
  bind_rows(res$az$fatal |> 
              group_by(scenario_id, population, name) |> 
              summarise(value = sum(value),
                        novac = sum(novac)) |> 
              mutate(vac_name = "vv")
  ) -> fatal


res$pfizer$non_fatal |> 
  group_by(scenario_id, population, name)|> 
  summarise(value = sum(value),
            novac = sum(novac)) |> 
  mutate(vac_name = "mrna") |> 
  bind_rows(res$az$non_fatal |> 
              group_by(scenario_id, population, name) |> 
              summarise(value = sum(value),
                        novac = sum(novac)) |> 
              mutate(vac_name = "vv")
  ) -> non_fatal

all_health <- bind_rows(fatal, non_fatal)



all_health |> 
  filter(!grepl("_p_",name)) |> 
  mutate(diff = novac - value) |> 
  left_join(cost_all |> 
              dplyr::select(scenario_id, population, vac_cost, Type) |> 
              arrange(population) |> 
              rename(vac_name = Type) |> 
              mutate(vac_name = if_else(vac_name == "pfizer","mrna","vv")),
            by = c("scenario_id", "population","vac_name")) |> 
  left_join(ms_scenarios, by = "scenario_id") |> 
  mutate(outcome_per_vac = diff/vac_cost,
         date_start = factor(date_start,
                             levels = seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "month") |> 
                               as.character()),
         vac_name = factor(vac_name,
                           levels = c("mrna", "vv"),
                           labels = c("mRNA Vaccine",
                                      "Viral Vector Vaccine")),
         name = factor(name,
                       levels = c("cases", "severe_i_all", "critical_i_all", "death_o_all"),
                       labels = c("Symptomatic Infections",
                                  "Severe Cases",
                                  "Critical Cases",
                                  "Fatal Cases"))) |> 
  pivot_longer(cols = c("outcome_per_vac","diff"),
               values_to = "value_compare",
               names_to = "adjustment") |> 
  mutate(adjustment = factor(adjustment,
                             levels = c("outcome_per_vac",
                                        "diff"),
                             labels = c("Adjusted",
                                        "Unadjusted"))) -> p_table

p_table  |> 
  # filter(vac_name == "mRNA Vaccine") |> 
  filter(vac_name == "Viral Vector Vaccine") |> 
  ggplot()+
  geom_jitter(aes(x = date_start, y = value_compare, color = scenario)) +
  geom_smooth(aes(x = date_start, y = value_compare, color = scenario, 
                  group = scenario, fill = scenario)) +
  facet_wrap(adjustment ~ name, scales = "free", ncol = 4) +
  scale_color_futurama() +
  scale_fill_futurama() +
  theme_bw() +
  labs(y = "Outcome saved from the no vaccination scenario",
       x = "Vaccination program starting date") +
  custom_theme +
  theme(axis.text.x = element_text(angle = 90)) -> p_tmp

ggsave(filename = "figs/R2R_R1/outcome_adjusted_vv.png",
       plot = p_tmp,
       width = 20, height = 10)

##### can we say some programs are better because of the marginal ICER thing? #####
ICER$az_03 |> 
  dplyr::select(scenario_id, econ_id, iso3c, Type, scenario, date_start,
                GDPPC_2020_USD, ICER, ICER_scaled) |> 
  bind_rows(ICER$pf_03 |> 
              dplyr::select(scenario_id, econ_id, iso3c, Type, scenario, date_start,
                            GDPPC_2020_USD, ICER, ICER_scaled)) -> ICER_res

res
