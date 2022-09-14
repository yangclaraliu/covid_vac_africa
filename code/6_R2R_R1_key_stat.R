ICER$az_03 %>%
  mutate(Type = "Viral Vector Vaccines") %>%
  bind_rows(ICER$pf_03 %>%
              mutate(Type = "mRNA Vaccines")) %>%
  filter(econ_id == 1) %>%
  # filter(scenario == "fast", Type == "Viral Vector Vaccines", date_start == "2021-01-01")
  mutate(scenario = factor(scenario, levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast")),
         ICER_factor=cut(ICER_scaled,
                         breaks=c(-0.5, 0.1, 0.3, 0.5, 1, 16),
                         labels=c("<0.1", "0.1-0.3", "0.3-0.5",
                                  "0.5-1", ">1"))) %>%
  filter(date_start < ymd("2021-07-01")) |> 
  left_join(group_income, by = "iso3c") |> 
  group_by(`Income Group`, Type) |> 
  summarise(ICER_scaled_mu = mean(ICER_scaled)) |> 
  pivot_wider(names_from = Type, values_from = ICER_scaled_mu) 
