ICER_all$az_05_ext %>% mutate(version = "ext", Type = "az") %>% 
  bind_rows(ICER_all$az_05 %>% mutate(version = "bc", Type = "az")) %>% 
  bind_rows(ICER_all$pf_05_ext %>% mutate(version = "ext", Type = "pf")) %>% 
  bind_rows(ICER_all$pf_05 %>% mutate(version = "bc", Type = "pf")) %>% 
  left_join(group_income, by = "iso3c") %>%
  filter(econ_id == 1) %>% 
  mutate(month = month(date_start)) |>
  select(iso3c, Type, month, scenario, ICER_scaled, version, econ_id) |> 
  mutate(econ_id = factor(econ_id,
                          labels = c("Time horizon: by 30/06/2023")),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccines",
                                  "mRNA vaccines")),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow",
                                      "Medium",
                                      "Fast"))#,
         # m = factor(m, levels = 1:12)
         ) |> 
  mutate(ICER_scaled= cut(ICER_scaled,
                       breaks = c(-Inf, 0.1, 0.3, 0.5, 1, Inf),
                       labels = c("<0.1",
                                  "0.1-0.3",
                                  "0.3-0.5",
                                  "0.5-1",
                                  ">1"))) |>
  pivot_wider(names_from = version, values_from = ICER_scaled) |> 
  group_by(ext, bc, Type) |> tally() |> #pull(n) |> sum()
  # mutate(dia = ext == bc) |> #pull(n) |> range() # -> y# |> group_by(Type) |> summarise(n = sum(n))
  ggplot(aes(x = bc, y = ext, fill = n)) +
  geom_tile(color = "black") +
  # scale_size_manual(values = c(0.5,1)) +
  facet_wrap(~Type) +
  viridis::scale_fill_viridis(option = "magma", 
                              direction = -1, 
                              limits = c(1,500)) +
  facet_wrap(~Type)+
  theme_bw() +
  custom_theme +
  labs(x = "Baseline ICER relative to GDP-per-capita",
       y = "ICER relative to GDP-per-capita\nextended time horizon") +
  theme(legend.position = "top") -> p1

ICER_all$az_05_low %>% mutate(version = "low", Type = "az") %>% 
  bind_rows(ICER_all$az_05 %>% mutate(version = "bc", Type = "az")) %>% 
  bind_rows(ICER_all$pf_05_low %>% mutate(version = "low", Type = "pf")) %>% 
  bind_rows(ICER_all$pf_05 %>% mutate(version = "bc", Type = "pf")) %>% 
  left_join(group_income, by = "iso3c") %>% 
  mutate(month = month(date_start)) %>% 
  select(iso3c, Type, month, scenario, ICER_scaled, version, econ_id) |> 
  filter(econ_id == 1) %>% 
  mutate(ICER_scaled= cut(ICER_scaled,
                          breaks = c(-Inf, 0.1, 0.3, 0.5, 1, Inf),
                          labels = c("<0.1",
                                     "0.1-0.3",
                                     "0.3-0.5",
                                     "0.5-1",
                                     ">1"))) |> 
  pivot_wider(names_from = version, values_from = ICER_scaled) %>% 
  mutate(econ_id = factor(econ_id,
                          labels = c("Lower bounds VE estimates")),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccines",
                                  "mRNA vaccines")),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow",
                                      "Medium",
                                      "Fast"))) |> 
  group_by(low, bc, Type) |> tally() |>
  # mutate(dia = low == bc) |> # pull(n) |> range()
  ggplot(aes(x = bc, y = low, fill = n)) +
  geom_tile(color = "black") +
  # scale_size_manual(values = c(0.5,1)) +
  facet_wrap(~Type) +
  viridis::scale_fill_viridis(option = "magma", 
                              limits = c(1,500),
                              direction = -1) +
  facet_wrap(~Type)+
  theme_bw() +
  custom_theme +
  labs(x = "Baseline ICER relative to GDP-per-capita",
       y = "ICER relative to GDP-per-capita\nlow vaccine efficacy set",
       fill = "Counts") +
  theme(legend.position = "top",
        legend.key.width = unit(3, 'cm')) -> p2

plot_grid(get_legend(p2),
          p1 + theme(legend.position = "none"), 
          p2 + theme(legend.position = "none"), 
          ncol = 1,
          rel_heights = c(1,10,10))

ggsave("figs/R2R_R1/fig4_v2.png",
       width = 10, height = 10)

ICER_all$az_05_ext %>% mutate(version = "ext", Type = "az") %>% 
  bind_rows(ICER_all$az_05 %>% mutate(version = "bc", Type = "az")) %>% 
  bind_rows(ICER_all$pf_05_ext %>% mutate(version = "ext", Type = "pf")) %>% 
  bind_rows(ICER_all$pf_05 %>% mutate(version = "bc", Type = "pf")) %>% 
  left_join(group_income, by = "iso3c") %>%
  filter(econ_id == 1) %>% 
  mutate(month = month(date_start)) |>
  select(iso3c, Type, month, scenario, ICER_scaled, version, econ_id) |> 
  mutate(econ_id = factor(econ_id,
                          labels = c("Time horizon: by 30/06/2023")),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccines",
                                  "mRNA vaccines")),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow",
                                      "Medium",
                                      "Fast"))#,
         # m = factor(m, levels = 1:12)
  ) |> 
  # mutate(ICER_scaled= cut(ICER_scaled,
  #                         breaks = c(-Inf, 0.1, 0.3, 0.5, 1, Inf),
  #                         labels = c("<0.1",
  #                                    "0.1-0.3",
  #                                    "0.3-0.5",
  #                                    "0.5-1",
  #                                    ">1")),
  #        ICER_scaled = factor(ICER_scaled,
  #                             levels = c("<0.1",
  #                                        "0.1-0.3",
  #                                        "0.3-0.5",
  #                                        "0.5-1",
  #                                        ">1"))) |>
  pivot_wider(names_from = version, values_from = ICER_scaled) |> 
  mutate(ext_cut= cut(ext,
                      breaks = c(-Inf, 0.1, 0.3, 0.5, 1, Inf),
                      labels = c("<0.1",
                                 "0.1-0.3",
                                 "0.3-0.5",
                                 "0.5-1",
                                 ">1")),
         bc_cut= cut(bc,
                      breaks = c(-Inf, 0.1, 0.3, 0.5, 1, Inf),
                      labels = c("<0.1",
                                 "0.1-0.3",
                                 "0.3-0.5",
                                 "0.5-1",
                                 ">1"))) |> 
  filter(ext_cut != bc_cut) |> 
  # group_by(ext, bc, Type) |> tally() |> 
  # mutate(dia = ext == bc) |> #pull(n) |> range()
  # mutate(bias = case_when(ext %in% c("<0.1",
  #                                    "0.1-0.3",
  #                                    "0.3-0.5") &
  #                           bc %in% c("0.5-1",
  #                                     ">1") ~ "over",
  #                         bc %in% c("<0.1",
  #                                    "0.1-0.3",
  #                                    "0.3-0.5") &
  #                           ext %in% c("0.5-1",
  #                                     ">1") ~ "under")) |>
  mutate(bias = if_else(ext > bc, "under", "over")) |> 
  # filter(!is.na(bias)) |> 
  group_by(Type, month, bias) |> tally() |> 
  ggplot(aes(x = month, y = n, group = Type, color = Type)) +
  geom_line() +facet_grid(~bias)
