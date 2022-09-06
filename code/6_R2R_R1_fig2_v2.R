res <- read_rds("data/res.rds")
impact <- read_rds("data/intermediate/impact.rds")

res$az %>%
  bind_rows(.id = "outcome_type") %>%
  dplyr::select(-iso3c) %>%
  group_by(scenario_id, population, name) %>%
  summarise(value = sum(value),
            novac = sum(novac)) %>%
  filter(!grepl("_p_", name)) %>%
  left_join(ms_scenarios %>%
            rownames_to_column(var = "scenario_id"),
            by = "scenario_id") %>%
  mutate(scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast")),
         name = factor(name,
                       levels = c("cases", "severe_i_all", "critical_i_all", "death_o_all"),
                       labels = c("All Cases", "Severe Cases", "Critical Cases", "Deaths"))) %>%
  filter(name %in% c("All Cases", "Deaths")) %>%
  mutate(rr = 1 - value/novac,
         country = countrycode(population, "country.name", "iso3c")) -> tmp1

impact$az |> 
  filter(econ_id == 1) |> 
  select(epi_id, country, dalys) |> 
  left_join(impact$novac |> 
              filter(econ_id == 1) |> 
              select(epi_id, country, dalys) |> 
              rename(dalys_novac = dalys),
            by = c("epi_id", "country")
  ) |> 
  mutate(rr = 1 - dalys/dalys_novac) -> tmp2

tmp1 |> 
  filter(name == "Deaths") |> 
  mutate(name = "DALYs") |> 
  select(-rr, -value, -novac) |> 
  left_join(tmp2 |> 
              rename(scenario_id = epi_id),
            by = c("scenario_id","country")) |> 
  rename(value = dalys,
         novac = dalys_novac) |> 
  select(colnames(tmp1)) -> tmp3

tmp <- bind_rows(tmp1, tmp3) |> 
  mutate(name = factor(name,
                       levels = c("All Cases",
                                  "Deaths",
                                  "DALYs")))

ggplot(tmp, aes(group = interaction(date_start, scenario), 
              y = 1-value/novac, x = date_start, color = scenario)) +
  geom_boxplot(outlier.shape = 1) +
  geom_vline(xintercept = seq(ymd("2020-12-15"), ymd("2022-01-15"), by = "month"), linetype = 2) +
  facet_wrap(~name, nrow = 4) +
  theme_bw() +
  # lims(y = c(0, 0.9)) +
  theme(legend.position = "top",
        strip.background = element_rect(fill = NA, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(color = "Vaccine Roll-out Rate",
       x = "Vaccine Roll-out Start Date",
       y = "Relative Reduction\n(1-outcome with vaccination/outcome without vaccination)") +
  scale_color_futurama() +
  scale_x_continuous(breaks = seq(ymd("2021-01-01"), ymd("2021-12-01"), "month"),
                     labels = paste0(month(seq(ymd("2021-01-01"), 
                                               ymd("2021-12-01"), "month"), 
                                           label = T), "\n2021"),
                     limits = c(ymd("2020-12-15"), 
                                ymd("2021-12-15"))) +
  custom_theme

ggsave("figs/R2R_R1/fig2_az_v2.png", width = 15, height = 15)
