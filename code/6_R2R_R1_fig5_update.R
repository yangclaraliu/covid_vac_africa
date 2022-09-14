ICER$az_05 %>% 
  mutate(Type = "az") %>% 
  bind_rows(ICER$pf_05 %>% 
              mutate(Type = "pf")) %>% 
  left_join(cost_hc_expenditure_GHED, by = "iso3c") %>% 
  # left_join(ms_cov_all %>% mutate(Type = tolower(Type)), by = c("iso3c", "date_start", "scenario", "Type")) %>% 
  mutate(affordability = (diff_cost/2)/hc_expenditure, #dividing 
         scenario = factor(scenario, 
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast")),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccine", 
                       "mRNA vaccine")),
         date_start = ymd(date_start)) %>% 
  filter(!is.na(affordability)) %>% 
  filter(econ_id == 1) |> 
  mutate(ICER_cat = cut(ICER_scaled,
                        breaks = c(-Inf, 0, seq(0.1,0.5,0.1), 1, Inf)),
         affordability_cat = cut(affordability,
                                 breaks = c(-Inf, 0, seq(0.1,0.5,0.1), 1, Inf))
            # ICER_scaled_comp = if_else(ICER_scaled > 1, 
            #                         1.2, 
            #                         ICER_scaled),
         # ICER_cat = case_when(ICER_scaled < 0.1 ~ "<0.1",
         #                      ICER_scaled >= 0.1 & ICER_scaled < 0.3 ~ "0.1-0.3",
         #                      ICER_scaled >= 0.3 & ICER_scaled < 0.5 ~ "0.3-0.5",
         #                      ICER_scaled >= 0.5 & ICER_scaled < 1 ~ "0.5-1",
         #                      ICER_scaled >= 1 ~ ">1"),
         # ICER_cat = factor(ICER_cat,
         #                   levels = c("<0.1",
         #                              "0.1-0.3",
         #                              "0.3-0.5",
         #                              "0.5-1",
         #                              ">1")),
         # affordability_comp = if_else(affordability > 1,
         #                              1.2, 
         #                              affordability)
         )|> 
  group_by(ICER_cat, affordability_cat, Type) |> tally() |> 
  mutate(pat = case_when(ICER_cat %in% c("(0.3,0.4]", 
                                         "(0,0.1]",
                                         "(0.1,0.2]",
                                         "(0.2,0.3]",
                                         "(0.4,0.5]") &
                           affordability_cat %in% c("(0.3,0.4]", 
                                                    "(0.1,0.2]", 
                                                    "(0.2,0.3]",
                                                    "(0.4,0.5]") ~ "Mixed",
                         ICER_cat %in% c("(0.5,1]",
                                         "(1, Inf]") |
                           affordability_cat %in% c("(0.5,1]",
                                                    "(1, Inf]") ~ "Negative",
                         TRUE ~ "Positive"),
         pat = factor(pat,
                      levels = c("Positive",
                                 "Mixed",
                                 "Negative"))) -> tmp_tab
  

tmp_tab |> 
  group_by(Type, pat) |> 
  summarise(tot = sum(n)) |> 
  group_by(Type) |> 
  mutate(all_comb = sum(tot)) |> mutate(p = tot/all_comb) |> View()

tmp_tab  |> 
  ggplot(aes(x = ICER_cat, y = affordability_cat)) +
  geom_tile_pattern(aes(pattern = pat), 
                    fill = "white",
                    color = "black",
                    pattern_fill = "white") +
  scale_pattern_manual(values = c("none", "stripe", "circle")) + 
  labs(pattern = "Qualitative conclusion") +
  custom_theme +
  theme(legend.key.size = unit(1.5, 'cm'),
        legend.position = "top")-> p_tmp
legend1 <- get_legend(p_tmp)

tmp_tab  |> 
  ggplot(aes(x = ICER_cat, y = affordability_cat)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma", direction = -1) +
  labs(fill = "Counts") +
  custom_theme +
  theme(legend.key.width = unit(1.5, 'cm'),
        legend.position = "top") -> p_tmp
legend2 <- get_legend(p_tmp)

# filter(affordability > 1 & ICER_scaled < 0.5)
  # mutate(date_start = ymd(date_start)) %>% 
  # filter(date_start == "2021-02-01", !is.na(affordability)) %>% 
  # ggplot(., aes(x = affordability, y = scenario,  
  #               fill = scenario, 
  #               color = scenario)) +
tmp_tab  |> 
  # filter(affordability > 1)
  # filter(ICER_scaled <= 1) |> 
  # filter(date_start %in% ymd(c("2021-01-01",
  #                              "2021-08-01",
  #                              "2021-12-01"))) |> 
  # mutate(date_start = factor(date_start)) |> 
  ggplot(aes(x = ICER_cat, y = affordability_cat)) +
  geom_tile_pattern(aes(fill = n,  pattern = pat), 
                    pattern_fill = "white") +
  # scale_color_manual(values = c("black", "red")) +
  scale_pattern_manual(values = c("none", "stripe", "circle")) +
  # guides(fill = guide_legend(override.aes = list(fill = "white")))
  # geom_bin2d(bins = 50)+
  
  # geom_boxplot() +
  # geom_point(aes(color = date_start,
  #                pch = scenario),
  #            size = 2,
  #            alpha = 0.5) +
  # scale_x_log10() +
  # scale_y_log10() +
  facet_wrap(~Type, ncol = 1) +
  # geom_hline(yintercept = seq(0,0.5,0.1),
  #            linetype = 2) +
  # geom_vline(xintercept = seq(0,0.5,0.1),
  #          linetype = 2)
  # geom_density_ridges(alpha = 0.01) +
  # geom_histogram() +
  # geom_density_ridges(alpha = 0.5, scale = 0.95, stat = "binline", binwidth = 0.02) +
  # geom_density(alpha = 0.1) +
  # facet_grid(date_start~Type) +
  # facet_grid( ~ Type, scales = "free") +
  # facet_grid(scenario ~ Type) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(color = "",
       fill = "",
       y = "Affordability\nIncremental Costs/General Healthcare Expenditure",
       x = "ICER in relation to GDP per capita by country"
       # y = "Frequency"
  ) +
  # scale_color_futurama() +
  scale_fill_viridis(option = "magma", direction = -1) +
  custom_theme +
  theme(panel.grid = element_blank()) -> p
  # geom_vline(xintercept = 0, linetype = 2)

plot_grid(legend1, legend2, p+ coord_fixed(ratio = 1), ncol = 1,
          rel_heights = c(1,1,15)) -> p_save
ggsave("figs/R2R_R1/fig5_updated.png",
       width = 8, height = 16)

# regression model
ICER$az_05 %>% 
  mutate(Type = "az") %>% 
  bind_rows(ICER$pf_05 %>% 
              mutate(Type = "pf")) %>% 
  left_join(cost_hc_expenditure_GHED, by = "iso3c") %>% 
  # left_join(ms_cov_all %>% mutate(Type = tolower(Type)), by = c("iso3c", "date_start", "scenario", "Type")) %>% 
  mutate(affordability = (diff_cost/2)/hc_expenditure, #dividing 
         scenario = factor(scenario, 
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast")),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccine", 
                                  "mRNA vaccine")),
         date_start = ymd(date_start)) %>% 
  filter(!is.na(affordability)) %>% 
  filter(econ_id == 1) |> 
  filter(affordability > 0.1,
         ICER_scaled < 0.5) |> 
  pull(iso3c) |> unique() -> outliers

regtab |> 
  mutate(issue = if_else(iso3c %in% outliers, T, F)) -> regtab_log

cov_list <- c("pop","p_OA", "sero","vac_unit", "hc_expenditure", "`Income Group`")
f_issue <- paste("issue ~ ", cov_list) |> map(formula)

f_issue |> 
  map(glm,
      family = binomial(link = "logit"),
      data = regtab_log)r -> m_issue

m_issue |> 
  map(summary) |> 
  map(with, 1 - deviance/null.deviance)
