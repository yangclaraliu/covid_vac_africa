# shape %>%
#   mutate(fit = if_else(ISO3_CODE %in% fitted_table$iso3c, "Included", "Excluded")) %>%
#   st_as_sf() |> 
#   st_crop(shape_africa, 
#           xmin = -30, xmax = 60, 
#           ymin = -40, ymax = 40) -> shape_africa
# 
# ggplot(shape_africa) +
#   geom_sf(aes(fill = fit), color = "black", size = 0.3) +
#   theme_map() +
#   scale_fill_manual(values = c("white","grey")) +
#   theme(legend.position = "top",
#         legend.justification = "center") +
#   labs(fill = "") #-> p_map
# 
# ggsave("figs/R2R_R1/p_map_appendix.png",
#        width = 6, height = 10)

# shape %>%
#   mutate(fit = if_else(ISO3_CODE %in% fitted_table$iso3c, "Included", "Excluded")) %>%
#   st_as_sf() %>%
#   ggplot(.) +
#   geom_sf(aes(fill = fit), color = "black", size = 0.3, alpha = 0.7) +
#   coord_sf(xlim = c(-30, 60), ylim = c(-40, 40), expand = FALSE) +
#   theme_map() +
#   scale_fill_manual(values = c("white","#0D5257")) +
#   theme(legend.position = "top",
#         legend.justification = "center") +
#   labs(fill = "") -> p_map
# ggsave("figs/ISPH_fig2.png")

# VE_4plot <- read_csv("data/VE_base.csv") %>% 
#   mutate(dose = factor(dose),
#          outcome = factor(outcome,
#                           levels = c("Onward Transmission",
#                                      "Infection",
#                                      "Disease",
#                                      "Non-ICU hospital admission",
#                                      "ICU admission",
#                                      "Mortality"),
#                           labels = c("Onward\ntransmission",
#                                      "Infection",
#                                      "Disease",
#                                      "Severe\ncase",
#                                      "Critical\ncase",
#                                      "Mortality")),
#          Type = factor(Type,
#                        levels = c("viral vector","mRNA"),
#                        labels = c("Viral vector vaccines",
#                                   "mRNA vaccines")))
# 
# VE_4plot %>% 
#   # filter(Type == "mRNA", outcome == "Infection")
#   ggplot(., aes(x = value, y = outcome, color = Type)) +
#   geom_text(aes(label = dose, show_guide = T), fontface = "bold", size = 6) +
#   # ggplot(., aes(x = dose, y = value, color = Type)) +
#   # geom_point(aes(pch = outcome)) +
#   # geom_line(aes(linetype = outcome, group = interaction(Type, outcome))) +
#   # geom_line() +
#   # facet_grid( ~ Type) +
#   scale_color_manual(values = ggsci::pal_futurama()(5)[c(4,5)]) +
#   theme_bw() +
#   theme(# axis.text.x = element_text(angle = 90),
#         legend.position = "none") +
#   labs(y = "Disease outcomes",
#        x = "Vaccine effectiveness",
#        color = "Vaccine type") +
#   custom_theme +
#   scale_y_discrete(limits = rev) -> p_VE
# 
# p_VE_legend <- get_legend(p_VE + theme(legend.position = "right"))
# plot(p_VE_legend)

# ms_cov_all %>% 
#   left_join(vac_denom, by = "iso3c") %>% 
#   mutate(year_end1 = ymd("2021-12-31"),
#          year_end2 = ymd("2022-12-31"),
#          days_diff1_dose1 = as.numeric(year_end1 - date_start),
#          days_diff1_dose2 = as.numeric(year_end1 - date_start) - 28,
#          days_diff2_dose1 = as.numeric(year_end2 - date_start),
#          days_diff2_dose2 = as.numeric(year_end2 - date_start) - 28,
#          days_diff2_dose1 = days_diff2_dose1 - days_diff1_dose1,
#          days_diff2_dose2 = days_diff2_dose2 - days_diff1_dose2,
#          cov_achieved_diff1_dose1 = days_diff1_dose1 *r_vac,
#          cov_achieved_diff1_dose2 = days_diff1_dose2 *r_vac,
#          cov_achieved_diff2_dose1 = days_diff2_dose1 *r_vac,
#          cov_achieved_diff2_dose2 = days_diff2_dose2 *r_vac,
#          cov_achieved_diff2_dose1 = if_else(cov_achieved_diff1_dose1 + cov_achieved_diff2_dose1 > 0.7,
#                                             0.7 - cov_achieved_diff1_dose1,
#                                             cov_achieved_diff2_dose1),
#          cov_achieved_diff2_dose2 = if_else(cov_achieved_diff1_dose2 + cov_achieved_diff2_dose2 > 0.7,
#                                             0.7 - cov_achieved_diff1_dose2,
#                                             cov_achieved_diff2_dose2),
#          year1_doses = (cov_achieved_diff1_dose1 + cov_achieved_diff1_dose2)*tot*1000,
#          year2_doses = (cov_achieved_diff2_dose1 + cov_achieved_diff2_dose2)*tot*1000,
#          vac_unit_disc = vac_unit/((1+discount_r)),
#          vac_cost = year1_doses*vac_unit + year2_doses*(vac_unit_disc)) %>%
#   # dplyr::select(date_start, scenario, Type, name, iso3c, r_vac, Rate, vac_cost) %>% 
#   # dplyr::select(date_start, Type, scenario, name, iso3c, vac_cost) %>% 
#   rename(population = name) %>% 
#   mutate(Type = tolower(Type),
#          Type = factor(Type,
#                        levels = c("az","pfizer"),
#                        labels = c("Viral vector vaccines",
#                                   "mRNA vaccines"))) %>% 
#   filter(date_start == "2021-07-01", scenario == "medium") %>% 
#   ggplot(., aes(x = vac_unit, y = Type, color = Type)) +
#   geom_boxplot() +
#   scale_color_manual(values = ggsci::pal_futurama()(5)[c(4,5)]) +
#   theme_bw() +
#   theme(# axis.text.x = element_text(angle = 90),
#         legend.position = "none") +
#   labs(y = "Vaccine type",
#        x = "Vaccine unit cost (USD$2020)",
#        color = "Dose number") +
#   custom_theme -> p_vac_cost

# ms_cov_all %>% 
#   left_join(vac_denom, by = "iso3c") %>% 
#   mutate(year_end1 = ymd("2021-12-31"),
#          year_end2 = ymd("2022-12-31"),
#          days_diff1_dose1 = as.numeric(year_end1 - date_start),
#          days_diff1_dose2 = as.numeric(year_end1 - date_start) - 28,
#          days_diff2_dose1 = as.numeric(year_end2 - date_start),
#          days_diff2_dose2 = as.numeric(year_end2 - date_start) - 28,
#          days_diff2_dose1 = days_diff2_dose1 - days_diff1_dose1,
#          days_diff2_dose2 = days_diff2_dose2 - days_diff1_dose2,
#          cov_achieved_diff1_dose1 = days_diff1_dose1 *r_vac,
#          cov_achieved_diff1_dose2 = days_diff1_dose2 *r_vac,
#          cov_achieved_diff2_dose1 = days_diff2_dose1 *r_vac,
#          cov_achieved_diff2_dose2 = days_diff2_dose2 *r_vac,
#          cov_achieved_diff2_dose1 = if_else(cov_achieved_diff1_dose1 + cov_achieved_diff2_dose1 > 0.7,
#                                             0.7 - cov_achieved_diff1_dose1,
#                                             cov_achieved_diff2_dose1),
#          cov_achieved_diff2_dose2 = if_else(cov_achieved_diff1_dose2 + cov_achieved_diff2_dose2 > 0.7,
#                                             0.7 - cov_achieved_diff1_dose2,
#                                             cov_achieved_diff2_dose2),
#          year1_doses = (cov_achieved_diff1_dose1 + cov_achieved_diff1_dose2)*tot*1000,
#          year2_doses = (cov_achieved_diff2_dose1 + cov_achieved_diff2_dose2)*tot*1000,
#          vac_unit_disc = vac_unit/((1+discount_r)),
#          vac_cost = year1_doses*vac_unit + year2_doses*(vac_unit_disc)) %>%
#   # dplyr::select(date_start, scenario, Type, name, iso3c, r_vac, Rate, vac_cost) %>% 
#   # dplyr::select(date_start, Type, scenario, name, iso3c, vac_cost) %>% 
#   rename(population = name) %>% 
#   mutate(Type = tolower(Type),
#          Type = factor(Type,
#                        levels = c("az","pfizer"),
#                        labels = c("Viral vector vaccines",
#                                   "mRNA vaccines"))) %>% 
#   filter(date_start == "2021-07-01", scenario == "medium") %>% 
#   dplyr::select(Type, iso3c, vac_unit) %>% 
#   pivot_wider(names_from = Type, values_from = vac_unit) %>% 
#   mutate(loc = countrycode(iso3c, "iso3c", "country.name")) %>% 
#   .[,c(4,1,2,3)] %>% 
#   write_csv("data/vac_price.csv")

# cost_care %>% 
#   mutate_at(vars(c("home", "hosp", "icu", "deaths")), as.numeric) %>% 
#   mutate(severe = hosp*9.6,
#          critical = hosp*8.88 + icu*12.6) %>% 
#   select(-hosp, - icu) %>% 
#   pivot_longer(cols = c("home", "deaths", "severe", "critical")) %>% 
#   mutate(name = factor(name,
#                        levels = c("home", "severe", "critical", "deaths"),
#                        labels = c("Home-based care",
#                                   "Hospital-based care\nfor severe cases",
#                                   "Hospital-based care\nfor critical cases",
#                                   "Deaths"))) %>% 
#   ggplot(., aes(x = log(value, base = 10), y = name)) +
#   geom_density_ridges(fill = "white", scale = 0.95) +
#   # scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
#   scale_y_discrete(limits = rev) +
#   scale_x_continuous(breaks = c(1, 2, 3, 4, 5), 
#                      labels = c("$10","$100","$1K","$10K","$100K")) +
#   theme_bw() +
#   custom_theme +
#   labs(y = "Case management",
#        x = "Health care costs per patient\n(USD$2020, log10-transformed)") -> p_hc_cost
  

ms_scenarios %>% 
  dplyr::select(-date_vac_end_ext, -cov_ext) %>% 
  mutate(sim_end = if_else(date_vac_end < "2022-12-31", "2022-12-31", as.character(NA)),
         cov_end = if_else(is.na(sim_end), as.numeric(NA), 0.7),
         scenario = factor(scenario, levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast"))) %>% 
  ggplot(.) +
  geom_segment(aes(x = date_start, 
                   xend = date_vac_end, 
                   y = 0, 
                   yend = cov, 
                   color = scenario), 
               size = 0.3) + 
  geom_segment(aes(x = date_vac_end, 
                   xend = ymd(sim_end), 
                   y = 0.7, 
                   yend = 0.7, 
                   color = scenario), 
               size = 0.3) +
  scale_color_futurama() +
  theme_bw() +
  labs(x = "Vaccination program start date",
       y = "Vaccine coverage\n(two-dose, assumed)",
       color = "Vaccine roll-out rate") +
  theme(legend.position = "top") +
  custom_theme +
  lims(y = c(0,1)) +
  scale_x_date(breaks = c("2021-01-01",
                          "2021-07-01",
                          "2022-01-01",
                          "2022-07-01") |> ymd()) -> p_ro_scenarios

#### p_ro_empirical ####
owid_vac %>% 
  dplyr::select(iso3c, date, daily_vaccinations_per_million) %>% 
  filter(!is.na(daily_vaccinations_per_million)) %>% 
  group_by(iso3c) %>% group_split() %>% map(~replace(., is.na(.), 0)) %>% 
  map(arrange, date) %>% map(mutate, cumulative_doses_per_million = cumsum(daily_vaccinations_per_million)) -> tmp

tmp %>% map(pull, iso3c) %>% map(unique) %>% unlist -> tmp_cn

tmp %>% 
  map(~lm(cumulative_doses_per_million ~ date, data = .)) %>% 
  map(summary) %>% map(~.$coefficients) %>% map(data.frame) %>%
  setNames(tmp_cn) %>% 
  bind_rows(.id = "iso3c") %>% 
  rownames_to_column(var = "metric") %>% 
  filter(grepl("date", metric)) %>% 
  mutate(nt_speed = ntile(Estimate, 3)) %>% 
  dplyr::select(iso3c, nt_speed, Estimate ) -> tmp_speed

tmp %>% bind_rows() %>%
  left_join(tmp_speed, by = "iso3c") %>%
  mutate(nt_speed = factor(nt_speed, levels = 1:3, 
                           labels = c("Slow", "Medium", "Fast"))) %>%
  ggplot(., aes(x = date,
                y = (cumulative_doses_per_million/1000000)/2,
                group = iso3c,
                color = nt_speed)) +
  geom_line(alpha = 1, size = 0.3) +
  # facet_wrap(~nt_speed) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Date",
       y = "Vaccine coverage\n(two-dose, observed)",
       color = "Vaccine roll-out rate level") +
  ggsci::scale_color_futurama() +
  custom_theme +
  scale_x_date(breaks = c("2021-01-01",
                          "2021-07-01",
                          "2022-01-01") |> ymd()) -> p_ro_empirical

# legend1 <- get_legend(p_ro_scenarios)
# legend2 <- get_legend(p_vac_cost + 
#                         theme(legend.position = "top") + 
#                         labs(color = "Vaccine type") )

p1 <- plot_grid(p_ro_empirical + theme(legend.position = "none"), 
                p_ro_scenarios + theme(legend.position = "none"),
                nrow = 1,
                rel_widths = c(2,3),
                labels = c("\n            (a)", 
                           "\n            (b)"),
                align = "h")


# p1 <- plot_grid(p_ro_empirical + theme(legend.position = "none"), 
#                 p_ro_scenarios + theme(legend.position = "none"), ncol = 2,
#                 rel_widths = c(2,3),
#                 labels = c("(a)", "(b)"))
# p2 <- plot_grid(plot_grid(p_vac_cost, p_hc_cost, ncol = 1, rel_heights = c(2,3),
#                           labels = c("   (d)",
#                                      "   (e)")), p_VE, labels = c("","(f)"))
# 

# p2 <- plot_grid(p_VE, p_vac_cost, p_hc_cost, ncol = 1, rel_heights = c(4, 2,3),
#                 labels = c("   (d)",
#                            "   (e)",
#                            "   (f)"))
# 
# p3 <- plot_grid(legend1,
#           legend2,
#           plot_grid(p_map, labels = "(c)"),
#           ncol = 1,
#           rel_heights = c(0.55,0.55, 10),
#           axis = c("l"),
#           align = "v")

# p3 <- plot_grid(p1, p2, ncol = 1)
# 
# p4 <- plot_grid(plot_grid(legend1, legend2, ncol = 1,
#                           axis = "bl"),
#                 p_map,
#                 ncol = 1,
#                 rel_heights = c(1, 10),
#                 axis = "bl",
#                 labels = c("","     (c)"))
# 
# p5 <- plot_grid(p3, p4, align = "h", rel_widths = c(3,2))

# p5 <- plot_grid(p1, 
#                 plot_grid(p2, p3, ncol = 2, rel_widths = c(4,3)), 
#                 ncol = 1, rel_heights = c(4,8))

# ggsave("figs/fig1.png", p5, width = 15, height = 12)
ggsave("figs/R2R_R1/fig1_R1.png",
       plot = p1,
       width = 10, height = 5)
