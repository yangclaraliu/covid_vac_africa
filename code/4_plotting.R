#### roll-out scenario ####
ms_scenarios %>% 
  dplyr::select(-date_vac_end_ext, -cov_ext) %>% 
  mutate(sim_end = if_else(date_vac_end < "2022-12-31", "2022-12-31", as.character(NA)),
         cov_end = if_else(is.na(sim_end), as.numeric(NA), 0.7),
         scenario = factor(scenario, levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast"))) %>% 
  ggplot(.) +
  geom_segment(aes(x = date_start, xend = date_vac_end, y = 0, yend = cov, color = scenario)) + 
  geom_segment(aes(x = date_vac_end, xend = ymd(sim_end), y = 0.7, yend = 0.7, color = scenario)) +
  scale_color_futurama() +
  theme_bw() +
  labs(x = "Availability of supply measured by\nVaccine Roll-out Start Date",
       y = "Ability to Deliver measured by \nVaccine Coverage Level (two-dose)",
       color = "Ability to Deliver") +
  theme(legend.position = "top") +
  custom_theme +
  lims(y = c(0,1))

ggsave("figs/vac_rate_viz.png", width = 8, height = 6)

#### Epidemic History ####
owid_epi %>% 
  left_join(pop %>% 
              group_by(iso3c) %>% 
              summarise(tot = (sum(f) + sum(m))*1000),
            by = "iso3c") %>% 
  mutate(deaths_rate = deaths/tot,
         cases_rate = cases/tot,
         cases = if_else(cases == 0, 1, cases),
         cases_lims = case_when(cases > 1e4 ~ 5,
                                cases <= 1e4 & cases > 1e3 ~ 4,
                                cases <= 1e3 & cases > 1e2 ~ 3,
                                cases <= 1e2 & cases > 1e1 ~ 2,
                                cases <= 1e1 ~ 1,
                                )) %>% 
  ggplot(., aes(x = date, y = reorder(iso3c, cases_lims), fill = factor(cases_lims,
                                                                        labels = c("<10","10-100","100-1000",
                                                                                   "1000-10000", ">10000")))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(y = "Disease Burden (Reported Cases)",
       x = "Date",
       fill = "Daily Case Counts") +
  theme_cowplot() +
  theme(axis.text.y = element_blank(),
        legend.position = "top")

ggsave("figs/AfHEA/cases_ts.png",
       width = 15, height = 10)

#### vaccination rate achievable ####
owid_vac %>% 
  dplyr::select(iso3c, date, people_fully_vaccinated) %>% 
  filter(!is.na(people_fully_vaccinated)) %>% 
  left_join(pop %>% 
              group_by(iso3c) %>% 
              summarise(tot = (sum(f) + sum(m))*1000),
            by = "iso3c") %>% 
  mutate(cov = people_fully_vaccinated/tot) %>%
  filter(cov >= 0.01) %>% 
  group_by(iso3c) %>% 
  mutate(date_min = min(date)) %>% 
  filter(date == date_min) %>% 
  left_join(tmp_speed, by = "iso3c") %>% 
  left_join(owid_epi %>% 
              left_join(pop %>% 
                          group_by(iso3c) %>% 
                          summarise(tot = (sum(f) + sum(m))*1000),
                        by = "iso3c") %>% 
              group_by(iso3c) %>% 
              summarise(deaths = sum(deaths),
                        cases = sum(cases),
                        tot = mean(tot),
                        days = n(),
                        burden = cases/days) %>% 
              dplyr::select(iso3c, burden),
            by = "iso3c") %>% #   -> mark_0
# mark_1 %>% 
#   left_join(mark_10, by = "iso3c") %>% 
#   mutate( d = date.x - date.y) %>% 
#   pull(d) %>% as.numeric %>% summary
# 
# mark_0$date %>% summary
  ggplot(., aes(x = date, y = Estimate,
                # fill = log(burden), 
                size = (tot)/1000000)) +
  geom_point(color = "#0D5257", alpha = 0.5) +
  # geom_hline(yintercept = speed_median$vac_rate_per_million, linetype = 2) +
  # scale_size(
  #   breaks = c(1:10),
  #   range = c(1,10)
  # ) +
  theme_cowplot() +
  ggsci::scale_fill_material("red") +
  guides(fill = F) +
  labs(size = "Population Size (million)", x = "First Day with Proportion of Population\n Fully Covered by Vaccine Exceed 1%", y = "Doses Administered/ Million Population-Day") +
  theme(legend.position = "top") +
  lims(x = c(ymd("2021-01-01", "2021-12-31")))

ggsave("figs/ISPH_fig1.png", width = 7, height = 5)

owid_epi %>% 
  group_by(iso3c) %>% 
  mutate(death_min = min(deaths)) %>% 
  filter(deaths == death_min) %>% 
  mutate(date_min = min(date)) %>% 
  filter(date == date_min) %>% 
  dplyr::select(iso3c, date_min) %>% 
  rename(t_start_fitting = date_min) %>% 
  right_join(fitted_table, by = "iso3c") %>% 
  dplyr::select(loc, iso3c, t_start_fitting, t_end_fitting) -> tmp
  
write_csv(tmp, "data/List_Fitted.csv")

ggsave("figs/vac_rate.png",
       width = 6, height = 6)

##### healthcare cost viz ####
shape %>% 
  left_join(cost_health_unit %>% 
              mutate(ISO3_CODE = countrycode(Country, "country.name", "iso3c")), by = c("ISO3_CODE")) %>% 
  mutate(case_management_critical = as.numeric(`...13`),
         case_management_hosp = as.numeric(`...12`),
         case_management_home = as.numeric(`...11`))  -> tmp

tmp %>% 
  dplyr::select(ISO3_CODE, NAME_ENGL,
                case_management_critical, case_management_hosp,
                case_management_home) %>% 
  filter(ISO3_CODE == "SYC") %>% 
  st_as_sf %>% 
  ggplot(., aes(fill = case_management_critical)) +
  geom_sf() +
  # theme_map() +
  ggsci::scale_fill_material(palette = "amber") +
  theme(legend.position = "top",
        legend.key.width = unit(1, 'cm')) +
  labs(fill = "Case Management: Critical Care")# +facet_wrap(~NAME_ENGL   , scales = "fixed")

ggsave("figs/AfHEA/cost_case_critical.png")

#### visualising parameters ####
ms_cov_all %>% 
  mutate(scenario = factor(scenario,
                       levels = c("slow", "medium", "fast"),
                       labels = c("Slow", "Medium", "Fast"))) %>% 
#   filter(Elapse == 10) %>% 
  ggplot(., aes(x = Elapse , y = vac_unit, group = iso3c, color = scenario, fill = scenario)) +
  geom_line() +
  facet_wrap(scenario~Type, scales = "free") +
  scale_fill_futurama() +
  scale_color_futurama() +
  theme_bw() +
  custom_theme +
  theme(legend.position = "top") +
  labs(y = "Elapse Time",
       x = "Vaccine Prices",
       fill = "Ability to Deliver",
       color = "Ability to Deliver")

ggsave("figs/model_vaccine_price.png", width = 20, height = 10)

fitted_table |> 
  ggplot(aes(x = r)) +
  geom_density() +
  theme_cowplot() +
  custom_theme +
  labs(x = "[1] Basic Reproduction Number",
       y = "Probability Density") -> p_1

fitted_table |> 
  ggplot(aes(x = t_intro)) +
  geom_density() +
  theme_cowplot() +
  custom_theme +
  labs(x = "[2] Infection Introduction Date",
       y = "Probability Density") -> p_2

fitted_table |> 
  ggplot(aes(x = rr)) +
  geom_density() +
  theme_cowplot() +
  custom_theme +
  labs(x = "[3] Reporting Probability",
       y = "Probability Density") -> p_3

fitted_table |> 
  filter(t_intro_voc1 == "2021-01-15") |> 
  ggplot(aes(x = t_intro_voc2)) +
  geom_density() +
  theme_cowplot() +
  custom_theme +
  labs(x = "[4] Variants of Concern Transition Time\n2021",
       y = "Probability Density") -> p_4

plot_grid(p_1, p_2, p_3, p_4, ncol = 2)
ggsave("figs/R2R_R1/fitted_values.png", width = 15, height = 8)
