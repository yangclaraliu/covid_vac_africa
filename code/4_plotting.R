

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
            by = "iso3c") %>% 
  ggplot(., aes(x = date, y = Estimate, size = (tot)/1000000, fill = log(burden))) +
  geom_point(pch = 21, stroke = 1.4) +
  geom_hline(yintercept = speed_median$vac_rate_per_million, linetype = 2) +
  # scale_size(
  #   breaks = c(1:10),
  #   range = c(1,10)
  # ) +
  theme_cowplot() +
  ggsci::scale_fill_material("red") +
  guides(fill = F) +
  labs(size = "Population Size (million)", x = "2021\nFirst Day with Proportion of Population\n Fully Covered by Vaccine Exceed 1%", y = "Doses Administered/ Million-Day") +
  theme(legend.position = "top")

ggsave("figs/AfHEA/vac_rate.png",
       width = 6, height = 6)

