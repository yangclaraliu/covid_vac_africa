max_date <- owid_vac %>% filter(is.na(people_vaccinated)) %>% pull(date) %>% max

owid_vac %>% 
  left_join(vac_denom, "iso3c") %>% 
  mutate(cov = people_vaccinated/(tot*1000)) %>% 
  filter(!is.na(cov)) %>% 
  ggplot(., aes(x = date, y = cov, group = iso3c)) +
  geom_line(alpha = 3) +
  geom_vline(xintercept = ymd("2021-03-01"), color = "firebrick4") +
  geom_hline(yintercept = c(0.2,0.4,0.6), linetype = 2) +
  geom_line(data = ROS, aes(x = milestone_date,
                            y = coverage,
                            group = ROS),
            color = "firebrick4",
            size = 1.2) +
  scale_x_date(date_labels = "%Y-%m-%d",
               limits = ymd(c("2021-01-01","2022-01-15"))) +
  labs(y = "Vaccine Coverage (>= 1 dose)",
       x = "Date",
       title = "Vaccine Coverage in African Union Member States",
       subtitle = paste("by", max_date)) +
  theme_cowplot() +
  geom_label(x = ymd("2022-01-01"), y = 0.1, 
             label = "ROS1", color = "firebrick4") +
  geom_label(x = ymd("2022-01-01"), y = 0.2, 
             label = "ROS2", color = "firebrick4") +
  geom_label(x = ymd("2022-01-01"), y = 0.3, 
             label = "ROS3", color = "firebrick4") -> p_tmp

ggsave("figs/empirical_rollout.png", p_tmp)

