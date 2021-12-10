max_date <- owid_vac %>% filter(is.na(people_vaccinated)) %>% pull(date) %>% max

owid_vac %>% 
  left_join(vac_denom, "iso3c") %>% 
  mutate(cov = people_vaccinated/(tot*1000)) %>% 
  filter(!is.na(cov)) %>% 
  ggplot(., aes(x = date, y = cov, group = iso3c)) +
  geom_line(alpha = 0.3) +
  geom_vline(xintercept = ymd("2021-03-01"), color = "firebrick4") +
  geom_hline(yintercept = c(0.2,0.4,0.6, 0.8), linetype = 2) +
  geom_line(data = ROS, aes(x = milestone_date,
                            y = coverage,
                            group = ROS),
            color = "firebrick4",
            size = 1.2) +
  scale_x_date(date_labels = "%Y-%m-%d",
               limits = ymd(c("2021-01-01","2022-01-15"))) +
  labs(y = "Vaccine Supply",
       x = "Date",
       title = "Vaccine Roll-out Statuses\nin African Union Member States",
       subtitle = paste("by", max_date)) +
  theme_cowplot() +
  geom_label(x = ymd("2022-01-01"), y = 0.1, size = 8,
             label = "ROS1", color = "firebrick4") +
  geom_label(x = ymd("2022-01-01"), y = 0.2, size = 8,
             label = "ROS2", color = "firebrick4") +
  geom_label(x = ymd("2022-01-01"), y = 0.3, size = 8,
             label = "ROS3", color = "firebrick4") +
  geom_label(x = ymd("2022-01-01"), y = 0.8, size = 8,
             label = "ROS4", color = "firebrick4") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        title = element_text(size = 18)) -> p_tmp

# owid_vac %>% 
#   left_join(vac_denom, "iso3c") %>% 
#   mutate(cov = people_vaccinated/(tot*1000)) %>% 
#   filter(!is.na(cov)) %>% 
#   filter(date <= "2021-04-01") %>% 
#   filter(cov > 0.2)


ggsave("figs/empirical_rollout.pdf", 
       p_tmp,
       width = 8, height = 8)

