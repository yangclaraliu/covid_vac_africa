# Advocacy 2021/12
ROS_labels <- c("No\nVaccine",
                paste0("ROS",1:4))
outcome_labels <- c("Symptomatic Infections",
                    "Severe Cases",
                    "Deaths")
scientific_10 <- function(x) {
  parse(text=gsub("e", "%*% 10^", scales::scientific_format()(x))%>% gsub("\\+","",.))
}

# x <- (10000102)
# scientific_10(x)


res %>% 
  bind_rows() %>% 
  group_by(R0, date_start, compartment) %>% 
  summarise_at(vars(c("no_vac", "ROS1", "ROS2", "ROS3", "ROS4")),
               sum) %>% 
  mutate(r_diff_1 = ROS1/no_vac - 1,
         r_diff_2 = ROS2/no_vac - 1,
         r_diff_3 = ROS3/no_vac - 1,
         r_diff_4 = ROS4/no_vac - 1) %>% 
  group_by(compartment) %>% 
  summarise(LL_1 = min(r_diff_1),
            UL_1 = max(r_diff_1),
            LL_2 = min(r_diff_2),
            UL_2 = max(r_diff_2),
            LL_3 = min(r_diff_3),
            UL_3 = max(r_diff_3),
            LL_4 = min(r_diff_4),
            UL_4 = max(r_diff_4))# %>% 
  pivot_longer(c(starts_with("U"), starts_with("L"))) %>% 
  separate(name, into = c("limits", "ROS")) %>% 
  pivot_wider(names_from = ROS, values_from = value)


res %>% 
  bind_rows() %>% 
  group_by(R0, date_start, compartment) %>% 
  summarise_at(vars(c("no_vac", "ROS1", "ROS2", "ROS3", "ROS4")),
               sum) %>% 
  pivot_longer(c("no_vac", "ROS1", "ROS2", "ROS3", "ROS4")) %>% 
  group_by(compartment, name) %>% 
  summarise(median = median(value),
            IQR_LL = quantile(value, 0.25),
            IQR_UL = quantile(value, 0.75),
            CI_LL = quantile(value, 0.025),
            CI_UL = quantile(value, 0.975)) %>% 
  rename(ROS = name) -> p_tab

p_tab

p_tab %>% 
  pivot_longer(cols = c(starts_with("IQR"), starts_with("CI"))) %>% 
  separate(name, into = c("metric", "limits")) %>% 
  mutate(ROS = factor(ROS, labels = ROS_labels),
         point = "Median",
         compartment = factor(compartment,
                              levels = c("cases", "hosp", "death"),
                              labels = outcome_labels)) %>%
  pivot_wider(names_from = limits, values_from = value) %>%
  mutate(metric = factor(metric,
                         levels = c("CI", "IQR"),
                         labels = c("95% Empirical Confidence Interval",
                                    "Interquartile Range"))) %>% 
  ggplot(., aes(x = ROS)) +
  geom_segment(aes(xend = ROS, y = LL, yend = UL, size = metric)) +

  facet_wrap(~compartment, scales = "free") +
  geom_point(aes(y = median, shape = point), size = 2, fill = "white") +
  scale_size_manual(values = c(1.2,2)) +
  scale_shape_manual(values = c(21)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 18),
        legend.position='bottom', 
        legend.justification='left',
        # legend.box = "vertical",
        legend.spacing.y = unit(-0.4, "cm"),
        # legend.margin = margin(-0.5, -1, 0,1, unit="cm"),
        legend.box = "vertical",
        legend.box.just = "left",
        legend.background = element_blank(),
   
        legend.box.background = element_blank(),
        legend.box.margin = margin(b = -0.4, unit = "cm"),
        legend.direction='horizontal') +
  labs(title = "Cumulative Health Outcomes based on Simulated Outbreaks\nbetween 2021-03-01 and 2022-12-31",
       x = "Roll-out Schedules",
       y = "Counts",
       shape = "Point Metric:", 
       size = "Range Metric:") +
  scale_y_continuous(label=scientific_10) +
  guides(size = guide_legend(order = 1),
         shape = guide_legend(order = 2)) -> p

ggsave("figs/advo_fig1.pdf", width = 15, height = 6)


#ts
ts %>% 
  bind_rows(.id = "index") %>% 
  left_join(params_table, by = "index") %>% 
  data.table() %>% 
  dplyr::select(-cn)  %>% 
  group_by(date_start, R0, date, name) %>% 
  summarise(value = sum(value)) %>% 
  pivot_wider(names_from = name, values_from = value) -> p_table

p_table %>%
  pivot_longer(c(no_vac, paste0("ROS",1:4))) %>% 
  group_by(date, name) %>% 
  summarise(LL = min(value),
            UL = max(value)) %>% 
  mutate(name = factor(name, labels = ROS_labels)) %>% 
  ggplot(., aes(x = date, group = name, fill = name)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), color = NA, alpha = 0.3) +
  ggsci::scale_fill_lancet()+
  ggsci::scale_color_lancet() +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 18),
        legend.position='bottom', 
        legend.justification='left') +
  scale_y_continuous(label=scientific_10) +
  labs(x = "Date",
       title = "Cumulative COVID-19 Deaths",
       subtitle = "All African Union Member States, n = 55",
       color = "Roll-out Schedules:",
       fill = "Roll-out Schedules:") -> p

p +
  facet_grid(~name) +
  scale_x_continuous(breaks = c(ymd("2020-01-01"), 
                                ymd("2021-01-01"),
                                ymd("2022-01-01")),
                     labels = c(2020, 2021, 2022)) 

ggsave("figs/advo_fig2_facet.pdf", width = 15, height = 5)


p_table %>%
  pivot_longer(c(no_vac, paste0("ROS",1:4))) %>% 
  group_by(date, name) %>% 
  summarise(LL = min(value),
            UL = max(value)) %>% 
  filter(name == "no_vac") %>% 
  ggplot(., aes(x = date)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.5, color = NA) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 18),
        legend.position='bottom', 
        legend.justification='left') +
  scale_y_continuous(label=scientific_10) +
  labs(x = "Date",
       title = "Cumulative COVID-19 Deaths (No Vaccine Scenario)",
       subtitle = "All African Union Member States, n = 55",
       color = "Roll-out Schedules:",
       fill = "Roll-out Schedules:") -> p

ggsave("figs/advo_fig2_no_vac.pdf", width = 8, height = 8)

p_table %>%
  ggplot(., aes(x = date)) +
  geom_line(aes(y = no_vac, group = interaction(R0, date_start)), alpha = 0.5, color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 18),
        legend.position='bottom', 
        legend.justification='left') +
  scale_y_continuous(label=scientific_10) +
  labs(x = "Date",
       title = "Cumulative COVID-19 Deaths (No Vaccine Scneario)",
       subtitle = "All African Union Member States, n = 55",
       color = "Roll-out Schedules:",
       fill = "Roll-out Schedules:") -> p

ggsave("figs/advo_fig2_no_vac_backend.pdf", width = 8, height = 8)
