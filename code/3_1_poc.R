# PROOF OF CONCEPT EXERCISE FOR DEC 2021

require(ggridges)

res <- read_rds("data/intermediate/simulationb_res.rds")

res %>% 
  filter(R0 == 2.5,
         date_start == "2020-03-25") %>% 
  dplyr::select(-starts_with("a_")) %>% 
  pivot_longer(cols = starts_with("r_diff")) %>% 
  mutate(name = factor(name, labels = paste0("ROS",1:3)),
         compartment = factor(compartment,
                              levels = c("cases", "hosp", "death"),
                              labels = c("Infections", "Severe Cases", 
                                         "Deaths"))) %>% 
  ggplot(., aes(y = value, x = name)) +
  geom_boxplot() +
  facet_grid(~compartment) +
  labs(x = "Roll-out Schedules",
       y = "Proportion Reduction") +
  theme_bw() -> p1

res %>% 
  filter(R0 == 2.5,
         date_start == "2020-03-25") %>% 
  dplyr::select(-starts_with("r_")) %>% 
  pivot_longer(cols = starts_with("a_diff")) %>% 
  # group_by(compartment, name) %>% summarise(value = sum(value)) %>% 
  mutate(ROS = parse_number(name),
         ROS = paste0("ROS",ROS),
         compartment = factor(compartment,
                              levels = c("cases", "hosp", "death"),
                              labels = c("Infections", "Severe Cases", 
                                         "Deaths"))) %>% 
  filter(value > 1) %>% 
  ggplot(., aes(x = ROS, y = value)) +
  geom_boxplot() +
  facet_wrap(~compartment, nrow = 1) +
  theme_bw() +
  scale_y_log10() +
  labs(x = "Roll-out Schedules",
       y = "Absolute Difference") -> p2


p_tmp <- plot_grid(p1, p2, nrow = 2, axis = "tblr", align = "hv",
                   labels = c("(a)","(b)"))

ggsave("figs/supplemental/poc_prop_reduction.png", p_tmp, 
       width = 8, height = 5)

# 
res %>% 
  filter(population == "Ethiopia") %>% 
  dplyr::select(-starts_with("a_")) %>% 
  pivot_longer(cols = starts_with("r_")) %>% 
  mutate(ROS = parse_number(name),
         ROS = paste0("ROS",ROS),
         compartment = factor(compartment,
                              levels = c("cases", "hosp", "death"),
                              labels = c("Infections", "Severe Cases", 
                                         "Deaths"))) %>% 
  ggplot(., aes(y = ROS, x = value, fill = ROS)) +
  geom_density_ridges(alpha = 0.5) +
  facet_grid(rows = "compartment") +
  labs(y = "Roll-out Schedules",
       x = "Proportion Reduction",
       fill = "",
       title = "Ethiopia") +
  ggsci::scale_fill_lancet() +
  theme_bw() -> p_tmp

ggsave("figs/supplemental/poc_ethiopia.png",
       width = 8, height = 5)

# 
res %>% 
  filter(compartment == "death") %>% 
  dplyr::select(-starts_with("a_")) %>% 
  pivot_longer(cols = starts_with("r_")) %>% 
  group_by(population, name) %>% 
  summarise(md = median(value, na.rm = T),
            IQR_LL = quantile(value, 0.25, na.rm = T),
            IQR_UL = quantile(value, 0.75, na.rm = T)) %>% 
  mutate(ROS = parse_number(name),
         ROS = paste0("ROS",ROS)) %>% 
  mutate(first = substr(population,1,1),
         g = if_else(first %in% LETTERS[1:12], 1, 2)) %>% 
  ggplot(., aes(x = md, y = population, color = ROS)) +
  geom_point() +
  geom_segment(aes(x = IQR_LL, xend = IQR_UL, y = population, yend = population)) +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  facet_wrap(~g, drop = T, scales = "free_y") +
  labs(x = "Proportion Reduction", y = "", color = "") +
  ggsci::scale_color_lancet() +
  theme(  strip.background = element_blank(),
          strip.text.x = element_blank())-> p_tmp

ggsave("figs/supplemental/poc_deaths_bycountry.png",
       width = 12, height = 6)


# 
res %>% 
  dplyr::select(-starts_with("r_")) %>% 
  pivot_longer(starts_with("a_")) %>% 
  mutate(ROS = parse_number(name),
         ROS = paste0("ROS",ROS)) %>%  
  group_by(compartment, R0, date_start, ROS) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  filter(compartment == "death") %>% 
  ggplot(., aes(x = date_start, y = R0, fill = value/1e6)) +
  geom_tile() +
  facet_wrap(~ROS) +
  ggsci::scale_fill_material("indigo") +
  theme_cowplot() +
  labs(title = "Deaths", x = "Infection Introduction (in 2020)",
       fill = "10^6") -> p3


res %>% 
  dplyr::select(-starts_with("r_")) %>% 
  pivot_longer(starts_with("a_")) %>% 
  mutate(ROS = parse_number(name),
         ROS = paste0("ROS",ROS)) %>%  
  group_by(compartment, R0, date_start, ROS) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  filter(compartment == "hosp") %>% 
  ggplot(., aes(x = date_start, y = R0, fill = value/(1000000))) +
  geom_tile() +
  facet_wrap(~ROS) +
  ggsci::scale_fill_material("indigo") +
  theme_cowplot() +
  labs(title = "Severe Cases", x = "Infection Introduction (in 2020)",
       fill = "10^6") -> p2

res %>% 
  dplyr::select(-starts_with("r_")) %>% 
  pivot_longer(starts_with("a_")) %>% 
  mutate(ROS = parse_number(name),
         ROS = paste0("ROS",ROS)) %>%  
  group_by(compartment, R0, date_start, ROS) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  filter(compartment == "cases") %>% 
  ggplot(., aes(x = date_start, y = R0, fill = value/1e6)) +
  geom_tile() +
  facet_wrap(~ROS) +
  ggsci::scale_fill_material("indigo") +
  theme_cowplot() +
  labs(title = "Infections", x = "Infection Introduction (in 2020)",
       fill = "10^6") -> p1

p_tmp <- plot_grid(p1, p2, p3, nrow = 3, axis = "tblr", align = "hv")

ggsave("figs/supplemental/poc_continental_tile.png", p_tmp, 
       width = 10, height = 7)

res %>% 
  dplyr::select(-starts_with("r_")) %>% 
  pivot_longer(starts_with("a_")) %>% 
  mutate(ROS = parse_number(name),
         ROS = paste0("ROS",ROS)) %>%  
  group_by(compartment, R0, date_start, ROS) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(compartment, ROS) %>% 
  summarise(LL = min(value), UL = max(value),
            IQR_LL = quantile(value, 0.25), IQR_UL = quantile(value, 0.75),
            mean = mean(value)) %>% 
  mutate( compartment = factor(compartment,
                              levels = c("cases", "hosp", "death"),
                              labels = c("Infections", "Severe Cases", 
                                         "Deaths"))) %>% 
  ggplot(., aes(x = ROS, y = mean)) +
  geom_point() +
  facet_wrap(~compartment, scales = "free", ncol = 3) +
  geom_segment(aes(x = ROS, xend = ROS, y = LL, yend = UL)) +
  geom_segment(aes(x = ROS, xend = ROS, y = IQR_LL, yend = IQR_UL), size = 1.2) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  labs(x = "Roll-out Schedules", y = "All-continent Sum") +
  theme_cowplot() -> p_tmp

ggsave("figs/supplemental/poc_continental_sum.png", p_tmp, 
       width = 10, height = 4)
