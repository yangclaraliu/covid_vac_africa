#### SA windows ####
point_size = 2.5
point_alpha = 0.8
ICER_all <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/ICER_all.rds")

ICER_all$az_05_ext %>% mutate(version = "ext", Type = "az") %>% 
  bind_rows(ICER_all$az_05 %>% mutate(version = "bc", Type = "az")) %>% 
  bind_rows(ICER_all$pf_05_ext %>% mutate(version = "ext", Type = "pf")) %>% 
  bind_rows(ICER_all$pf_05 %>% mutate(version = "bc", Type = "pf")) %>% 
  left_join(group_income, by = "iso3c") %>%
  filter(econ_id == 1) %>% 
  mutate(m = month(date_start)) |>
  select(iso3c, Type, m, scenario, ICER_scaled, version, econ_id) |> 
  mutate(ICER_cat= cut(ICER_scaled,
                        breaks = c(-Inf, 0.1, 0.3, 0.5, 1, Inf),
                        labels = c("<0.1", 
                                   "0.1-0.3",
                                   "0.3-0.5",
                                   "0.5-1",
                                   ">1"))) |> 
  # ungroup() |> 
  # group_by(scenario_id, econ_id, scenario, date_start, version, Type) %>% 
  # summarise(CE_tot = sum(ICER_scaled_bin),
  #           tot = n()) %>% 
 # ,
         # CE_prop = CE_tot/tot) %>% 
  # mutate(m = month(date_start),
  #        CE_prop = CE_tot/tot) %>%
  # dplyr::select(-CE_tot) %>% 
  pivot_wider(names_from = version, values_from = ICER_scaled) %>% 
  # mutate(CE_diff = bc - ext) %>% 
  mutate(econ_id = factor(econ_id,
                         labels = c("Time horizon: by 30/06/2023")),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccines",
                                  "mRNA vaccines")),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow",
                                      "Medium",
                                      "Fast"))) #%>% 
  # filter(Type == "Viral vector vaccines") |> 
  # group_by(m, bc, ext, Type, scenario) |> tally() |> 
  # ggplot(aes(x = bc, y = ext, fill = n)) +
  # geom_tile(color = "black") +
  # ggsci::scale_fill_material("deep-purple") + 
  # facet_wrap(~Type)+
  # theme_bw() +
  # custom_theme +
  # theme()
  
# ggplot(., aes(x = bc,
  #               y = ext,
  #               fill = m)) +
  # geom_point(aes(pch = Type),
  #             size = point_size,
  #             alpha = point_alpha) +
  # scale_shape_manual(values = c(21,24)) +
  # geom_abline(intercept = 0, slope = 1, linetype = 2) +
  #   viridis::scale_fill_viridis(breaks = c(3,6,9, 12),
  #                                labels = c("Mar.",
  #                                           "Jun.",
  #                                           "Sep.",
  #                                           "Dec."),
  #                                option = "magma") +
  # theme_bw() +
  # custom_theme +
  # theme(legend.position = "none") +
  # # lims(y = c(0,1)) +
  # labs(x = "Base case proportion cost-effective\n(time horizon: by 31/12/2022)",
  #      y = "Test case\nproportion cost-effective") +
  # facet_wrap(Type~m) # -> p1

#### SA: WTP thresholds ####
ICER$az_05 %>% mutate(Type = "az", threshold = "medium") %>% 
  bind_rows(ICER$az_03 %>% mutate(Type = "az", threshold = "low")) %>% 
  bind_rows(ICER$az_10%>% mutate(Type = "az", threshold = "high")) %>% 
  bind_rows(ICER$pf_03 %>% mutate(Type = "pf", threshold = "low")) %>% 
  bind_rows(ICER$pf_05%>% mutate(Type = "pf", threshold = "medium")) %>% 
  bind_rows(ICER$pf_10%>% mutate(Type = "pf", threshold = "high")) %>% 
  group_by(scenario_id, econ_id, scenario, date_start, threshold, Type) %>% 
  summarise(CE_prop = sum(ICER_scaled_bin)/n()) %>% 
  mutate(m = month(date_start)) %>% 
  pivot_wider(names_from = threshold, values_from = CE_prop) %>% 
  pivot_longer(cols= c("high", "low"),
               values_to = "sa",
               names_to = "threshold") %>% 
  mutate(threshold = factor(threshold, 
                            levels = c("low", "high"),
                            labels = c("0.3xGDPpc",
                                       "1.0xGDPpc")),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow",
                                      "Medium",
                                      "Fast"))) %>% 
  ggplot(., aes(x = medium, y = sa, fill = m)) +
  geom_jitter(aes(pch = Type),
              size = point_size,
              alpha = point_alpha) +
  scale_shape_manual(values = c(21,24)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  viridis::scale_fill_viridis(breaks = c(3,6,9, 12),
                               labels = c("Mar.",
                                          "Jun.",
                                          "Sep.",
                                          "Dec."),
                               option = "magma") +
  theme_bw() +
  custom_theme +
  theme(legend.position = "none") +
  lims(y = c(0,1)) +
  facet_wrap(~threshold, nrow = 1) +
  labs(x = "Base case proportion cost-effective\n(willingness-to-pay threshold at 0.5xGDPpc)",
       y = "Test case\nproportion cost-effective") -> p2

#### SA: econ scenario ####
ICER$az_05 %>% mutate(Type = "az") %>% 
  bind_rows(ICER$pf_05 %>% mutate(Type = "pf")) %>% 
  group_by(scenario_id, econ_id, scenario, date_start, Type) %>% 
  summarise(CE_prop = sum(ICER_scaled_bin)/n()) %>% 
  mutate(m = month(date_start)) %>% 
  pivot_wider(names_from = econ_id, values_from = CE_prop) %>%
  pivot_longer(cols = c("2","3")) %>% 
  rename(bc = `1`,
         econ_id = name,
         sa = value) %>% 
  mutate(econ_id = factor(econ_id, levels = 1:3,
                          labels = c("Discount rate = 0.03\nSMR = 1",
                                     "Discount rate = 0\nSMR = 1",
                                     "Discount rate = 0.03\nSMR = 1.5")),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow",
                                      "Medium",
                                      "Fast"))) %>% 
  ggplot(., aes(x = bc, y = sa, fill = m)) +
  geom_jitter(aes(pch = Type),
              size = point_size,
              alpha = point_alpha) +
  scale_shape_manual(values = c(21,24)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  viridis::scale_fill_viridis(breaks = c(3,6,9, 12),
                               labels = c("Mar.",
                                          "Jun.",
                                          "Sep.",
                                          "Dec."),
                               option = "magma") +
  theme_bw() +
  custom_theme +
  theme(legend.position = "none") +
  lims(y = c(0,1)) +
  facet_wrap(~econ_id, nrow = 1) +
  labs(x = "Base case proportion cost-effective\n(Discount rate = 0.03, SMR = 1)",
       y = "Test case\nproportion cost-effective") -> p3


#### SA windows ####
ICER$az_05_low %>% mutate(version = "low", Type = "az") %>% 
  bind_rows(ICER$az_05 %>% mutate(version = "bc", Type = "az")) %>% 
  bind_rows(ICER$pf_05_low %>% mutate(version = "low", Type = "pf")) %>% 
  bind_rows(ICER$pf_05 %>% mutate(version = "bc", Type = "pf")) %>% 
  left_join(group_income, by = "iso3c") %>% 
  group_by(scenario_id, econ_id, scenario, date_start, version, Type) %>% 
  summarise(CE_tot = sum(ICER_scaled_bin),
            tot = n()) %>% 
  mutate(m = month(date_start),
         CE_prop = CE_tot/tot) %>% 
  # mutate(m = month(date_start),
  #        CE_prop = CE_tot/tot) %>%
  dplyr::select(-CE_tot) %>% 
  pivot_wider(names_from = version, values_from = CE_prop) %>% 
  # mutate(CE_diff = bc - ext) %>% 
  filter(econ_id == 1) %>% 
  mutate(econ_id = factor(econ_id,
                          labels = c("Lower bounds VE estimates")),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow",
                                      "Medium",
                                      "Fast"))) %>% 
  ggplot(., aes(x = bc,
                y = low,
                fill = m)) +
  geom_jitter(aes(pch = Type),
              size = point_size,
              alpha = point_alpha) +
  scale_shape_manual(values = c(21,24)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  viridis::scale_fill_viridis(breaks = c(3,6,9, 12),
                               labels = c("Mar.",
                                          "Jun.",
                                          "Sep.",
                                          "Dec."),
                               option = "magma") +
  theme_bw() +
  custom_theme +
  theme(legend.position = "none") +
  lims(y = c(0,1)) +
  labs(x = "Base case proportion cost-effective\n(base case VE estimates)",
       y = "Test case\nproportion cost-effective") +
  facet_wrap(~econ_id) -> p4

plot_grid(p1, 
          p4 +
            labs(y = "") +
            theme(axis.text.y = element_blank()),
          axis = "tblr",
          labels = c("(a)", 
                     "(b)"),
          align = "h",
          rel_widths = c(1.2,1)) -> row1

plot_grid(p2, 
          p3,
          axis = "tblr",
          labels = c("(c)","(d)"),
          ncol = 1,
          align = "h") -> row2

# col1 <- plot_grid(p1,
#                   p2,
#                   nrow = 2,
#                   axis = "tblr",
#                   align = "h",
#                   rel_heights = c(1, 2))
# 
# col2 <- plot_grid(p4 +
#                     labs(y = "") +
#                     theme(axis.text.y = element_blank()),
#                   p3 +
#                     labs(y = "") +
#                     theme(axis.text.y = element_blank()),
#                   nrow = 2,
#                   axis = "tblr",
#                   align = "h",
#                   rel_widths = c(1, 2))

ICER$az_05_ext %>% mutate(version = "ext", Type = "az") %>% 
  bind_rows(ICER$az_05 %>% mutate(version = "bc", Type = "az")) %>% 
  bind_rows(ICER$pf_05_ext %>% mutate(version = "ext", Type = "pf")) %>% 
  bind_rows(ICER$pf_05 %>% mutate(version = "bc", Type = "pf")) %>% 
  left_join(group_income, by = "iso3c") %>% 
  group_by(scenario_id, econ_id, scenario, date_start, version, Type) %>% 
  summarise(CE_tot = sum(ICER_scaled_bin),
            tot = n()) %>% 
  mutate(m = month(date_start),
         CE_prop = CE_tot/tot) %>% 
  # mutate(m = month(date_start),
  #        CE_prop = CE_tot/tot) %>%
  dplyr::select(-CE_tot) %>% 
  pivot_wider(names_from = version, values_from = CE_prop) %>% 
  # mutate(CE_diff = bc - ext) %>% 
  filter(econ_id == 1) %>% 
  mutate(econ_id = factor(econ_id,
                          labels = c("Time horizon: by 30/06/2023")),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccines",
                                  "mRNA vaccines"))) %>% 
  ggplot(., aes(x = bc,
                y = ext,
                fill = m)) +
  geom_jitter(size = point_size,
              alpha = point_alpha) +
  scale_shape_manual(values = c(21,24)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  viridis::scale_fill_viridis(breaks = c(3,6,9, 12),
                              labels = c("Mar.",
                                         "Jun.",
                                         "Sep.",
                                         "Dec."),
                              option = "magma") +
  labs(fill = "Vaccination programme starting date (2021)") +
  theme(legend.position = "top",
        legend.key.width = unit(1, 'cm')) +
  custom_theme -> p_legend1

legend1 <- get_legend(p_legend1)

ICER$az_05_ext %>% mutate(version = "ext", Type = "az") %>% 
  bind_rows(ICER$az_05 %>% mutate(version = "bc", Type = "az")) %>% 
  bind_rows(ICER$pf_05_ext %>% mutate(version = "ext", Type = "pf")) %>% 
  bind_rows(ICER$pf_05 %>% mutate(version = "bc", Type = "pf")) %>% 
  left_join(group_income, by = "iso3c") %>% 
  group_by(scenario_id, econ_id, scenario, date_start, version, Type) %>% 
  summarise(CE_tot = sum(ICER_scaled_bin),
            tot = n()) %>% 
  mutate(m = month(date_start),
         CE_prop = CE_tot/tot) %>% 
  # mutate(m = month(date_start),
  #        CE_prop = CE_tot/tot) %>%
  dplyr::select(-CE_tot) %>% 
  pivot_wider(names_from = version, values_from = CE_prop) %>% 
  # mutate(CE_diff = bc - ext) %>% 
  filter(econ_id == 1) %>% 
  mutate(econ_id = factor(econ_id,
                          labels = c("Time horizon: by 30/06/2023")),
         Type = factor(Type, levels = c("az","pf"),
                       labels = c("Viral vector vaccines",
                                  "mRNA vaccines"))) %>% 
  ggplot(., aes(x = bc,
                y = ext)) +
  geom_jitter(aes(pch = Type),
              size = 4) +
  scale_shape_manual(breaks = c("Viral vector vaccines",
                                "mRNA vaccines"),
                     values = c(21,24)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(pch = "Vaccine type") +
  theme_bw() +
  custom_theme +
  theme(legend.position = "top",
        legend.key.width = unit(1, 'cm'))-> p_legend2

legend2 <- get_legend(p_legend2)


p <- plot_grid(legend1, legend2,
               row1, row2, nrow = 4, rel_heights = c(0.5, 0.5, 5, 11))

ggsave("figs/fig4.png", plot = p, width = 10, height = 15)

p1 +
  facet_wrap(~scenario)

ggsave("figs/fig4a_BySpeed.png", width = 15, height = 5)

p4 +
  facet_wrap(~scenario)

ggsave("figs/fig4b_BySpeed.png", width = 15, height = 5)


p2 +
  facet_grid(scenario~threshold)

ggsave("figs/fig4c_BySpeed.png", width = 10, height = 15)

p3 +
  facet_grid(scenario~econ_id)

ggsave("figs/fig4d_BySpeed.png", width = 10, height = 15)

plot_ICER_grid <- function(version = "bc",
                           GDP_p = 0.5){

  seg2 <- GDP_p
  case_when(
    seg2 == 0.5 ~ "05",
    seg2 == 0.3 ~ "03",
    seg2 == 1 ~ "10"
  ) -> seg2

  seg3 <- version
  case_when(
    seg3 == "bc" ~ "",
    seg3 == "ext" ~ "_ext",
    seg3 == "low" ~ "_low"
  ) -> seg3

  name1 <- paste0("az_",seg2,seg3)
  name2 <- paste0("pf_",seg2,seg3)

  for(i in group_income$`Income Group` %>% unique){
    ICER[[name1]] %>%
      mutate(Type = "Viral Vector Vaccines") %>%
      bind_rows(ICER[[name2]] %>%
                  mutate(Type = "mRNA Vaccines")) %>%
      filter(econ_id == 1) %>%
      # filter(scenario == "fast", Type == "Viral Vector Vaccines", date_start == "2021-01-01")
      mutate(scenario = factor(scenario, levels = c("slow", "medium", "fast"),
                               labels = c("Slow", "Medium", "Fast")),
             ICER_factor=cut(ICER_scaled,
                             breaks=c(-0.5, 0.1, 0.3, 0.5, 1, 16),
                             labels=c("<0.1", "0.1-0.3", "0.3-0.5",
                                      "0.5-1", ">1"))) %>%
      left_join(group_income, by = "iso3c") %>%
      filter(`Income Group` == i) %>%
      ggplot(., aes(x = date_start, y = reorder(iso3c, ICER_scaled), fill = ICER_factor)) +
      geom_tile(color = "black", size = 0.5) +
      facet_grid(scenario ~ Type) +
      scale_fill_manual(values=rev(brewer.pal(6, "YlGnBu")), na.value="grey90") +
      scale_x_continuous(breaks = ymd("2021-06-01")) +
      theme_bw() +
      custom_theme +
      # coord_fixed(ratio = 1) +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.grid = element_blank(),
            legend.position = "top",
            legend.key.size = unit(1, "cm")) +
      labs(x = "Availability of Supply measured by\nVaccine Roll-out Start Date",
           fill = "ICER as a proportion\nof GDP per capita") -> p_list[[i]]
  }

  p1 <- plot_grid(plot_grid(NA,
                            p_list$LIC + 
                              theme(legend.position = "none") + 
                              labs(title = "A. Low Income"),
                            rel_heights = c(1, 5), ncol = 1),
                  p_list$LMIC + theme(legend.position = "none") + 
                    labs(title = "B. Lower Middle Income"),
                  align = "h")

  plot_grid(plotlist = list(p1,
                            plot_grid(get_legend(p_list$LIC + 
                                                   guides(fill=guide_legend(nrow=5,
                                                                            byrow=TRUE))),
                                      NA,
                                      p_list$UMIC + 
                                        theme(legend.position = "none") + 
                                        labs(title = "C. Upper Middle Income"),
                                      rel_heights = c(6,1,5), ncol = 1)
                            ),
            ncol = 2,
            rel_widths = c(2,1)) -> p_all

  return(p_all)
}

p_ICER <- list()
p_ICER[["base_case"]] <- plot_ICER_grid()
ggsave(filename = "figs/R2R_R1/fig3_v2_fixed.png",
       plot = p_ICER[["base_case"]],
       width = 18, height = 10)
# p_ICER[["base_case_ext"]] <- plot_ICER_grid(version = "ext")
# p_ICER[["base_case_low"]] <- plot_ICER_grid(version = "low")
# 
# ICER$az_05 %>% 
#   mutate(Type = "AZ") %>% 
#   bind_rows(ICER$pf_05 %>% 
#               mutate(Type = "Pfizer")) %>%
#   mutate(threshold = 0.5) %>% 
#   bind_rows(ICER$az_03 %>% 
#               mutate(Type = "AZ") %>% 
#               bind_rows(ICER$pf_03 %>% 
#                           mutate(Type = "Pfizer")) %>%
#               mutate(threshold = 0.3)) %>% 
#   bind_rows(ICER$az_10 %>% 
#               mutate(Type = "AZ") %>% 
#               bind_rows(ICER$pf_10 %>% 
#                           mutate(Type = "Pfizer")) %>%
#               mutate(threshold = 1)) %>% 
#   group_by(date_start, econ_id, scenario, Type, threshold) %>% 
#   summarise(ICER_CE = sum(ICER_scaled_bin)) %>% 
#   filter(threshold == 0.5) %>% 
#   mutate(ICER_CE_prop = ICER_CE/nrow(fitted_table),
#          scenario = factor(scenario, levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast")),
#          Type = factor(Type, levels = c("AZ", "Pfizer"), labels = c("Viral Vector Vaccines", "mRNA Vaccines")),
#          threshold = paste0("Decision Threshold = ",threshold," * GDP"),
#          econ_id = factor(econ_id, levels = 1:3,
#                           labels = c("Discount Rate = 0.03\nSMR = 1",
#                                      "Discount Rate = 0\nSMR = 1",
#                                      "Discount Rate = 0.03\nSMR = 1.5"))) %>% 
#   # filter(econ_id == "Discount Rate = 0.03\nSMR = 1") %>% 
#   ggplot(., aes(x = date_start, y = ICER_CE_prop, 
#                 color = econ_id)) +
#   geom_line() +
#   geom_smooth(alpha = 0.5, aes(fill = econ_id)) +
#   facet_grid(Type ~ scenario) +
#   labs(x = "Availability of Supply measured by\nVaccine Roll-out Start Date",
#        y = "Proportion of Countries with Cost-Effective Strategies") +
#   theme_bw() +
#   custom_theme +
#   theme(legend.position = "top") +
#   labs(color = "", fill= "") # +
#   # scale_color_futurama() +
#   # scale_fill_futurama()
# 
# ICER$az_05 %>% 
#   mutate(Type = "AZ") %>% 
#   bind_rows(ICER$pf_05 %>% 
#               mutate(Type = "Pfizer")) %>%
#   mutate(version = "bc") %>% 
#   bind_rows(ICER$az_05_low %>% 
#               mutate(Type = "AZ") %>% 
#               bind_rows(ICER$pf_05_low %>% 
#                           mutate(Type = "Pfizer")) %>%
#               mutate(version = "low")) %>% 
#   group_by(date_start, econ_id, scenario, Type, version) %>% 
#   summarise(ICER_CE = sum(ICER_scaled_bin)) %>% 
#   mutate(ICER_CE_prop = ICER_CE/nrow(fitted_table),
#          scenario = factor(scenario, levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast")),
#          Type = factor(Type, levels = c("AZ", "Pfizer"), labels = c("Viral Vector Vaccines", "mRNA Vaccines"))#,
#          # threshold = paste0("Decision Threshold = ",threshold," * GDP"),
#          # econ_id = factor(econ_id, levels = 1:3,
#                           # labels = c("Discount Rate = 0.03\nSMR = 1",
#                             #          "Discount Rate = 0\nSMR = 1",
#                               #        "Discount Rate = 0.03\nSMR = 1.5"))
#          ) %>% 
#   # filter(econ_id == "Discount Rate = 0.03\nSMR = 1") %>% 
#   filter(econ_id == 1) %>% 
#   ggplot(., aes(x = date_start, 
#                 y = ICER_CE_prop, 
#                 color = version)) +
#   geom_line() +
#   geom_smooth(alpha = 0.5, aes(fill = version)) +
#   facet_grid(Type ~ scenario) +
#   labs(x = "Availability of Supply measured by\nVaccine Roll-out Start Date",
#        y = "Proportion of Countries with Cost-Effective Strategies") +
#   theme_bw() +
#   custom_theme +
#   theme(legend.position = "top") +
#   labs(color = "", fill= "") 
# 
# ICER$az_05 %>% 
#   mutate(Type = "AZ") %>% 
#   bind_rows(ICER$pf_05 %>% 
#               mutate(Type = "Pfizer")) %>%
#   mutate(version = "bc") %>% 
#   bind_rows(ICER$az_05_ext %>% 
#               mutate(Type = "AZ") %>% 
#               bind_rows(ICER$pf_05_ext %>% 
#                           mutate(Type = "Pfizer")) %>%
#               mutate(version = "ext")) %>% 
#   group_by(date_start, econ_id, scenario, Type, version) %>% 
#   summarise(ICER_CE = sum(ICER_scaled_bin)) %>% 
#   mutate(ICER_CE_prop = ICER_CE/nrow(fitted_table),
#          scenario = factor(scenario, levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast")),
#          Type = factor(Type, levels = c("AZ", "Pfizer"), labels = c("Viral Vector Vaccines", "mRNA Vaccines"))#,
#          # threshold = paste0("Decision Threshold = ",threshold," * GDP"),
#          # econ_id = factor(econ_id, levels = 1:3,
#          # labels = c("Discount Rate = 0.03\nSMR = 1",
#          #          "Discount Rate = 0\nSMR = 1",
#          #        "Discount Rate = 0.03\nSMR = 1.5"))
#   ) %>% 
#   # filter(econ_id == "Discount Rate = 0.03\nSMR = 1") %>% 
#   filter(econ_id == 1) %>% 
#   ggplot(., aes(x = date_start, 
#                 y = ICER_CE_prop, 
#                 color = version)) +
#   geom_line() +
#   geom_smooth(alpha = 0.5, aes(fill = version)) +
#   facet_grid(Type ~ scenario) +
#   labs(x = "Availability of Supply measured by\nVaccine Roll-out Start Date",
#        y = "Proportion of Countries with Cost-Effective Strategies") +
#   theme_bw() +
#   custom_theme +
#   theme(legend.position = "top") +
#   labs(color = "", fill= "") 
