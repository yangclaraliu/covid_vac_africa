# shape %>% 
#   mutate(fit = if_else(ISO3_CODE %in% tmp$iso3c, "Included", "Excluded")) %>% 
#   st_as_sf() %>% 
#   ggplot(.) +
#   geom_sf(aes(fill = fit)) +
#   coord_sf(xlim = c(-30, 60), ylim = c(-40, 40), expand = FALSE) +
#   theme_map() +
#   scale_fill_manual(values = c("white","#0D5257")) +
#   theme(legend.position = "top") +
#   labs(fill = "") -> p
# 
# ggsave("figs/Report/Fitted_Countries.png", p, height = 5, width = 5)

# p <- list()
# for(i in 1:nrow(fitted_table)){
#   gen_country_basics(country = fitted_table$loc[i],
#                      waning_nat = 52*7*3,
#                      R0_assumed = fitted_table$r[i],
#                      date_start = as.character(ymd("2019-12-01") + fitted_table$t0[i]),
#                      processes = burden_processes,
#                      deterministic = T) %>%
#     update_vac_char(.,
#                     ve_i   = ve$ve_i_o[1],  # infection blocking VE post 1 dose
#                     v2e_i  = ve$ve_i_o[2],  # infection blocking VE post 2 doses
#                     ve_d   = ve$ve_d[1],    # clinical fraction among breakthrough post 1 dose
#                     v2e_d  = ve$ve_d[2],    # clinical fraction among breakthrough post 2 doses
#                     wv = 1/360) %>% 
#     change_VOC(.,
#                date_switch = c(as.character(fitted_table$t_intro_voc1[i]), 
#                                as.character(fitted_table$t_intro_voc2[i]),
#                                as.character(fitted_table$t_intro_voc3[i])),
#                rc_severity = c(fitted_table$rc_severity_1[i], 
#                                fitted_table$rc_severity_2[i], 
#                                fitted_table$rc_severity_3[i]), 
#                rc_transmissibility = c(fitted_table$rc_transmissibility_1[i], 
#                                        fitted_table$rc_transmissibility_2[i], 
#                                        fitted_table$rc_transmissibility_3[i]), 
#                rc_ve = c(fitted_table$rc_ve_1[i], 
#                          fitted_table$rc_ve_2[i], 
#                          fitted_table$rc_ve_3[i]) # relative in ve against infection
#     ) %>% 
#     cm_simulate() %>% 
#     .[["dynamics"]] %>% 
#     filter(grepl("death", compartment)) %>% 
#     group_by(t, compartment) %>% 
#     summarise(value = sum(value), .groups = "drop") %>% 
#     mutate(date = ymd("2019-12-01") + fitted_table$t0[i] + t) %>% 
#     filter(date <= fitted_table$t_end_fitting[i]) %>% 
#     pivot_wider(names_from = compartment, values_from = value) %>% 
#     mutate(deaths_sim = case_when(date <= (ymd(fitted_table$t_intro_voc1[i])) ~ death_o,
#                                   
#                                   date > (ymd(fitted_table$t_intro_voc1[i])) & 
#                                     date <=  (ymd(fitted_table$t_intro_voc2[i])) ~ death_voc1_o,
#                                   
#                                   date > (ymd(fitted_table$t_intro_voc2[i])) & 
#                                     date <=  (ymd(fitted_table$t_intro_voc3[i])) ~ death_voc2_o,
#                                   
#                                   date > (ymd(fitted_table$t_intro_voc3[i])) ~ death_voc3_o),
#            scaled = deaths_sim*fitted_table$rr[i],
#            date = as.character(date)) %>% 
#     left_join(tmp %>% 
#                 filter(iso3c == fitted_table$iso3c[i]) %>% 
#                 ungroup %>% 
#                 dplyr::select(date, deaths) %>% 
#                 mutate(date = as.character(date)), 
#               by = "date") %>% 
#     mutate(deaths = if_else(is.na(deaths), 0, deaths),
#            date = ymd(date))  -> res
#   
#   
#   res %>% 
#     ggplot(., aes(x = date, color = "loc")) +
#     geom_point(aes(y = scaled)) +
#     geom_line(aes(y = deaths)) +
#     scale_color_manual(values = "black") +
#     theme_bw() +
#     labs(title = paste0(fitted_table$index[i],
#                         "_",
#                         fitted_table$loc[i]))+
#     theme(legend.position = "none") -> p[[i]]
#   
#   # fn_tmp <- paste0("figs/intermediate/fitting_20220324/",
#   #                  fitted_table$index[i],"_",fitted_table$loc[i],
#   #                  ".png")
#   # 
#   # ggsave(fn_tmp, p)
# }
# 
# highlight <- which(fitted_table$loc %in% c("Ethiopia", "Ghana", "Nigeria"))
# 
# for(i in highlight){
#   p[[i]] <- p[[i]] +
#     scale_color_manual(values = "#0D5257") +
#     theme(legend.position = "none") 
# }
# 
# p_all <- cowplot::plot_grid(plotlist = p)
# 
# ggsave("figs/Report/Fitted_Results.png",
#        p_all, height = 10, width = 20)

fitted_table %>% 
  dplyr::select(index, r, rr) %>% 
  pivot_longer(cols = c("r", "rr")) %>% 
  mutate(name = factor(name,
                       levels = c("r","rr"),
                       labels = c("R0", "Reporting Rate"))) %>% 
  ggplot(., aes(x = value)) +
  geom_histogram(color = NA, fill = "#0D5257") +
  facet_wrap(~name, scales = "free", ncol = 1) +
  theme_bw() -> p1

fitted_table %>% 
  dplyr::select(index, loc, t_intro, t_intro_voc2 ) %>% 
  filter(!loc %in% c("Ghana", "Ethiopia", "Nigeria")) %>% 
  pivot_longer(cols = c("t_intro", "t_intro_voc2")) %>% 
  mutate(name = factor(name,
                       levels = c("t_intro","t_intro_voc2"),
                       labels = c("Infection Introduction", 
                                  "VOC Introduction"))) %>% 
  ggplot(., aes(x = value)) +
  geom_histogram(color = NA, fill = "#0D5257") +
  facet_wrap(~name, ncol = 1) +
  theme_bw() -> p2

plot_grid(p1 + labs(x = ""), 
          p2 + labs(x = ""), 
          nrow = 1) -> p

ggsave("figs/Report/Fitted_Parameters.png",
       p, height = 8, width = 15)
