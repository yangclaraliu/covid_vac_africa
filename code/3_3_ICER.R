impact <- read_rds("data/intermediate/impact_combined.rds")

tab <- list()
# tab[["bc"]] <- tab[["ext"]] <- tab[["low"]] <- list()

lapply(impact, "[[", "az") %>% 
  map(rename, scenario_id = epi_id) %>% 
  map(right_join,
      cost_all %>% 
        filter(Type == "az"),
      by = c("scenario_id", "country" = "iso3c")) -> tab[["az"]]

lapply(impact, "[[", "pfizer") %>% 
  map(rename, scenario_id = epi_id) %>% 
  map(right_join,
      cost_all %>% 
        filter(Type == "pfizer"),
      by = c("scenario_id", "country" = "iso3c")) -> tab[["pfizer"]]


lapply(impact, "[[", "novac") %>% 
  map(rename, 
      scenario_id = epi_id,
      dalys_novac = dalys) -> tab[["novac"]]

# cost_all %>% 
#   filter(Type == "az") %>% 
#   left_join(impact$az %>% 
#               rename(scenario_id = epi_id), 
#             by = c("scenario_id", "iso3c" = "country")) -> tab[["az"]]

# cost_all %>% 
#   filter(Type == "pfizer") %>% 
#   left_join(impact$pfizer %>% 
#               rename(scenario_id = epi_id), 
#             by = c("scenario_id", "iso3c" = "country")) -> tab[["pfizer"]]

# impact$novac %>% 
#   rename(scenario_id = epi_id) %>% 
#   # dplyr::select(scenario_id, econ_id, country, ylds, ylls, dalys,GDPPC_2020_USD) %>% 
#   rename(dalys_novac = dalys) -> tab[["novac"]]

compile_ICER_by_threshold <- function(GDP_p = 0.5, 
                                      version = "bc", 
                                      vac_type){
  tab[[vac_type]][[version]] %>% 
  # tab$az$bc %>% 
    # dplyr::select(scenario_id, econ_id, iso3c, Type, tot_cost, tot_cost_novac, dalys) %>% 
    dplyr::select(scenario_id, econ_id, country, Type, tot_cost, tot_cost_novac, dalys) %>% 
    distinct() %>% 
    left_join(tab[["novac"]][[version]],
              by = c("scenario_id",
                     "econ_id",
                     # "iso3c" = "country"
                     "country"
                     )) %>% 
    rename(iso3c = country) %>% 
    left_join(ms_scenarios, by = "scenario_id") %>% 
    # filter(econ_id == 1) %>% 
    mutate(diff_health = dalys_novac - dalys,
           diff_cost = tot_cost- tot_cost_novac,
           ICER = diff_cost/diff_health,
           ICER_scaled = ICER/GDPPC_2020_USD,
           ICER_scaled_bin = if_else(ICER_scaled >= GDP_p, F, T)) -> m
  return(m)
}

compile_BCA <- function(version = "bc", 
                        vac_type){
  tab[[vac_type]][[version]] %>% 
    # tab$az$bc %>% 
    # dplyr::select(scenario_id, econ_id, iso3c, Type, tot_cost, tot_cost_novac, dalys) %>% 
    dplyr::select(scenario_id, econ_id, country, Type, tot_cost, tot_cost_novac, vsl) %>% 
    distinct() %>% 
    left_join(tab[["novac"]][[version]] %>% 
                rename(vsl_novac = vsl),
              by = c("scenario_id",
                     "econ_id",
                     # "iso3c" = "country"
                     "country"
              )) %>% 
    rename(iso3c = country) %>% 
    left_join(ms_scenarios, by = "scenario_id") %>% 
    mutate(benefit = vsl_novac - vsl,
           cost = tot_cost - tot_cost_novac,
           BCR = benefit/cost) -> m
  return(m)
}

ICER <- list()
ICER[["az_03"]] <- compile_ICER_by_threshold(GDP_p = 0.3, vac_type = "az")
ICER[["az_05"]] <- compile_ICER_by_threshold(GDP_p = 0.5, vac_type = "az")
ICER[["az_10"]] <- compile_ICER_by_threshold(GDP_p = 1, vac_type = "az")
ICER[["pf_03"]] <- compile_ICER_by_threshold(GDP_p = 0.3, vac_type = "pfizer")
ICER[["pf_05"]] <- compile_ICER_by_threshold(GDP_p = 0.5, vac_type = "pfizer")
ICER[["pf_10"]] <- compile_ICER_by_threshold(GDP_p = 1, vac_type = "pfizer")

ICER[["az_03_ext"]] <- compile_ICER_by_threshold(GDP_p = 0.3, vac_type = "az", version = "ext")
ICER[["az_05_ext"]] <- compile_ICER_by_threshold(GDP_p = 0.5, vac_type = "az", version = "ext")
ICER[["az_10_ext"]] <- compile_ICER_by_threshold(GDP_p = 1, vac_type = "az", version = "ext")
ICER[["pf_03_ext"]] <- compile_ICER_by_threshold(GDP_p = 0.3, vac_type = "pfizer", version = "ext")
ICER[["pf_05_ext"]] <- compile_ICER_by_threshold(GDP_p = 0.5, vac_type = "pfizer", version = "ext")
ICER[["pf_10_ext"]] <- compile_ICER_by_threshold(GDP_p = 1, vac_type = "pfizer", version = "ext")

ICER[["az_03_low"]] <- compile_ICER_by_threshold(GDP_p = 0.3, vac_type = "az", version = "low")
ICER[["az_05_low"]] <- compile_ICER_by_threshold(GDP_p = 0.5, vac_type = "az", version = "low")
ICER[["az_10_low"]] <- compile_ICER_by_threshold(GDP_p = 1, vac_type = "az", version = "low")
ICER[["pf_03_low"]] <- compile_ICER_by_threshold(GDP_p = 0.3, vac_type = "pfizer", version = "low")
ICER[["pf_05_low"]] <- compile_ICER_by_threshold(GDP_p = 0.5, vac_type = "pfizer", version = "low")
ICER[["pf_10_low"]] <- compile_ICER_by_threshold(GDP_p = 1, vac_type = "pfizer", version = "low")

write_rds(ICER, "data/intermediate/ICER_all.rds")

BCA <- list()
BCA[["az"]] <- compile_BCA(version = "bc", vac_type = "az")
BCA[["pf"]] <- compile_BCA(version = "bc", vac_type = "pfizer")
BCA[["az_ext"]] <- compile_BCA(version = "ext", vac_type = "az")
BCA[["pf_ext"]] <- compile_BCA(version = "ext", vac_type = "pfizer")
BCA[["az_low"]] <- compile_BCA(version = "low", vac_type = "az")
BCA[["pf_low"]] <- compile_BCA(version = "low", vac_type = "pfizer")
 
write_rds(BCA, "data/intermediate/BCA_all.rds")

# ICER$az %>%
#   group_by(scenario_id, econ_id) %>%
#   mutate(GDP_rank = rank(GDPPC_2020_USD),
#          GDP_label = paste0(GDP_rank, "_",iso3c)) -> tmp
# 
# tmp %>%
#   ungroup %>%
#   select(iso3c, GDP_rank, GDP_label) %>%
#   arrange(GDP_rank) %>%
#   disgeom_tile(color = "black", size = 0.25) +
# geom_tile(color = "black", size = 0.25) +
# geom_tile(color = "black", size = 0.25) +
# geom_tile(color = "black", size = 0.25) +
# geom_tile(color = "black", size = 0.25) +
# geom_tile(color = "black", size = 0.25) +
# geom_tile(color = "black", size = 0.25) +
# tinct() %>%
#   pull(GDP_label) -> tmp_labels
# 
# tmp %>%
#   mutate(GDP_label = factor(GDP_label, levels = tmp_labels)) %>%
#   ggplot(., aes(x = diff_health, y = diff_cost, color = date_start)) +
#   geom_point(aes(pch = scenario)) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~GDP_label, scales = "free") +
#   geom_abline(aes(intercept = 0, slope = GDPPC_2020_USD*0.5))

ICER$az_05 %>% 
  mutate(Type = "Viral Vector Vaccine") %>% 
  bind_rows(ICER$pf_05 %>% 
              mutate(Type = "mRNA Vaccine")) %>% 
  group_by(date_start, econ_id, Type, iso3c) %>% 
  mutate(ICER_rank = rank(desc(ICER)) %>% factor(., levels = 1:3),
         iso3c = factor(iso3c),
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast"))) %>% 
  group_by(date_start, scenario, econ_id, Type, ICER_rank, ICER_scaled_bin ) %>% tally %>% 
  # filter(Type == "Pfizer") %>% 
  filter(econ_id == 1) %>% 
  # filter(ICER_scaled_bin == T) %>% 
  ggplot(., aes(x = date_start, y = n, color = ICER_rank, fill = ICER_rank)) +
  geom_bar(stat = "identity", position = "stack", width=32) +
  scale_fill_rickandmorty() +
  scale_color_rickandmorty() +
  facet_grid(Type~scenario) +
  theme_bw() +
  custom_theme +
  theme(legend.position = "top") +
  labs(y = "Counts",
       color = "ICER Ranking",
       fill = "ICER Ranking",
       x = "Availability of Supply measured by\nVaccine Roll-out Start Date")

# ggsave("figs/ICER_ranking_full_v3.png", height = 10, width = 20)

#### ICER RAW #### 
p_list <- list()

require(RColorBrewer)

for(i in group_income$`Income Group` %>% unique){
ICER$az_05 %>% 
  mutate(Type = "Viral Vector Vaccines") %>% 
  bind_rows(ICER$pf_05 %>% 
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
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.key.size = unit(1, "cm")) +
  labs(x = "Availability of Supply measured by\nVaccine Roll-out Start Date",
       fill = "Normalised ICER") -> p_list[[i]]
}

p1 <- plot_grid(plot_grid(NA, 
                       p_list$LIC + theme(legend.position = "none") + labs(title = "A. Low Income"),
                       rel_heights = c(1, 5), ncol = 1),
                p_list$LMIC + theme(legend.position = "none") + labs(title = "B. Lower Middle Income"),
                align = "h")

plot_grid(plotlist = list(p1,
                          plot_grid(get_legend(p_list$LIC + guides(fill=guide_legend(nrow=2,byrow=TRUE))), NA,
                                    p_list$UMIC + theme(legend.position = "none") + labs(title = "C. Upper Middle Income"), 
                                    rel_heights = c(1, 3,3), ncol = 1)),
          ncol = 2,
          rel_widths = c(2,1))

# # ggsave("figs/ICER_scaled_raw_income_econ_id_1_v3.png", height = 10, width = 20)
# ggsave("figs/fig3.png", height = 10, width = 20)

#### INLINE STATS ####
ICER$az_05 %>% 
  mutate(Type = "Viral Vector Vaccines") %>% 
  bind_rows(ICER$pf_05 %>% 
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
  filter(date_start == "2021-08-01") %>% 
  mutate(ICER_scaled_bin = ICER_scaled < 0.5) %>% 
  group_by(scenario, Type) %>% 
  summarise(CE = sum(ICER_scaled_bin),
            CE_n = n()) %>% 
  mutate(r = CE/CE_n) %>% arrange(Type)

ICER$az_05 %>% 
  mutate(Type = "AZ") %>% 
  bind_rows(ICER$pf_05 %>% 
              mutate(Type = "Pfizer")) %>%
  mutate(threshold = 0.5) %>% 
  bind_rows(ICER$az_03 %>% 
              mutate(Type = "AZ") %>% 
              bind_rows(ICER$pf_03 %>% 
                          mutate(Type = "Pfizer")) %>%
              mutate(threshold = 0.3)) %>% 
  bind_rows(ICER$az_10 %>% 
              mutate(Type = "AZ") %>% 
              bind_rows(ICER$pf_10 %>% 
                          mutate(Type = "Pfizer")) %>%
              mutate(threshold = 1)) %>% 
  group_by(date_start, econ_id, scenario, Type, threshold) %>% 
  summarise(ICER_CE = sum(ICER_scaled_bin)) %>% 
  filter(threshold == 0.5) %>% 
  mutate(ICER_CE_prop = ICER_CE/nrow(fitted_table),
         scenario = factor(scenario, levels = c("slow", "medium", "fast"),
                           labels = c("Slow", "Medium", "Fast")),
         Type = factor(Type, levels = c("AZ", "Pfizer"), labels = c("Viral Vector Vaccines", "mRNA Vaccines")),
         threshold = paste0("Decision Threshold = ",threshold," * GDP"),
         econ_id = factor(econ_id, levels = 1:3,
                          labels = c("Discount Rate = 0.03\nSMR = 1",
                                     "Discount Rate = 0\nSMR = 1",
                                     "Discount Rate = 0.03\nSMR = 1.5"))) %>% 
  # filter(econ_id == "Discount Rate = 0.03\nSMR = 1") %>% 
  ggplot(., aes(x = date_start, y = ICER_CE_prop, scenario, color = scenario)) +
  geom_line() +
  geom_smooth(alpha = 0.5, aes(fill = scenario)) +
  facet_grid(Type ~ econ_id) +
  labs(x = "Availability of Supply measured by\nVaccine Roll-out Start Date",
      y = "Proportion of Countries with Cost-Effective Strategies") +
  theme_bw() +
  custom_theme +
  theme(legend.position = "top") +
  labs(color = "", fill= "") +
  scale_color_futurama() +
  scale_fill_futurama()

# ggsave("figs/prop_CE_econ_v1.png", height = 10, width = 15)

# ICER$az_05 %>% 
#   mutate(Type = "Viral Vector Vaccines") %>% 
#   bind_rows(ICER$pf_05 %>% 
#               mutate(Type = "mRNA Vaccines")) %>% 
#   left_join(cost_hc_expenditure_GHED, by = "iso3c") %>% 
#   # left_join(ms_cov_all %>% mutate(Type = tolower(Type)), by = c("iso3c", "date_start", "scenario", "Type")) %>% 
#   mutate(affordability = (diff_cost/2)/hc_expenditure, #dividing 
#          scenario = factor(scenario, 
#                            levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast")),
#          # Type = factor(Type, levels = c("az","pfizer"),
#          #               labels = c("AZ", "Pfizer")),
#          date_start = factor(date_start)) %>% 
#   filter(!is.na(affordability), econ_id == 1) %>% mutate(date_start = ymd(date_start)) %>% 
#   # filter(affordability > 1) %>% group_by(name, Type) %>% tally
#   # filter(date_start %in% ymd(c("2021-01-01", 
#   #                          "2021-08-01",
#   #                          "2021-12-01")), !is.na(affordability), econ_id == 1) %>%
#   mutate(ICER_scaled = if_else(ICER_scaled > 1, 1.1, ICER_scaled)) %>% 
#   mutate(affordability = if_else(affordability > 1, 1.1, affordability),
#          m = month(date_start)) %>% 
#   # group_by(Type, scenario) %>% filter(ICER_scaled_bin == T) %>% 
#   # summarise(Q1 = quantile(affordability, 0.25),
#   #           md = quantile(affordability, 0.5),
#   #           Q3 = quantile(affordability, 0.75),
#   #           n = n()) %>% View()
#   # mutate(ICER_scaled  = if_else(ICER_scaled < 0, 0.05, ICER_scaled )) %>% 
#   # mutate(affordability  = if_else(affordability < 0, 0.05, affordability)) %>%  
#   # group_by(Type, scenario) %>% tally
#   ggplot(., aes(x = ICER_scaled, y = affordability, color = m)) +
#   geom_rect(aes(xmin = 1.05, xmax = 1.15, ymin = min(affordability), ymax = max(affordability)*1.15),
#             fill = "grey90", color = NA) +
#   geom_rect(aes(xmin = min(ICER_scaled)*0.95, xmax = 1.15, ymin = 1.05, ymax = max(affordability)*1.15),
#             fill = "grey90", color = NA) +
#   geom_point(alpha = 0.5) +
#   viridis::scale_color_viridis(breaks = c(3,6,9, 12),
#                                labels = c("Mar.",
#                                           "Jun.",
#                                           "Sep.",
#                                           "Dec."),
#                                option = "magma") +
#   # geom_line(aes(group = iso3c)) +
#   # ggplot(., aes(x = affordability, y = scenario,  
#   #               fill = scenario, 
#   #               color = scenario)) +
#   # ggplot(., aes(x = affordability, y = date_start)) +
#   # geom_density_ridges(alpha = 0.01) +
#   # geom_histogram() +
#   # geom_density_ridges(alpha = 0.5, scale = 0.95, stat = "binline", binwidth = 0.02) +
#   # geom_density(alpha = 0.1) +
#   facet_grid(scenario~Type) +
#   # facet_grid( ~ Type, scales = "free") +
#   # facet_grid(date_start ~ Type) +
#   theme_bw() +
#   theme(legend.position = "top") +
#   labs(color = "Vaccination Program Starting Date\n(2021)", fill = "", 
#        y = "Affordability\nIncremental Costs/General Healthcare Expenditure",
#        x = "Normalised ICER"
#        # y = "Frequency"
#        ) +
#   # scale_color_futurama() +
#   # scale_fill_futurama() +
#   custom_theme +
#   theme(panel.grid = element_blank(),
#         strip.background = element_rect(fill = NA),
#         legend.key.width = unit(1, 'cm')) +
#   # geom_vline(xintercept = 0.3, linetype = 2, color = "black") +
#   geom_vline(xintercept = 0.5, linetype = 1, color = "black") +
#   geom_vline(xintercept = 0, linetype = 1, color = "black") +
#   geom_hline(yintercept = 0, linetype = 1, color = "black") +
#   geom_hline(yintercept = 0.1, linetype = 2, color = "black") +
#   # geom_hline(yintercept = 0.15, linetype = 2, color = "black") +
#   geom_hline(yintercept = 0.2, linetype = 2, color = "black") +
#   scale_x_continuous(breaks = c(0, 0.3, 0.5, 1, 1.1),
#                      labels = c(0, 0.3, 0.5, 1, ">1")) + 
#   scale_y_continuous(breaks = c(0, 0.1, 0.2, 1, 1.1),
#                      labels = c("0%", "10%", "20%", "100%", ">100%"))
#   # scale_x_continuous(breaks = c(0, 0.5, 1), 
#   #                    labels = c("0%", "50%", "100%"), limits = c(-0.3,2)) 
# 
# # ggsave("figs/affordability_v2.png", height = 10, width = 20)
# # ggsave("figs/fig5.png", height = 10, width = 8)
# 
# # tab$az %>% 
# 
# tab$az %>% 
#   left_join(tab$novac %>% 
#               rename(iso3c = country,
#                      ylds_novac = ylds,
#                      ylls_novac = ylls),
#             c("scenario_id", "iso3c", "econ_id", "GDPPC_2020_USD")) %>% 
#   dplyr::select(scenario_id, iso3c, econ_id,
#                 starts_with(c("yll","yld","daly"))) %>%
#   mutate(ylls_rr = 1 - ylls/ylls_novac,
#          ylds_rr = 1 - ylds/ylds_novac,
#          dalys_rr = 1 - dalys/dalys_novac) %>% 
#   left_join(ms_scenarios, 
#             by = "scenario_id") %>% 
#   dplyr::select(scenario_id, iso3c, econ_id, scenario, ends_with("rr"), date_start) %>% 
#   pivot_longer(cols = c(starts_with(c("yll", "yld", "daly")))) %>% 
#   mutate(tag = substr(name,1,4),
#          scenario = factor(scenario,
#                            levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast")),
#          tag = factor(tag, 
#                       levels = c("ylds", "ylls","daly"),
#                       labels = c("Years of Life Lost due to Premature Mortality (YLLs)",
#                                  "Years Lived with Disability (YLDs)",
#                                  "Disability-justed Life Years (DALYs)"))) %>% 
#   ggplot(., aes(x = date_start, y = value, group = interaction(scenario, date_start),
#                 color = scenario)) +
#   # geom_density_ridges() +
#   geom_boxplot(outlier.shape = NA) +
#   facet_wrap(.~tag, ncol = 1) +
#   scale_color_futurama() +
#   theme_bw() +
#   custom_theme +
#   theme(panel.grid = element_blank(),
#         legend.position = "top") +
#   labs(y = "Relative Reduction\n(1-outcome w/ vaccine/ outcome w/o vaccine",
#        x = "Vaccine Roll-out Start Date",
#        color = "") +
#   geom_vline(xintercept = seq(ymd("2020-12-15"),
#                               ymd("2021-12-15"),
#                               by = "month"),
#              linetype = 2)
# 
# ggsave("figs/impact.png", width = 15, height = 8)

# ICER$az_05 %>% 
#   mutate(Type = "Viral Vector Vaccine") %>% 
#   bind_rows(ICER$pf_05 %>% 
#               mutate(Type = "mRNA Vaccine")) %>% 
#   group_by(date_start, econ_id, Type, iso3c) %>% 
#   left_join(vac_denom, by = "iso3c") %>% 
#   mutate(ICER_rank = rank(desc(ICER)) %>% factor(., levels = 1:3),
#          iso3c = factor(iso3c),
#          scenario = factor(scenario,
#                            levels = c("slow", "medium", "fast"),
#                            labels = c("Slow", "Medium", "Fast")),
#          diff_health_pc = diff_health/(tot*1000),
#          diff_cost_pc = diff_cost/(tot*1000)) %>% 
#   ggplot(., aes(x = diff_health_pc, y = diff_cost_pc, color = scenario)) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~date_start, nrow = 2) +
#   geom_vline(xintercept = 0, linetype = 2) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   scale_color_futurama() +
#   theme_bw() +
#   custom_theme +
#   theme(panel.grid = element_blank(),
#         legend.position = "top") +
#   labs(color = "",
#        x = "Difference in Dalys (normalised by population size)",
#        y = "Difference in Costs (normalised by population size)") 

# ggsave("figs/CE_plane_v3.png", width = 18, height = 8)
