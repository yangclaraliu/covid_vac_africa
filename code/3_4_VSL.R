
# impact <- read_rds("data/intermediate/impact.rds")

# impact$az %>% 
#   dplyr::select(epi_id, econ_id, country, vsl) %>% 
#   mutate(Type = "Viral Vector Vaccines") %>% 
#   bind_rows(impact$pfizer %>% 
#               dplyr::select(epi_id, econ_id, country, vsl) %>% 
#               mutate(Type = "mRNA Vaccines")) %>% 
#   left_join(impact$novac %>% 
#               dplyr::select(epi_id, econ_id, country, vsl) %>% 
#               rename(vsl_novac = vsl),
#             by = c("epi_id", "econ_id", "country")) %>% 
#   rename(scenario_id = epi_id,
#          iso3c = country) %>% 
#   left_join(tab$az %>% 
#               dplyr::select(scenario_id, econ_id, iso3c, tot_cost, tot_cost_novac),
#             by = c("scenario_id", "econ_id", "iso3c")) %>% 
#   dplyr::select(-econ_id) %>% 
#   distinct() %>% 
#   mutate(vsl_diff = vsl_novac - vsl,
#          cost_diff = tot_cost_novac - tot_cost) %>% 
#   left_join(vac_denom, by = "iso3c") %>% 
#   mutate(tot = tot*1000,
#          NMB = vsl_diff - cost_diff,
#          NMBpc = NMB/tot) %>% 
#   left_join(ms_scenarios, by = "scenario_id") %>% 
#   left_join(group_income, by = "iso3c") %>% 
#   mutate(sign = NMBpc < 0) -> tab_NMB

BCA <- read_rds("data/intermediate/BCA_all.rds")

plot_BCA_grid <- function(version = "bc"){
  p_list <- list()
  for(i in group_income$`Income Group` %>% unique){
    if(version == "bc") tmp <- bind_rows(BCA$az,BCA$pf)
    if(version == "ext") tmp <- bind_rows(BCA$az_ext,BCA$pf_ext)
    if(version == "low") tmp <- bind_rows(BCA$az_low,BCA$pf_low)
    
    tmp %>% 
      left_join(group_income, by = "iso3c") %>% 
      mutate(scenario = factor(scenario, levels = c("slow", "medium", "fast"),
                               labels = c("Slow", "Medium", "Fast"))) %>% 
      filter(`Income Group` == i) %>%
      mutate(BCR_scaled = case_when(BCR > 1 ~ 1.1,
                                    BCR < 0 ~ -0.1,
                                    TRUE ~ BCR)) %>% 
      ggplot(., aes(x = date_start, y = reorder(iso3c, BCR_scaled), fill = BCR_scaled)) +
      geom_tile(size = 0.5, color = "black") +
      facet_grid(scenario ~ Type) +
      scale_x_continuous(breaks = ymd("2021-06-01")) +
      viridis::scale_fill_viridis(option = "mako"#, 
                                  # limits = range(tab_NMB$NMBpc)
      ) +
      # geom_tile(size = 0.5, aes(fill = sign)) +
      # scale_fill_manual(values = c("black", "orange")) +
      theme_bw() +
      custom_theme +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.grid = element_blank(),
            legend.text = element_text(angle = 45),
            legend.position = "top",
            legend.key.size = unit(1, "cm")) +
      labs(x = "Vaccination Programme Starting Date",
           fill = "Benefit-Cost Ratio") -> p_list[[i]]
  }
  
  plot_grid(plotlist = list(p_list$LIC + 
                              theme(strip.text.y = element_blank()) + 
                              labs(title = "A. Low Income"),
                            p_list$LMIC + 
                              labs(title = "B. Lower Middle Income"),
                            plot_grid(# get_legend(p_list$LIC), 
                              NA,
                              p_list$UMIC + 
                                # theme(legend.position = "none") + 
                                labs(title = "C. Upper Middle Income"), 
                              rel_heights = c(3, 4), 
                              ncol = 1)),
            ncol = 3)
}

BCA_plots <- list()
BCA_plots[["bc"]] <- plot_BCA_grid()
