# ve_i_o: observed VE against infection
# ve_d_o: observed VE against disease
# ve_d: VE against disease conditioned on changes occurring RE infection
# ve_severe, ve_critical, ve_mort: observed VE against different outcomes
# ve_severe_condition, ve_critical_condition, ve_mort_condition: VE against different outcomes condition on infection

# vaccine efficacy tested
data.table(ve_i_o = c(0.7, 0.85),
           ve_d_o = c(0.7, 0.9),
           ve_severe = c(0.85, 0.95),
           ve_critical = c(0.85, 0.95),
           ve_mort = c(0.85, 0.95)) %>% 
  # the following lines do not explicit reflect existing changes in infection
  # which has been explicitly modelled as changes in u
  mutate(ve_d = 1 - (1-ve_d_o)/((1-ve_i_o)),
         ve_severe_condition = 1 - (1-ve_severe)/((1-ve_i_o)),
         ve_critical_condition = 1 - (1-ve_critical)/((1-ve_i_o)),
         ve_mort_condition = 1 - (1-ve_mort)/((1-ve_i_o))) -> ve_pfizer

data.table(ve_i_o = c(0.7, 0.75),
           ve_d_o = c(0.7, 0.8),
           ve_severe = c(0.85, 0.9),
           ve_critical = c(0.85, 0.93),
           ve_mort = c(0.85, 0.95)) %>% 
  # the following lines do not explicit reflect existing changes in infection
  # which has been explicitly modelled as changes in u
  mutate(ve_d = 1 - (1-ve_d_o)/((1-ve_i_o)),
         ve_severe_condition = 1 - (1-ve_severe)/(1-ve_i_o),
         ve_critical_condition = 1 - (1-ve_critical)/(1-ve_i_o),
         ve_mort_condition = 1 - (1-ve_mort)/(1-ve_i_o)) -> ve_az

         