# vaccine efficacy tested
data.table(ve_i_o = c(0.67, 0.68),
           ve_d_o = c(0.67, 0.78)) %>% 
  mutate(
    ve_d = exp_ve(ve_d_o, ve_i_o),
    ve_h = c(0.845, 0.9),
    ve_mort = c(0.845, 0.95)) -> ve
