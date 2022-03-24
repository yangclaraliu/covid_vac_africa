##### expand VE estimates to meet the needs of the model ####
exp_ve <- function(ve_d_o,  # disease blocking VE observed
                   ve_i_o   # infection blocking VE assumed
){
  # calculate of clinical fraction reduction
  ve_d <- (ve_d_o - ve_i_o)/(1 - ve_i_o)
  return(ve_d)
}


# ve_i_o: observed VE against infection
# ve_d_o: observed VE against disease
# ve_d: VE against disease conditioned on changes occurring RE infection
# ve_h: observed VE against hospitalisation
# ve_mort: observed VE against mortality
# ve_scr: VE against hospitalisation conditioned on existing changes RE infection and disease
# ve_cfr: VE against mortality conditioned on existing changes RE infection and disease

# vaccine efficacy tested
data.table(ve_i_o = c(0.7, 0.85),
           ve_d_o = c(0.7, 0.89)) %>% 
  mutate(
    ve_d = exp_ve(ve_d_o, ve_i_o),
    ve_h = c(0.845, 0.9),
    ve_mort = c(0.845, 0.95)) %>% 
  # the following lines do not explicit reflect existing changes in infection
  # which has been explicitly modelled as changes in u
  mutate(ve_scr = 1 - (1-ve_h)/((1-ve_i_o) * (1-ve_d)),
         ve_cfr = 1 - (1-ve_mort)/((1-ve_i_o) * (1-ve_d))) -> ve

         