##### expand VE estimates to meet the needs of the model ####
exp_ve <- function(ve_d_o,  # disease blocking VE observed
                   ve_i_o   # infection blocking VE assumed
){
  # calculate of clinical fraction reduction
  ve_d <- (ve_d_o - ve_i_o)/(1 - ve_i_o)
  return(ve_d)
}

# vaccine efficacy tested
data.table(ve_i_o = c(0.7, 0.85),
           ve_d_o = c(0.7, 0.89)) %>% 
  mutate(
    ve_d = exp_ve(ve_d_o, ve_i_o),
    ve_h = c(0.845, 0.9),
    ve_mort = c(0.845, 0.95)) %>% 
  mutate(ve_scr = 1 - (1-ve_h)/((1-ve_i_o) * (1-ve_d)),
         ve_cfr = 1 - (1-ve_mort)/((1-ve_i_o) * (1-ve_d))) -> ve

         