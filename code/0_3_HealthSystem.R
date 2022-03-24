
##### healthcare parameters ####
critical2 <- 0
picu_cocin_func <- function(age) {
  x <- c(-0.1309118, 0, 17.2398874, 65.7016492, 100)
  y <- c(-2.1825091, -2.1407043, -1.3993552, -1.2344361, -8.8191062)
  p <- splinefun(x, y)(age)
  exp(p) / (1 + exp(p))
}
picu_cocin <- picu_cocin_func(0:85)

# Infection fatality rate (derived from Levin et al., preprint)
ifr_levin <- 100 * exp(-7.56 + 0.121 * 0:85) / (100 + exp(-7.56 + 0.121 * 0:85)) / 100
# Infection hospitalisation rate (derived from Salje et al., Science)
ihr_salje <- exp(-7.37 + 0.068 * 0:85) / (1 + exp(-7.37 + 0.068 * 0:85))
# Amalgamate probabilities
probabilities <- data.table(age = 0:85, ihr = ihr_salje, ifr = ifr_levin, picu = picu_cocin)
probabilities[, age_group := pmin(15, age %/% 5)]
probabilities <- probabilities[, lapply(.SD, mean), by = age_group, .SDcols = 2:4]

# Create model burden processes
P.critical <- probabilities[, ihr * picu]
P.severe <- probabilities[, ihr * (1 - picu)]
P.death <- probabilities[, ifr]
P.hosp <- P.critical + P.severe

delay_2death <- cm_delay_gamma(26, 5, 60, 0.25)$p
delay_2severe <- cm_delay_gamma(8.5, 5, 60, 0.25)$p
delay_2hosp <- cm_delay_gamma(14.6, 5, 60, 0.25)$p

burden_processes <- list(
  cm_multinom_process("E",       
                      data.frame(death = P.death),                   
                      delays = data.frame(death = delay_2death), report = "o"),
  cm_multinom_process("Ev",      
                      data.frame(death = P.death*(1-ve$ve_d[1])*(1-ve$ve_cfr[1])), 
                      delays = data.frame(death = delay_2death), report = "o"),
  cm_multinom_process("Ev2",     
                      data.frame(death = P.death*(1-ve$ve_d[2])*(1-ve$ve_cfr[2])), 
                      delays = data.frame(death = delay_2death), report = "o"),
  
  
  cm_multinom_process("E",       
                      data.frame(to_hosp = P.hosp),                  
                      delays = data.frame(to_hosp = delay_2severe)),
  cm_multinom_process("Ev",      
                      data.frame(to_hosp = P.hosp*(1-ve$ve_d[1])*(1-ve$ve_scr[1])),   
                      delays = data.frame(to_hosp = delay_2severe)),
  cm_multinom_process("Ev2",     
                      data.frame(to_hosp = P.hosp*(1-ve$ve_d[2])*(1-ve$ve_scr[2])),   
                      delays = data.frame(to_hosp = delay_2severe)),
  
  cm_multinom_process("to_hosp", 
                      data.frame(hosp = rep(1,16)),                  
                      delays = data.frame(hosp = delay_2hosp),   report = "ip")
)
