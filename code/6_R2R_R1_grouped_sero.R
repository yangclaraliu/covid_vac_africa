#### base parameters ####
sim_end = "2021-12-31"

build_base_S <- function(){
  
  tmp <- list()
  
  for(i in 1:nrow(fitted_table)){
    tmp[[i]] <- 
      gen_country_basics(country = fitted_table$loc[i],
                         waning_nat = 52*7*3,
                         R0_assumed  = fitted_table$r[i],
                         date_start = as.character(ymd("2019-12-01") + fitted_table$t0[i]),
                         date_end = sim_end,
                         processes = burden_processes_az,
                         deterministic = TRUE) %>%
      change_VOC(., 
                 date_switch = c(fitted_table$t_intro_voc1[i], 
                                 fitted_table$t_intro_voc2[i], 
                                 fitted_table$t_intro_voc3[i]) %>% 
                   as.character,
                 rc_severity = c(fitted_table$rc_severity_1[i], 
                                 fitted_table$rc_severity_2[i],
                                 fitted_table$rc_severity_3[i]), 
                 rc_transmissibility = c(fitted_table$rc_transmissibility_1[i], 
                                         fitted_table$rc_transmissibility_2[i], 
                                         fitted_table$rc_transmissibility_3[i]), 
                 rc_ve = c(fitted_table$rc_ve_1[i], 
                           fitted_table$rc_ve_2[i], 
                           fitted_table$rc_ve_3[i]),
                 VE = ve_az) |> 
      cm_simulate() %>%
      .[["dynamics"]]
    
  }
  return(tmp)
}

base_S <- build_base_S()

base_S |> 
  map(filter, compartment == "S") |> 
  map(group_by, t, compartment, population) |> 
  map(summarise, value = sum(value)) |> 
  map(ungroup) |> 
  map(mutate, value_max = max(value)) |> bind_rows() -> non_S

non_S |> 
  left_join(fitted_table[,c("loc", "t0")],
            by = c("population" = "loc")) |> 
  mutate(date = ymd("2019-12-01") + t0 + t,
         date_char = as.character(date),
         sero = 1 - value/value_max)|> 
  filter(date_char %in% (seq(ymd("2021-01-01"),
                             ymd("2021-12-01"),
                             by = "month") |> as.character())) |> 
  mutate(iso3c = countrycode(population,
                             "country.name",
                             "iso3c")) -> non_S

ICER[["az_05"]][,c( "date_start",
                   "scenario",
                   "iso3c",
                   "ICER_scaled")] |> 
  mutate(date_start = as.character(date_start)) |> 
  left_join(non_S[,c("iso3c", "date_char", "sero")],
            by = c("iso3c",
                   "date_start" = "date_char")) |> 
  mutate(ICER_cat = case_when(ICER_scaled < 0.1 ~ 1,
                              ICER_scaled >= 0.1 & ICER_scaled < 0.3 ~ 2,
                              ICER_scaled >= 0.3 & ICER_scaled < 0.5 ~ 3,
                              ICER_scaled >= 0.5 & ICER_scaled < 1 ~ 4,
                              ICER_scaled >= 1 ~ 5
                              ),
         ICER_cat = factor(ICER_cat,
                           levels = 1:5,
                           labels = c("<0.1",
                                      "0.1-0.3",
                                      "0.3-0.5",
                                      "0.5-1",
                                      ">=1"))) -> non_S_combined

non_S_combined |> 
  mutate(ICER_cat = factor(ICER_cat)) |> 
  ggplot(aes(x = ICER_cat, y = sero)) +
  geom_boxplot() +
  theme_cowplot() +
  custom_theme +
  labs(x = "ICER as a proportion of GDP per capita",
       y = "Pop level seroprevalence at the beginning of vaccine roll-out")

ggsave("figs/R2R_R1/CE_by_sero.png",
       width = 10, height = 10)

lm(ICER_cat ~ sero, 
   data = non_S_combined) |> summary()

aov(ICER_cat~ sero,
    data = non_S_combined) |> summary()

require(MASS)
require(DescTools)
polr(ICER_cat ~ sero, data = non_S_combined) -> m3
PseudoR2(m3)
