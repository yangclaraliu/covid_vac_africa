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

ICER[["pf_05"]][,c( "date_start",
                   "scenario",
                   "iso3c",
                   "ICER_scaled",
                   "Type")] |> 
  mutate(Type = "pf") |> 
  bind_rows(ICER[["az_05"]][,c( "date_start",
                                "scenario",
                                "iso3c",
                                "ICER_scaled",
                                "Type")] |> 
              mutate(Type = "az") ) |> 
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

#### more regression ####
# population
# proportion above 60
# sero
# cost per vac
# GDPpc
cov_list <- c("pop","p_OA", "sero","vac_unit", "hc_expenditure", "`Income Group`")
f_cat <- paste("ICER_cat ~ ", cov_list) |> map(formula)
f_con <- paste("ICER_scaled ~ ", cov_list) |> map(formula)

pop |> 
  separate(age, into = c("age_LL", "age_UL")) |> 
  mutate(OA = if_else(age_LL >= 60, T, F)) |> 
  filter(OA == T) |> 
  group_by(iso3c) |> 
  summarise(pop_OA = sum(m+f)) -> pop_OA

non_S_combined |> 
  left_join(pop_bycountry, by = "iso3c") |> rename(pop = tot) |> 
  left_join(pop_OA, by = "iso3c") |> 
  left_join(ms_cov_all[,c("date_start","scenario","iso3c","vac_unit")] |> 
              mutate(date_start = as.character(date_start)),
            by = c("date_start","scenario","iso3c")) |> 
  left_join(GDPPC |> 
              rename(iso3c = country), 
            by = "iso3c") |> 
  mutate(p_OA = pop_OA*1000/pop) |> 
  left_join(cost_hc_expenditure_GHED[,c(1,5)], by = "iso3c") |> 
  left_join(group_income, by = "iso3c") -> regtab

non_S_combined |> 
  mutate(ICER_cat = factor(ICER_cat),
         Type = factor(Type,
                       levels = c("az","pf"),
                       labels = c("Viral vector vaccine",
                                  "mRNA vaccine"))) |> 
  ggplot(aes(x = ICER_cat, y = sero)) +
  geom_boxplot() +
  theme_bw() +
  custom_theme +
  labs(x = "ICER as a proportion of GDP per capita",
       y = "Pop level seroprevalence at the beginning of vaccine roll-out") +
  facet_wrap(~Type, ncol = 1) 

ggsave("figs/R2R_R1/CE_by_sero.png",
       width = 10, height = 18)

aov(sero ~ ICER_cat, data = non_S_combined |> 
      filter(Type == "az")) -> m1
summary(m1)

aov(sero ~ ICER_cat, data = non_S_combined |> 
      filter(Type == "pf")) -> m2
summary(m2)

require(MASS)
require(DescTools)
polr(ICER_cat ~ sero, data = non_S_combined |> filter(Type == "az")) -> m3
PseudoR2(m3)

polr(ICER_cat ~ sero, data = non_S_combined |> filter(Type == "pf")) -> m4
PseudoR2(m4)*100

lm(ICER_scaled ~ sero, data = non_S_combined |> filter(Type == "az")) -> m5
lm(ICER_scaled ~ sero, data = non_S_combined |> filter(Type == "pf")) -> m6
summary(m5)
summary(m6)

# test all covariates
cov_list
m_cat_az <- m_cat_pf <- list()
m_cat_az[[1]] <- polr("ICER_cat ~ pop", data = regtab |> filter(Type == "az"))
m_cat_az[[2]] <- polr("ICER_cat ~ p_OA", data = regtab |> filter(Type == "az"))
m_cat_az[[3]] <- polr("ICER_cat ~ sero", data = regtab |> filter(Type == "az"))
m_cat_az[[4]] <- polr("ICER_cat ~ vac_unit", data = regtab |> filter(Type == "az"))
m_cat_az[[5]] <- polr("ICER_cat ~ hc_expenditure", data = regtab |> filter(Type == "az"))
m_cat_az[[6]] <- polr("ICER_cat ~ `Income Group`", data = regtab |> filter(Type == "az"))

m_cat_pf[[1]] <- polr("ICER_cat ~ pop", data = regtab |> filter(Type == "pf"))
m_cat_pf[[2]] <- polr("ICER_cat ~ p_OA", data = regtab |> filter(Type == "pf"))
m_cat_pf[[3]] <- polr("ICER_cat ~ sero", data = regtab |> filter(Type == "pf"))
m_cat_pf[[4]] <- polr("ICER_cat ~ vac_unit", data = regtab |> filter(Type == "pf")) 
m_cat_pf[[5]] <- polr("ICER_cat ~ hc_expenditure", data = regtab |> filter(Type == "pf")) 
m_cat_pf[[6]] <- polr("ICER_cat ~ `Income Group`", data = regtab |> filter(Type == "pf"))

m_cat_az |> map(PseudoR2) |> unlist() -> RS_cat_az
m_cat_pf |> map(PseudoR2) |> unlist() -> RS_cat_pf

# 
m_con_az <- map(f_con, lm, data = regtab |> filter(Type == "az"))
m_con_pf <- map(f_con, lm, data = regtab |> filter(Type == "pf"))

m_con_az |> map(summary) |> lapply("[[", "r.squared") |> unlist() -> RS_con_az
m_con_pf |> map(summary) |> lapply("[[", "r.squared") |> unlist() -> RS_con_pf

data.frame(RS_con_az = RS_con_az,
           RS_con_pf = RS_con_pf,
           RS_cat_az = RS_cat_az,
           RS_cat_pf = RS_cat_pf) |> 
  rowMeans()

m_con_az |> map(summary) |> lapply("[[", "coefficients") 

