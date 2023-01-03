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

lm(formula = (ICER_scaled) ~ scale(pop) + scale(p_OA) + scale(sero) + scale(vac_unit) + `Income Group` + scale(hc_expenditure) + Type,
   data = regtab) -> model_all
lm(formula = (ICER_scaled) ~ scale(pop) + scale(p_OA) + scale(sero) + scale(vac_unit) + `Income Group` + scale(hc_expenditure) + Type + date_start + scenario,
   data = regtab) -> model_all_adj
model_summary  <- summary(model_all)
model_summary_adj  <- summary(model_all_adj)
vif(model_all)
vif(model_all_adj)
model_summary$coefficients |> 
  data.frame() |> 
  mutate(LL = Estimate - 1.96*`Std..Error`,
         UL = Estimate + 1.96*`Std..Error`) %>%
  rownames_to_column(var = "var") |> 
  dplyr::filter(!grepl("date_start|scenario|Intercept|pf", var)) |> 
  arrange(desc(Estimate)) |> 
  mutate(var = factor(var, 
                      levels = var,
                      labels = c("Proportion of non-susceptibles",
                                 "Domestic healthcare expenditure",
                                 "Vaccine delivery unit cost",
                                 "Population size",
                                 "Proportion of population 60+",
                                 "Lower-middle income countries\n(compared to low-income countries)",
                                 "Upper-middle income countries\n(compared to low-income countries)"))) |> 
  ggplot(aes(y = var, x = Estimate)) +
  geom_point() +
  geom_segment(aes(y = var, yend = var, x = LL, xend = UL)) +
  geom_vline(xintercept = c(0.25,0,-0.25), linetype = c(2,1,2)) +
  theme_cowplot() +
  labs(y = "Variable",
       x = "Effect sizes") +
  scale_x_continuous(breaks = c(0.25,0,-0.25)) -> p_save

ggsave("figs/R2R_R2/reg_res.png",
       plot = p_save,
       width = 10,
       height = 7)

model_summary_adj$coefficients |> 
  data.frame() |> 
  mutate(LL = Estimate - 1.96*`Std..Error`,
         UL = Estimate + 1.96*`Std..Error`) %>%
  rownames_to_column(var = "var") |> 
  dplyr::filter(!grepl("date_start|scenario|Intercept|pf", var)) |> 
  arrange(desc(Estimate)) |> 
  mutate(var = factor(var, levels = var)) |> 
  ggplot(aes(y = var, x = Estimate)) +
  geom_point() +
  geom_segment(aes(y = var, yend = var, x = LL, xend = UL)) +
  geom_vline(xintercept = c(0.25,0,-0.25), linetype = c(2,1,2)) +
  theme_cowplot() +
  labs(y = "Variable",
       x = "Effect sizes") +
  scale_x_continuous(breaks = c(0.25,0,-0.25)) 
                     # m_con_az |> map(summary) |> lapply("[[", "r.squared") |> unlist() -> RS_con_az
# m_con_pf |> map(summary) |> lapply("[[", "r.squared") |> unlist() -> RS_con_pf

# data.frame(RS_con_az = RS_con_az,
#            RS_con_pf = RS_con_pf,
#            RS_cat_az = RS_cat_az,
#            RS_cat_pf = RS_cat_pf) |> 
#   rowMeans()

# coef_tab <- list()
# 
# m_con_az |> 
# # m_con |> 
#   map(summary) |> 
#   lapply("[[", "coefficients") |> 
#   setNames(cov_list) |> 
#   map(data.frame) |> 
#   map(rownames_to_column, var = "variable") |> 
#   bind_rows(.id = "model") |> 
#   filter(variable != "(Intercept)")  -> coef_tab[["az"]]
# 
# m_con_pf |> 
#   # m_con |> 
#   map(summary) |> 
#   lapply("[[", "coefficients") |> 
#   setNames(cov_list) |> 
#   map(data.frame) |> 
#   map(rownames_to_column, var = "variable") |> 
#   bind_rows(.id = "model") |> 
#   filter(variable != "(Intercept)")  -> coef_tab[["pf"]]
# 
# (m_con_az |> 
#     map(summary) |> 
#     lapply("[[", "adj.r.squared")) |> 
#   unlist() |> 
#   enframe(name = "model",
#           value = "adj.r.squared") |> 
#   mutate(model = cov_list) |> 
#   right_join(coef_tab$az, by = "model")  -> coef_tab[["az"]]
# 
# (m_con_pf |> 
#     map(summary) |> 
#     lapply("[[", "adj.r.squared")) |> 
#   unlist() |> 
#   enframe(name = "model",
#           value = "adj.r.squared") |> 
#   mutate(model = cov_list) |> 
#   right_join(coef_tab$pf, by = "model")  -> coef_tab[["pf"]]
