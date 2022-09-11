merge_novac_grouped <- function(tmp){
  require(magrittr)
    tmp$pfizer$non_fatal %<>%
      left_join(res_novac_grouped$pfizer[[1]] %>%
                  rename(novac = value),
                by = c("name", "population", "group","year"))
    
    tmp$pfizer$fatal %<>%
      left_join(res_novac_grouped$pfizer[[2]],
                by = c("name", "population", "group", "year"))
    
    
    tmp$az$non_fatal %<>%
      left_join(res_novac_grouped$az[[1]] %>%
                  rename(novac = value),
                by = c("name", "population", "group","year"))
    
    tmp$az$fatal %<>%
      left_join(res_novac_grouped$az[[2]],
                by = c("name", "population", "group", "year"))
  return(tmp)
}

# load data
params_grid_az_70_grouped <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_az_70_grouped.rds")
params_grid_pfizer_70_grouped <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/params_grid_pfizer_70_grouped.rds")
res_novac_grouped <- readRDS("~/GitHub/covid_vac_africa/data/intermediate/res_novac_grouped.rds")

# organise results
res_grouped <- organise_outcomes(tmp_pfizer = params_grid_pfizer_70_grouped,
                                 tmp_az = params_grid_az_70_grouped)
res_grouped_merged <- merge_novac_grouped(res_grouped)

# write tables
input_fatal_grouped <- list()

res_grouped_merged$pfizer$fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_grouped_merged$az$fatal %>% 
              mutate(Type = "az")) %>% 
  left_join(fitted_table[,c("loc", "iso3c")] %>% 
              rename(population = loc),
            by = c("population", "iso3c")
  ) %>% 
  rename(epi_id = scenario_id,
         country = iso3c,
         age = group,
         deaths = value) %>% 
  mutate(age = factor(age,
                      levels = unique(res_grouped_merged$pfizer$fatal$group),
                      labels = 1:16),
         age = as.numeric(age)) %>% 
  ungroup -> input_fatal_grouped[["all"]]

c(input_fatal_grouped, 
  input_fatal_grouped$all %>% 
    dplyr::select(epi_id, country, year, age, deaths, Type) %>% 
    group_by(Type) %>% group_split() %>% 
    setNames(c("az","pfizer")) %>% 
    map(dplyr::select, -Type) %>% 
    map(data.table)) -> input_fatal_grouped

input_fatal_grouped$all %>% 
  dplyr::select(epi_id, country, year, age, novac) %>% 
  rename(deaths = novac) %>% 
  distinct() %>% 
  data.table() -> input_fatal_grouped[["novac"]]

input_non_fatal_grouped <- list()

res_grouped_merged$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_grouped_merged$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(!grepl("_p_", name)) %>% 
  dplyr::select(-novac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  rename(epi_id = scenario_id,
         country = iso3c,
         icu = critical_i_all,
         non_icu = severe_i_all) %>% 
  ungroup()  -> input_non_fatal_grouped[["all"]]

c(input_non_fatal_grouped, 
  input_non_fatal_grouped$all %>% 
    dplyr::select(epi_id, country, group, year, cases, non_icu, icu, Type) %>% 
    group_by(Type) %>% group_split() %>% 
    setNames(c("az","pfizer")) %>% 
    map(dplyr::select, -Type)%>% 
    map(data.table)) -> input_non_fatal_grouped

res_grouped_merged$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res_grouped_merged$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(!grepl("_p_", name)) %>% 
  dplyr::select(-value, -Type) %>%
  distinct() %>% 
  pivot_wider(names_from = name, values_from = novac) %>% 
  rename(epi_id = scenario_id,
         country = iso3c,
         icu = critical_i_all,
         non_icu = severe_i_all) %>% 
  ungroup() %>% 
  data.table() -> input_non_fatal_grouped[["novac"]]

input_non_fatal_grouped$novac |> 
  dplyr::select(colnames(input_non_fatal_grouped$az)) -> input_non_fatal_grouped$novac

# 
#testing 
# epi_deaths = input_fatal_grouped$az
# epi_cases = input_non_fatal_grouped$az
# econ_scens = econ_scens[1,]
# LT = UNLT
# POP = UNPOP
# GDPPC = GDPPC
# GNIPC = GNIPC

# pre calculate life expectancy as now we only care about one type of economic
# scenario
dLE <- vector("list", length = nrow(econ_scens))
for (s in 1:nrow(econ_scens)){
  # calculate discount life expectancy for given discount rate, smr and country
  dLE[[s]] <- cov_dLE(
    r   = econ_scens[[s,"discount_rate"]],
    smr = econ_scens[[s,"smr"]],
    selectCountries = unique(epi_deaths[,country]),
    LT = UNLT,
    POP = UNPOP
  )
  dLE[[s]][, econ_id := s]
}
dLE <- rbindlist(dLE)

# update_functions
cov_econ_outcomes_grouped <- function(
    epi_deaths,   # data table of age-specific deaths
    epi_cases,    # data table of cases, non_icu and icu admission (not age-specific)
    econ_scens,   # econ scenarios specifying discount rate, smr
    LT,           # UNWPP life tables
    POP,          # UNWPP population estimates
    GDPPC,        # World Bank country GDP per capita 2020
    GNIPC         # World Bank country GNI per capita 2020
){
  
  require(data.table)
  
  # merge deaths and vsl data
  vsl <- cov_VSL(GNIPC = GNIPC)
  ylls <- vsl[  # merge deaths and vsl data
    epi_deaths, 
    on = .(country == country)
  ]
  
  # merge gdp per capita data
  ylls <- GDPPC[
    ylls,
    on = .(country == country)
  ]
  
  # merge dLE data and calculated ylls, vsl and hc 
  first_year <- 2021 # min(epi_deaths$year) # reference year for discounting
  ylls <- dLE[   
    ylls, 
    on = .(country = country, AgeBand = age), 
    allow.cartesian = TRUE
  ][               
    , 
    .(
      ylls = sum(deaths * d_LEx * 
                   1 / (1 + econ_scens[[s,"discount_rate"]])^(year - first_year)
      ),
      vsl = sum(deaths * vsl),
      human_capital = sum(d_LEx * GDPPC_2020_USD),
      GDPPC_2020_USD = mean(GDPPC_2020_USD)
    ),
    by = .(epi_id, econ_id, country, AgeBand) # collapse age bands
  ] 
  
  # ylds based on number of cases / hospitalizations
  unit_ylds <- cov_unit_ylds()
  first_year <- 2021
  ylds <- epi_cases[
    ,
    as.list(econ_scens[,c("econ_id","discount_rate")]), #  combine with different econ scenarios
    by=epi_cases
  ][
    ,
    .(
      ylds = sum(
        (
          (cases     * unit_ylds$per_case) +
            (non_icu  * unit_ylds$per_non_icu_case) +
            (icu      * unit_ylds$per_icu_case)
        ) * 1 / (1 + discount_rate)^(year - first_year) # discounting
      )
    ),
    by = .(epi_id, econ_id, country, group)
  ]
  
  ylls |> 
    mutate(group = factor(AgeBand,
                          levels = 1:16,
                          labels = unique(ylds$group))) |> 
    dplyr::select(-AgeBand) -> ylls
    
  
  # merge ylls with ylds and calculate dalys
  out <- ylds[
    ylls, 
    on = .(epi_id, econ_id, country, group)
  ][, dalys := ylls + ylds]
  
  return(out)
}

cov_econ_outcomes_grouped(
  epi_deaths = input_fatal_grouped$az,
  epi_cases = input_non_fatal_grouped$az,
  econ_scens = econ_scens[1,],
  LT = UNLT,
  POP = UNPOP,
  GDPPC = GDPPC,
  GNIPC = GNIPC) -> out_az_grouped

cov_econ_outcomes_grouped(
  epi_deaths = input_fatal_grouped$pfizer,
  epi_cases = input_non_fatal_grouped$pfizer,
  econ_scens = econ_scens[1,],
  LT = UNLT,
  POP = UNPOP,
  GDPPC = GDPPC,
  GNIPC = GNIPC) -> out_pfizer_grouped

cov_econ_outcomes_grouped(
  epi_deaths = input_fatal_grouped$novac,
  epi_cases = input_non_fatal_grouped$novac,
  econ_scens = econ_scens[1,],
  LT = UNLT,
  POP = UNPOP,
  GDPPC = GDPPC,
  GNIPC = GNIPC) -> out_novac_grouped

# when medium is better than fast
pop_bycountry <- pop |> 
  group_by(iso3c) |> 
  summarise(tot = sum(m,f)*1000) |> 
  filter(iso3c %in% fitted_table$iso3c)

ICER$az_03 |> 
  filter(econ_id == 1) |> 
  mutate(Type = "az") |> 
  bind_rows(ICER$pf_03 |> 
              filter(econ_id == 1) |> 
              mutate(Type = "pf")) |> 
  dplyr::select(date_start, ICER_scaled, iso3c, scenario, Type) |> 
  left_join(pop_bycountry, by = "iso3c") |> 
  mutate(ICER_cat = case_when(ICER_scaled < 0.1 ~ 1,
                              ICER_scaled >= 0.1 & ICER_scaled < 0.3 ~ 2,
                              ICER_scaled >= 0.3 & ICER_scaled < 0.5 ~ 3,
                              ICER_scaled >= 0.5 & ICER_scaled < 1 ~ 4,
                              ICER_scaled >= 1 ~ 5)) |> 
  # select(-ICER_scaled) |> 
  dplyr::select(-ICER_cat) |> 
  pivot_wider(names_from = scenario, values_from = ICER_scaled) |> 
  mutate(check = case_when(medium < fast ~ "1",
                           medium == fast ~ "2",
                           medium > fast ~ "3")) -> bar_met
# plot results
out_az_grouped |> 
  left_join(ms_scenarios |> 
              rownames_to_column(var = "epi_id"), 
            by = "epi_id") |> 
  mutate(Type = "az") |> 
  bind_rows(out_pfizer_grouped |> 
              left_join(ms_scenarios |> 
                          rownames_to_column(var = "epi_id"), 
                        by = "epi_id") |> 
              mutate(Type = "pf")) |> 
  mutate(date_start = ymd(date_start),
         older = if_else(group %in% c("60-64",
                                      "65-69",
                                      "70-74",
                                      "75+"),
                         T, F)) |> 
  group_by(epi_id, country, scenario, date_start, older, Type) |> 
  summarise(dalys = sum(dalys)) |> 
  group_by(epi_id, country, scenario, date_start, Type) |> 
  mutate(dalys_tot = sum(dalys),
         dalys_prop = dalys/dalys_tot,
         scenario = factor(scenario,
                           levels = c("slow", "medium", "fast"))) |> 
  rename(iso3c = country) |> 
  right_join(bar_met[,c("date_start", "iso3c", "check", "Type")], 
             by = c("date_start", "iso3c", "Type")) |> 
  filter(older == T, scenario != "slow") |> 
  ungroup() |> 
  dplyr::select(-dalys, -dalys_tot, -epi_id) |> 
  pivot_wider(names_from = scenario, values_from = dalys_prop) |> 
  data.table() -> data_test

t1 <- t.test(data_test[check == "1" & Type == "az", ]$fast, 
             data_test[check == "1" & Type == "az", ]$medium, 
             paired = T, 
             alternative = "two.sided")

t2 <- t.test(data_test[check == "3" & Type == "az", ]$fast, 
             data_test[check == "3" & Type == "az", ]$medium, 
             paired = T, 
             alternative = "two.sided")

t3 <- t.test(data_test[check == "1" & Type == "pf", ]$fast, 
             data_test[check == "1" & Type == "pf", ]$medium, 
             paired = T, 
             alternative = "two.sided")

t4 <- t.test(data_test[check == "3" & Type == "pf", ]$fast, 
             data_test[check == "3" & Type == "pf", ]$medium, 
             paired = T, 
             alternative = "two.sided")


data.frame(mean = c(t1$estimate,
                    t2$estimate,
                    t3$estimate,
                    t4$estimate), 
           LL = c(t1$conf.int[1],
                  t2$conf.int[1],
                  t3$conf.int[1],
                  t4$conf.int[1]),
           UL = c(t1$conf.int[2],
                  t2$conf.int[2],
                  t3$conf.int[2],
                  t4$conf.int[2]),
           lab = c("ICER_medium < ICER_fast", 
                   "ICER_medium > ICER_fast"),
           Type = c("Viral vector vaccine",
                    "Viral vector vaccine",
                    "mRNA vaccine",
                    "mRNA vaccine")) |> 
  ggplot() +
  geom_point(aes(x = lab, y = mean), size = 3) +
  geom_segment(aes(x = lab, xend = lab,
                   y = LL, yend = UL), size = 1.5) +
  theme_bw() +
  custom_theme +
  labs(x = "",
       y = expression("paDALY"["fast, 60+"] - "paDALY"["medium, 60+"])) +
  scale_x_discrete(labels = parse(text = c("ICER[medium] < ICER[fast]",
                                           "ICER[medium] > ICER[fast]")))+
  facet_wrap(~Type, ncol = 1, scales = "free")

ggsave("figs/R2R_R1/ICER_grouped.png", width = 8, height = 12)  
