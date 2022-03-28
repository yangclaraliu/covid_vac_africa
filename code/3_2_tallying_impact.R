# impact <- cov_econ_outcomes(
#   epi_deaths = input_fatal$az,
#   epi_cases = input_non_fatal$az,
#   econ_scens = econ_scens,
#   LT = UNLT,
#   POP = UNPOP,
#   GDPPC = GDPPC,
#   GNIPC = GNIPC
# )

input_fatal <- list()

res$pfizer$fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res$az$fatal %>% 
              mutate(Type = "az")) %>% 
  left_join(fitted_table[,c("loc", "iso3c")] %>% 
              rename(population = loc),
            by = "population"
  ) %>% 
  rename(epi_id = scenario_id,
         country = iso3c,
         age = group,
         deaths = value) %>% 
  mutate(age = factor(age,
                      levels = unique(res$pfizer$fatal$group),
                      labels = 1:16),
         age = as.numeric(age)) %>% 
  ungroup -> input_fatal[["all"]]
  
c(input_fatal, 
  input_fatal$all %>% 
    dplyr::select(epi_id, country, year, age, deaths, Type) %>% 
    group_by(Type) %>% group_split() %>% 
    setNames(c("az","pfizer")) %>% 
    map(select, -Type) %>% 
    map(data.table)) -> input_fatal

input_fatal$all %>% 
  dplyr::select(epi_id, country, year, age, novac) %>% 
  rename(deaths = novac) %>% 
  distinct() %>% 
  data.table() -> input_fatal[["novac"]]

input_fatal %>% map(dim)

# -> input_fatal

input_non_fatal <- list()

res$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res$az$non_fatal %>% 
              mutate(Type = "az")) %>% 
  filter(!grepl("_p_", name)) %>% 
  dplyr::select(-novac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  rename(epi_id = scenario_id,
         country = iso3c,
         icu = critical_i_all,
         non_icu = severe_i_all) %>% 
  ungroup()  -> input_non_fatal[["all"]]

c(input_non_fatal, 
  input_non_fatal$all %>% 
    dplyr::select(epi_id, country, year, cases, non_icu, icu, Type) %>% 
    group_by(Type) %>% group_split() %>% 
    setNames(c("az","pfizer")) %>% 
    map(select, -Type)%>% 
    map(data.table)) -> input_non_fatal

res$pfizer$non_fatal %>% 
  mutate(Type = "pfizer") %>% 
  bind_rows(res$az$non_fatal %>% 
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
  data.table() -> input_non_fatal[["novac"]]

impact <- list()
labs <- c("az", "pfizer", "novac")
for(i in 1:length(labs)){
  impact[[labs[i]]] <- cov_econ_outcomes(
    epi_deaths = input_fatal[[labs[i]]],
    epi_cases = input_non_fatal[[labs[i]]],
    econ_scens = econ_scens,
    LT = UNLT,
    POP = UNPOP,
    GDPPC = GDPPC,
    GNIPC = GNIPC
  )
}

write_rds(impact, "data/intermediate/impact.rds")



