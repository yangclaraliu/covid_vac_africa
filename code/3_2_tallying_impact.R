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

get_fatal <- function(tmp){
  m <- list()
  
  tmp$pfizer$fatal %>% 
    mutate(Type = "pfizer") %>% 
    bind_rows(tmp$az$fatal %>% 
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
                        levels = unique(tmp$pfizer$fatal$group),
                        labels = 1:16),
           age = as.numeric(age)) %>% 
    ungroup -> m[["all"]]
  
  c(m, 
    m$all %>% 
      dplyr::select(epi_id, country, year, age, deaths, Type) %>% 
      group_by(Type) %>% group_split() %>% 
      setNames(c("az","pfizer")) %>% 
      map(select, -Type) %>% 
      map(data.table)) -> m
  
  m$all %>% 
    dplyr::select(epi_id, country, year, age, novac) %>% 
    rename(deaths = novac) %>% 
    distinct() %>% 
    data.table() -> m[["novac"]]
  
  return(m)
}

get_non_fatal <- function(tmp){
  m <- list()
  
  tmp$pfizer$non_fatal %>% 
    mutate(Type = "pfizer") %>% 
    bind_rows(tmp$az$non_fatal %>% 
                mutate(Type = "az")) %>% 
    filter(!grepl("_p_", name)) %>% 
    dplyr::select(-novac) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    rename(epi_id = scenario_id,
           country = iso3c,
           icu = critical_i_all,
           non_icu = severe_i_all) %>% 
    ungroup()  -> m[["all"]]
  
  c(m, 
    m$all %>% 
      dplyr::select(epi_id, country, year, cases, non_icu, icu, Type) %>% 
      group_by(Type) %>% group_split() %>% 
      setNames(c("az","pfizer")) %>% 
      map(select, -Type)%>% 
      map(data.table)) -> m
  
  tmp$pfizer$non_fatal %>% 
    mutate(Type = "pfizer") %>% 
    bind_rows(tmp$az$non_fatal %>% 
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
    data.table() -> m[["novac"]]
  
  return(m)
}

f <- list()
f[["bc"]] <- get_fatal(res)
f[["ext"]] <- get_fatal(res_ext)
f[["low"]] <- get_fatal(res_low)

nf <- list()
nf[["bc"]] <- get_non_fatal(res)
nf[["ext"]] <- get_non_fatal(res_ext)
nf[["low"]] <- get_non_fatal(res_low)

impact <- list()
labs_vac <- c("az", "pfizer", "novac")
labs_ver <- c("bc", "ext","low")

for(j in 1:length(labs_ver)){
# for(j in 1){
  impact[[labs_ver[j]]] <- list()
  # for(i in 1){
  for(i in 1:length(labs_vac)){
    impact[[labs_ver[j]]][[labs_vac[i]]] <-
      cov_econ_outcomes(
      epi_deaths = f[[labs_ver[j]]][[labs_vac[i]]],
      epi_cases = nf[[labs_ver[j]]][[labs_vac[i]]],
      econ_scens = econ_scens,
      LT = UNLT,
      POP = UNPOP,
      GDPPC = GDPPC,
      GNIPC = GNIPC
    )
  }
}

write_rds(impact, "data/intermediate/impact_combined.rds")



