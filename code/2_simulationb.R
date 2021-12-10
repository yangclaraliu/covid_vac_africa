params_table <- CJ(cn = members_complete$name_internal, # country name
                   R0 = seq(1.5,5,0.5),
                   date_start = seq(ymd("2020-01-15"), # infection introduction
                                    ymd("2020-05-15"),
                                    7)) %>% 
  rownames_to_column(var = "index") %>% 
  mutate(cn = as.character(cn))
VOC_date <- "2021-06-15"

# params_list <- list()

# for(i in 1:nrow(params_table)){
#   params_list[[i]] <- gen_country_basics(country = params_table$cn[i],
#                                          waning_nat = 52*7*3,
#                                          R0_assumed = params_table$R0[i],
#                                          date_start = params_table$date_start[i],
#                                          processes = burden_processes,
#                                          deterministic = T) %>%
#     update_vac_char(.,
#                     ve_i   = ve$ve_i_o[1],  # infection blocking VE post 1 dose
#                     v2e_i  = ve$ve_i_o[2],  # infection blocking VE post 2 doses
#                     ve_d   = ve$ve_d[1],    # clinical fraction among breakthrough post 1 dose
#                     v2e_d  = ve$ve_d[2],    # clinical fraction among breakthrough post 2 doses
#                     wv = 1/360) %>% # 1/ waning duration
#     change_VOC(.,
#                date_switch = "2021-06-15",
#                rc_severity = 1.5,
#                rc_transmissibility = 1.5,
#                rc_ve = 0.4)
#   if(i%%100 == 0) print(i)
# }

params_table %>%
  filter(R0 == 1.5,
         date_start == "2020-01-15") %>%
  pull(index) %>% as.numeric -> index_tmp

params_table %>%
  filter(R0 == 1.5,
         date_start == "2020-01-15") %>%
  dplyr::select(index, cn) -> cn_rf

# # params_allocation <- list()
# # for(i in 1:length(index_tmp)){
# for(i in c(53)){
#   params_allocation[[i]] <- list()
#   # for(j in 1:max(as.numeric(ROS$ROS))){
#   for(j in 4){
#       vac_policy(params_list[[index_tmp[i]]],
#                  # these two parameters define the supply conditions
#                  milestone_date = ROS[ROS$ROS==j,]$milestone_date,
#                  milestone_cov = ROS[ROS$ROS==j,]$coverage,
#                  # prioritisation, assume 60+  all prioritised
#                  priority = c(NA, NA, NA, NA,
#                               2,  2,  2,  2,
#                               2,  2,  2,  2,
#                               1,  1,  1,  1),
#                  # maximum feasible uptakes
#                  cov_max = c(rep(0,4),
#                              rep(0.7, 8),
#                              rep(0.9, 4)),
#                  supply_delay = 24, # unit = weeks
#                  dose_interval = 4) ->   params_allocation[[i]][[j]] 
#   }
#   print(paste0(round(i*100/length(index_tmp),2),"%"))
# }
# 
# piece4 <- params_allocation
# piece <- qs::qread("data/intermediate/params_allocation.qs")
# for(i in c(15, 16, 27, 29, 35, 46, 53)){
#   piece[[i]][[4]] <- piece4[[i]][[4]]
# }
# # qs::qsave(piece, "data/intermediateparams_allocation_v2.qs")
# # for(i in 1:nrow(cn_rf)){
# for(i in c(53)){
#   lapply(1:4, function(x){
#     piece[[i]][[x]]$scenarios %>%
#       lapply(., "[[", "daily_vac_scenarios")
#   }) %>%
#     lapply(., "[[", 1) %>%
#     map(filter, date >= "2021-02-28") %>%
#     map(mutate_at, vars(starts_with("Y", ignore.case = F)), cumsum) %>%
#     map(pivot_longer, starts_with("Y", ignore.case = F)) %>%
#     map(separate, name, into = c("ag", "dose")) %>%
#     bind_rows(.id = "ROS") %>%
#     group_by(ROS, date, dose) %>%
#     summarise(value = sum(value), .groups = "drop") %>%
#     mutate(ROS = paste0("ROS", ROS),
#            iso3c = countrycode(cn_rf$cn[i], "country.name", "iso3c")) %>%
#     left_join(vac_denom[,2:3], by = "iso3c") %>%
#     mutate(r = value/(tot*1000)) %>%
#     ggplot(., aes(x = date, y = r, color = dose, group = dose)) +
#     geom_line() +
#     facet_wrap(~ROS) +
#     theme_cowplot() +
#     theme(axis.text.x = element_text(angle = 45))  -> p
#   ggsave(plot = p, filename = paste0("figs/intermediate/vaccine_allocation/",
#                                      cn_rf$cn[i],".png"))
# }
# 
# 

#### sanity check #### 
params_list <- qread("data/intermediate/params_list.qs")
params_allocation <- qread("data/intermediate/params_allocation_v2.qs")


neg <- list()
for(i in 1:55){
  lapply(params_allocation[[i]], "[[", "scenarios") %>% 
    lapply(., "[[", 1) %>% 
    lapply(., "[[", "pending") %>% 
    bind_rows(.id = "ROS") %>% 
    dplyr::select(ROS, t_dose1, t_dose2, starts_with("Y", ignore.case = T)) %>% 
    pivot_longer(starts_with("Y", ignore.case = T)) %>% 
    filter(value < 0) -> neg[[i]]
}

res <- ts <- list()
# for(i in 1:10){
for(i in 1:nrow(params_table)){
  tmp <- list()
  cn_tmp <- params_table$cn[i]
  tmp[["no_vac"]] <- 
    tmp[["ROS1"]] <- 
    tmp[["ROS2"]] <- 
    tmp[["ROS3"]] <- 
    tmp[["ROS4"]] <- 
    params_list[[i]]
  date_start <- tmp$no_vac$date0
  
  tmp[["ROS1"]]$schedule[["v"]] <- 
    params_allocation[[which(cn_rf$cn == cn_tmp)]][[1]]$res[[1]]$schedule[["v"]]
  tmp[["ROS1"]]$schedule[["v2"]] <- 
    params_allocation[[which(cn_rf$cn == cn_tmp)]][[1]]$res[[1]]$schedule[["v2"]]
  
  
  tmp[["ROS2"]]$schedule[["v"]] <- 
    params_allocation[[which(cn_rf$cn == cn_tmp)]][[2]]$res[[1]]$schedule[["v"]]
  tmp[["ROS2"]]$schedule[["v2"]] <- 
    params_allocation[[which(cn_rf$cn == cn_tmp)]][[2]]$res[[1]]$schedule[["v2"]]
  
  tmp[["ROS3"]]$schedule[["v"]] <- 
    params_allocation[[which(cn_rf$cn == cn_tmp)]][[3]]$res[[1]]$schedule[["v"]]
  tmp[["ROS3"]]$schedule[["v2"]] <- 
    params_allocation[[which(cn_rf$cn == cn_tmp)]][[3]]$res[[1]]$schedule[["v2"]]
  
  tmp[["ROS4"]]$schedule[["v"]] <- 
    params_allocation[[which(cn_rf$cn == cn_tmp)]][[4]]$res[[1]]$schedule[["v"]]
  tmp[["ROS4"]]$schedule[["v2"]] <- 
    params_allocation[[which(cn_rf$cn == cn_tmp)]][[4]]$res[[1]]$schedule[["v2"]]
  
  tmp %>% 
    map(cm_simulate) %>% 
    lapply(., "[[", "dynamics") -> dyna
  
  dyna %>% 
    bind_rows(.id = "ROS") %>% 
    filter(grepl("death", compartment)) %>% 
    mutate(date = t +   ymd(params_table[i,]$date_start)) %>% 
    filter(!(date <= VOC_date & compartment == "death_voc_o"),
           !(date > VOC_date & compartment == "death_o")) %>% 
    group_by(ROS, date, population) %>% 
    summarise(value = sum(value)) %>%
    pivot_wider(names_from = ROS, values_from = value) %>% 
    arrange(date) %>%
    ungroup %>% 
    mutate_at(vars(c("no_vac", paste0("ROS",1:4))), cumsum) %>% 
    pivot_longer(cols = (c("no_vac", paste0("ROS",1:4)))) -> ts[[i]]

  
  dyna %<>% 
    bind_rows(.id = "ROS") %>% 
    mutate(date = ymd(date_start) + t,
           VOC = if_else(date >= VOC_date, T, F)) %>% 
    filter(grepl("hosp|death|cases", compartment),
           date >= "2021-03-01") %>% 
    group_by(ROS, population, compartment, VOC) %>% 
    summarise(value = sum(value), .groups = "drop")
  
  dyna %>% 
    filter(!(compartment %in% c("hosp_p", "hosp_voc_p", "cases_reported")),
           !(compartment == "death_o" & VOC == TRUE),
           !(compartment == "death_voc_o" & VOC == FALSE),
           !(compartment == "hosp_i" & VOC == TRUE),
           !(compartment == "hosp_voc_i" & VOC == FALSE)) %>% 
    separate(compartment, sep = "_", into = c("compartment", "dep")) %>% 
    dplyr::select(-dep) %>% 
    group_by(ROS, population, compartment) %>% 
    summarise(value = sum(value), .groups = "drop") %>%
    pivot_wider(names_from = ROS,
                values_from = value) %>% 
    # mutate(r_diff1 = 1 - ROS1/no_vac,
    #        r_diff2 = 1 - ROS2/no_vac,
    #        r_diff3 = 1 - ROS3/no_vac,
    #        a_diff1 = no_vac - ROS1,
    #        a_diff2 = no_vac - ROS2,
    #        a_diff3 = no_vac - ROS3) %>% 
    # dplyr::select(-starts_with("ROS"),
    #               -no_vac) %>% 
    left_join(params_table[i,cn:date_start], 
              by = c("population" = "cn")) -> res[[i]]
  
  require(qs)
  
  if(i%%10 == 0) {
    print(i)
    qs::qsave(ts, "data/intermediate/advo_ts.qs")
    write_rds(res, "data/intermediate/simulationb_res.rds")
  }
}

ts
res
