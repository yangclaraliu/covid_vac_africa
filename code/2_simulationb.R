params_table <- CJ(cn = members$name, # country name
                   R0 = seq(1.5,5,0.5),
                   date_start = seq(ymd("2020-01-15"), # infection introduction
                                    ymd("2020-05-15"),
                                    7)) %>% 
  rownames_to_column(var = "index")


params_list <- list()
VOC_date <- "2021-06-15"
pb <- progress_bar$new(total = nrow(params_table))
for(i in 1:nrow(params_table)){
  pb$tick()
  params_list[[i]] <- gen_country_basics(country = params_table$cn[i],
                                         waning_nat = 52*7*3,
                                         R0_assumed = params_table$R0[i],
                                         date_start = params_table$date_start[i],
                                         processes = burden_processes,
                                         deterministic = T)%>%
    update_vac_char(.,
                    ve_i   = ve$ve_i_o[1],  # infection blocking VE post 1 dose
                    v2e_i  = ve$ve_i_o[2],  # infection blocking VE post 2 doses
                    ve_d   = ve$ve_d[1],    # clinical fraction among breakthrough post 1 dose
                    v2e_d  = ve$ve_d[2],    # clinical fraction among breakthrough post 2 doses
                    wv = 1/360) %>% # 1/ waning duration
    change_VOC(.,
               date_switch = "2021-06-15",
               rc_severity = 1.5,
               rc_transmissibility = 1.5,
               rc_ve = 0.4)
}

params_table %>% 
  filter(R0 == 1.5,
         date_start == "2020-01-15") %>% 
  pull(index) %>% as.numeric -> index_tmp

params_table %>% 
  filter(R0 == 1.5,
         date_start == "2020-01-15") %>% 
  dplyr::select(index, cn) -> cn_rf

# params_allocation <- list()
for(i in 36:length(index_tmp)){
  params_allocation[[i]] <- list()
  for(j in 1:max(as.numeric(ROS$ROS))){
    vac_policy(params_list[[i]],
               # these two parameters define the supply conditions
               milestone_date = ROS[ROS$ROS==j,]$milestone_date,
               milestone_cov = ROS[ROS$ROS==j,]$coverage,
               # prioritisation, assume 60+  all prioritised
               priority = c(NA, NA, NA, NA,
                            2,  2,  2,  2,
                            2,  2,  2,  2,
                            1,  1,  1,  1),
               # maximum feasible uptakes
               cov_max = c(rep(0,4),
                           rep(0.7, 8),
                           rep(0.9, 4)),
               supply_delay = 24, # unit = weeks
               dose_interval = 4) ->   params_allocation[[i]][[j]] 
  }
  print(paste0(round(i*100/length(index_tmp),2),"%"))
}

res <- list()
for(i in 1:nrow(params_table)){
  tmp <- list()
  cn_tmp <- params_table$cn[i]
  tmp[["no_vac"]] <- 
    tmp[["ROS1"]] <- 
    tmp[["ROS2"]] <- 
    tmp[["ROS3"]] <- 
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
  
  tmp %>% 
    map(cm_simulate) %>% 
    lapply(., "[[", "dynamics") -> dyna
  
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
    mutate(r_diff1 = 1 - ROS1/no_vac,
           r_diff2 = 1 - ROS2/no_vac,
           r_diff3 = 1 - ROS3/no_vac,
           a_diff1 = no_vac - ROS1,
           a_diff2 = no_vac - ROS2,
           a_diff3 = no_vac - ROS3) %>% 
    dplyr::select(-starts_with("ROS"),
                  -no_vac) %>% 
    left_join(params_table[i,cn:date_start], 
              by = c("population" = "cn")) -> res[[i]]
  
  if(i%%10 == 0) print(i)
}

res %>% bind_rows() -> x
write_rds(x, "data/intermediate/simulationb_res.rds")
