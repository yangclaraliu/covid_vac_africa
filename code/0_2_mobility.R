#### mobility data ####
# gm_type <- c("retail", "grocery", "parks", "transit", "work", "residential")
# gm <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv") %>%
#   .[sub_region_1 == "" & sub_region_2 == "" & metro_area == ""] %>%
#   .[, wb := countrycode::countrycode(
#     country_region,
#     "country.name",
#     "iso3c"
#   )] %>%
#   .[, !c(
#     "sub_region_1", "sub_region_2", "metro_area", "iso_3166_2_code",
#     "census_fips_code", "country_region_code", "place_id"
#   )] %>%
#   setnames(., c(
#     "country_name", "date",
#     gm_type, "iso3c"
#   )) %>%
#   mutate_at(gm_type, as.numeric) %>%
#   filter(iso3c %in% members_complete$iso3c) -> gm
# # 
# # ###### project mobility ####
# gm %>%
#   melt(.,
#        id.vars = c("country_name", "iso3c", "date"),
#        measures.var = gm_type) %>%
#   .[, c("m",
#         "dow",
#         "doy",
#         "d",
#         "variable",
#         "value") := list(factor(month(date), levels = 1:12),
#                          factor(lubridate::wday(date)),
#                          lubridate::yday(date),
#                          as.numeric(date),
#                          factor(variable),
#                          (value + 100)/100)] %>%
#   left_join(., si %>%
#               mutate(d = as.numeric(date)) %>%
#               .[,c("d", "iso3c", "StringencyIndex")],
#             by = c("iso3c", "d")) %>%
#   filter(date < max(si$date)) %>%
#   filter(variable %in% c("retail",
#                          "transit",
#                          "grocery",
#                          "work"),
#          !is.na(value)) %>%
#   mutate(iso3c = factor(iso3c),
#          date_num = as.numeric(date)) -> tab
# #
# # #
# fit <- gam(formula = value ~  dow + s(iso3c, bs = "re")  +
#              dow*variable + s(StringencyIndex) + m, #+ s(date_num),
#            data = tab,
#            na.action = na.omit)
# # # 
# # summary(fit)
# # # 
# CJ(date = seq(range(tab$date)[1],
#               as.Date("2022-12-31"),
#               by = 1),
#    iso3c = unique(tab$iso3c),
#    variable = c("retail",
#                 "transit",
#                 "grocery",
#                 "work")) %>%
#   .[, c("dow",
#         "doy",
#         "m",
#         "date_num") := list(lubridate::wday(date) %>% factor,
#                             lubridate::yday(date),
#                             month(date),
#                             if_else(date <= max(tab$date),
#                                     as.numeric(date),
#                                     as.numeric(max(tab$date))))]  %>%
#   # .[, m := if_else(m == 12, 11, m)] %>%
#   # .[, m := if_else(m == 1, 2, m)] %>%
#   .[, m := factor(m)] %>%
#   .[, date := as.character(date)] %>%
#   left_join(si[,c("iso3c","date", "StringencyIndex")] %>%
#               mutate(date = as.character(date)),
#             by = c("iso3c", "date")) %>%
#   split(by = "variable") %>%
#   map(arrange, date) %>%
#   bind_rows() %>%
#   filter(iso3c %in% unique(tab$iso3c)) -> pre_tab
# # 
# pre_tab[,"value"] <- predict(fit, newdata = pre_tab)
# # # 
# pre_tab %>%
#   mutate(variable = paste0(variable,"_predicted")) %>%
#   dplyr::select(date, iso3c, variable, value) %>%
#   pivot_wider(names_from = variable,
#               values_from = value) %>%
#   left_join(gm %>%
#               mutate(date = as.character(date)) %>%
#               dplyr::select(-country_name),
#             by = c("date","iso3c")) %>%
#   mutate(grocery_c = if_else(is.na(grocery), grocery_predicted, (grocery+100)/100),
#          retail_c = if_else(is.na(retail), retail_predicted, (retail+100)/100),
#          transit_c = if_else(is.na(transit), transit_predicted, (transit+100)/100),
#          work_c = if_else(is.na(work), work_predicted, (work+100)/100),
#          grocery_source = if_else(is.na(grocery), "gam", "obs"),
#          retail_source = if_else(is.na(retail), "gam", "obs"),
#          transit_source = if_else(is.na(transit), "gam", "obs"),
#          work_source = if_else(is.na(work), "gam", "obs"),
#          ) %>%
#   dplyr::select(date, iso3c,
#                 ends_with("_c"),
#                 ends_with("_source")) %>%
#   pivot_longer(cols = ends_with("_c"),
#                names_to = "setting",
#                values_to = "mobility") %>%
#   pivot_longer(cols = ends_with("_source")) %>%
#   separate(setting, into = c("setting1","seg1")) %>%
#   separate(name, into = c("setting2", "seg2")) %>%
#   filter(setting1 == setting2,
#          !is.na(value)) %>%
#   dplyr::select(-seg1, -seg2, -setting2) %>%
#   rename(setting = setting1) -> tmp
# 
# # tmp %>% 
# #   mutate(date = ymd(date)) %>% 
# #   ggplot(., aes(x = date, y = mobility, group = setting, color = setting)) +
# #   geom_line() +
# #   facet_wrap(~iso3c)
# 
# # # imputed by time
# # write_rds(tmp, "data/gm_t.rds")
# gm <- read_rds("data/gm_t.rds")
# 
# CJ(iso3c = members_complete$iso3c,
#    date = unique(gm$date)) %>%
#   left_join(shape %>%
#               data.table %>%
#               dplyr::select(ISO3_CODE) %>%
#               rownames_to_column(var = "country_index") %>%
#               rename(iso3c = ISO3_CODE),
#             by = "iso3c") %>%
#   left_join(gm %>%
#               dplyr::select(-value) %>%
#               pivot_wider(names_from = setting, values_from = mobility), 
#             by = c("iso3c", "date")) %>%
#   mutate(source = if_else(is.na(retail), "assumed", "empirical")) %>%
#   dplyr::select(-source) -> gm
# 
# gm_tmp <- gm 
# 
# lapply(which(colnames(gm) %in% gm_type[!(gm_type %in% c("parks","residential"))]),
#        function(i) {which(is.na(data.frame(gm)[,i]))}) %>%
#   unlist %>%
#   unique %>%
#   sort -> r_missing
# #
# for(r in r_missing){
#   tmp <- gm[r,]
#   # venues to
#   v_tmp <- tmp %>%
#     pivot_longer(cols = colnames(gm)[4:7]) %>%
#     filter(is.na(value)) %>%
#     pull(name)
# 
#   gm_tmp %>%
#     filter(country_index %in% nb[[as.numeric(tmp$country_index)]],
#            date == tmp$date) %>%
#     dplyr::select(colnames(gm)[4:7]) %>%
#     colMeans(., na.rm = T) -> imputed_vals
# 
#   for(v in v_tmp){
#     imputed_vals[v] -> gm_tmp[r,v]
#   }
#   gm_tmp[r,"source"] <- "assumed"
# 
#   print(r)
# }
# #
# 
# lapply(which(colnames(gm_tmp) %in% gm_type[!(gm_type %in% c("parks",
#                                                             "residential"))]),
#        function(i) {which(is.na(data.frame(gm_tmp)[,i]))}) %>%
#   unlist %>%
#   unique %>%
#   sort -> r_missing
# #
# for(r in r_missing){
#   tmp <- gm[r,]
#   # venues to
#   v_tmp <- tmp %>%
#     pivot_longer(cols = gm_type[c(1,2,4,5)]) %>%
#     filter(is.na(value)) %>%
#     pull(name)
# 
#   gm_tmp %>%
#     filter(country_index %in% nb[[as.numeric(tmp$country_index)]],
#            date == tmp$date) %>%
#     dplyr::select(gm_type[c(1,2,4,5)]) %>%
#     colMeans(., na.rm = T) -> imputed_vals
# 
#   for(v in v_tmp){
#     imputed_vals[v] -> gm_tmp[r,v]
#   }
#   gm_tmp[r,"source"] <- "assumed"
# 
#   print(r)
# }
# 
# gm_tmp %>%
#   # dplyr::select(-parks, -residential) %>%
#   pivot_longer(cols = c("retail", "grocery", "transit", "work")) %>%
#   mutate(date = ymd(date)) %>%
#   ggplot(., aes(x = date, y = value, group = name, color = name)) +
#   geom_line() +
#   facet_wrap(~iso3c, scales = "free") -> p
# 
# # ggsave("figs/supplemental/mobility_imputed.png", p,
# #        width = 20, height = 10)
# 
# # write_rds(gm_tmp, "data/gm_ts.rds")
gm_ts <- read_rds("data/gm_ts.rds")

gm_ts %>% filter(iso3c == "ZWE") %>% 
  mutate(date = ymd(date)) %>% 
  ggplot(., aes(x = date, y = grocery)) +
  geom_line()

# gm_ts %>%
#   ggplot(., aes(x = date, y = get("work"), group = iso3c)) +
#   geom_line() +
#   facet_wrap(~iso3c)
# 
# ####### mobility scalers ####
curves <- data.table(
  work_scaler = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0.008, 0.021, 0.033, 0.046, 0.058, 0.071, 0.083, 0.096, 0.108, 0.121, 0.133,
    0.146, 0.158, 0.171, 0.183, 0.196, 0.208, 0.221, 0.233, 0.246, 0.258, 0.271,
    0.283, 0.296, 0.308, 0.321, 0.334, 0.346, 0.359, 0.371, 0.384, 0.397, 0.41,
    0.422, 0.435, 0.448, 0.461, 0.474, 0.487, 0.5, 0.513, 0.526, 0.539, 0.552,
    0.566, 0.579, 0.592, 0.606, 0.619, 0.633, 0.646, 0.66, 0.674, 0.687, 0.701,
    0.715, 0.729, 0.743, 0.757, 0.771, 0.785, 0.799, 0.813, 0.828, 0.842, 0.856,
    0.87, 0.885, 0.899, 0.914, 0.928, 0.942, 0.957, 0.971, 0.986, 1, 1.014, 1.029,
    1.043, 1.058, 1.072, 1.087, 1.101, 1.115, 1.13, 1.144, 1.159, 1.173, 1.188,
    1.202, 1.216, 1.231, 1.245, 1.26, 1.274, 1.289, 1.303, 1.317, 1.332, 1.346, 1.361
  ),
  other_scaler = c(
    0.064, 0.066, 0.067, 0.068, 0.069, 0.071, 0.072, 0.073, 0.075, 0.076, 0.077, 0.078,
    0.08, 0.081, 0.082, 0.084, 0.085, 0.086, 0.087, 0.089, 0.09, 0.091, 0.092, 0.094,
    0.095, 0.096, 0.098, 0.099, 0.1, 0.101, 0.103, 0.104, 0.105, 0.106, 0.108, 0.109,
    0.11, 0.112, 0.113, 0.114, 0.116, 0.118, 0.119, 0.121, 0.123, 0.125, 0.128, 0.13,
    0.132, 0.135, 0.137, 0.14, 0.143, 0.146, 0.15, 0.154, 0.159, 0.164, 0.169, 0.175,
    0.182, 0.19, 0.198, 0.207, 0.217, 0.228, 0.24, 0.252, 0.266, 0.28, 0.295, 0.31,
    0.327, 0.344, 0.361, 0.379, 0.398, 0.418, 0.438, 0.459, 0.48, 0.502, 0.525, 0.549,
    0.572, 0.597, 0.621, 0.647, 0.672, 0.698, 0.725, 0.751, 0.778, 0.805, 0.833, 0.86,
    0.888, 0.916, 0.944, 0.972, 1, 1.028, 1.056, 1.084, 1.112, 1.14, 1.168, 1.196, 1.224,
    1.252, 1.28, 1.308, 1.337, 1.365, 1.393, 1.421, 1.449, 1.477, 1.505, 1.533, 1.561,
    1.589, 1.617, 1.645, 1.673, 1.701
  ),
  perc = round(seq(0, 1.25, 0.01), 2)
)
# 
gm_ts %>%
  # mutate_at(vars(gm_type[c(1,2,4,5)]), function(x) (x + 100)/100) %>%
  mutate(work = if_else(work > 1.25, 1.25, work),
         othx = 0.345*retail + 0.445*transit + 0.21*grocery,
         othx = if_else(othx > 1.25, 1.25, othx),
         work = round(work, 2),
         othx = round(othx, 2)) %>%
  left_join(curves[,c("perc","work_scaler")], by = c("work" = "perc")) %>%
  left_join(curves[,c("perc", "other_scaler")], by = c("othx" = "perc")) %>%
  dplyr::select(-c(grocery, retail, transit, work, othx, source)) %>%
  rename(work = work_scaler,
         other = other_scaler) %>%
  mutate(date = ymd(date)) -> gm_scaled

gm_scaled %>%
  mutate(date = ymd(date)) %>%
  ggplot(., aes(x = date, y = work)) +
  geom_line() +
  facet_wrap(~iso3c)

# 
country_data_length <-
  gm_scaled %>% group_by(iso3c) %>% group_split() %>% map(nrow) %>% unlist()

schedule_raw <- gm_scaled %>%
  mutate(home = 1,
         date = as.character(date)) %>%
  left_join(oxcgrt_C1 %>%
              dplyr::select(date, C1, iso3c) %>%
              setNames(c("date", "school", "iso3c")), # %>%
            # mutate(school = case_when(school == 0 ~ 1,
            #                           is.na(school) ~ 1,
            #                           school == 3 ~ 0,
            #                           TRUE ~ 0.5),
            by = c("date", "iso3c"))  %>%
  # filter(is.na(school)) %>% pull(date) %>% table
  # filter(is.na(school))
  # dplyr::filter(!is.na(school)) %>%
  mutate(school = case_when(school == 0 ~ 1,
                            school == 3 ~ 0,
                            is.na(school) ~ 1,
                            TRUE ~ 0.5)) %>%
  dplyr::select(iso3c, date, home, work, school, other)
# 
CJ(date = seq(as.Date("2019-12-01"), as.Date("2020-02-14"),1),
   iso3c = unique(members_complete$iso3c)) %>%
  .[,status := "assumed"] %>%
  .[,c("home",
       "work",
       "school",
       "other",
       "date") :=
      list(1,1,1,1,
           as.character(date))] -> schedule_pre

# # school holidays
schedule_raw %>%
  bind_rows(schedule_pre) %>%
  arrange(date) %>%
  mutate(date = lubridate::ymd(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         year = lubridate::year(date)) %>%
  mutate(holiday = if_else(
    #winter holiday,
    (year > 2020 & month == 12 & day >=  15) |
      (year > 2020 & month == 1 & day < 5) |
      # summer holiday
      month %in% c(7,8),
    T,
    F),
    school = if_else(holiday, 0, school)) %>%
  dplyr::select(-holiday, -status, -month, -day, -year) %>%
  mutate(date = ymd(date)) -> tmp

tmp %>%
  ggplot(., aes(x = date, y = work, group = iso3c)) +
  geom_line() +
  facet_wrap(~iso3c) +
  theme_cowplot() -> p

# # 
# ggsave("figs/supplemental/contact_school_imputed.png", p,
#        width = 20, height = 10)

# write_rds(tmp, "data/schedule_raw.rds")
