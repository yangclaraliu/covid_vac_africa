# ### stringency index ####
# oxcgrt_raw <- fread("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv")
# iso3c_dic <- oxcgrt_raw[,"CountryName"] %>%
#   distinct() %>%
#   mutate(iso3c = countrycode(CountryName, "country.name", "iso3c"))
# 
# ##### school related policies ####
# oxcgrt_raw %>%
#   filter(Jurisdiction == "NAT_TOTAL") %>%
#   mutate(date = lubridate::ymd(Date)) %>%
#   left_join(iso3c_dic, by = "CountryName") %>%
#   dplyr::filter(iso3c %in% members_complete$iso3c) %>%
#   dplyr::filter(C1_Flag == 1 | is.na(C1_Flag)) %>%
#   dplyr::select(iso3c, date, `C1_School closing`) %>%
#   rename(C1 = `C1_School closing`) %>%
#   pivot_wider(
#     names_from = date,
#     values_from = C1
#   ) %>%
#   ungroup()  %>%
#   pivot_longer(
#     cols = starts_with("202"),
#     names_to = "date",
#     values_to = "C1"
#   ) %>%
#   pivot_wider(names_from = iso3c, values_from = C1) %>%
#   group_by(date) %>%
#   pivot_longer(cols = AGO:ZWE,
#                names_to = "iso3c",
#                values_to = "C1") %>%
#   mutate(C1 = if_else(is.na(C1), 0, C1)) -> oxcgrt
# 
# members_complete[which(!(members_complete$iso3c %in% oxcgrt$iso3c)),]
# members_complete[which((members_complete$iso3c %in% oxcgrt$iso3c)),]
# 
# si <- oxcgrt_raw %>%
#   dplyr::select(CountryName, Date, StringencyIndex) %>%
#   mutate(date = lubridate::ymd(Date)) %>%
#   left_join(iso3c_dic, by = "CountryName") %>%
#   dplyr::select(
#     -Date,
#     -CountryName
#   ) %>%
#   distinct()
# 
# si %>%
#   filter(!is.na(StringencyIndex)) %>%
#   group_by(date) %>%
#   tally() %>%
#   mutate(
#     n_max = max(n),
#     missing = (n_max - n) / n_max
#   ) %>%
#   filter(missing > 0.1,
#          date > "2021-01-01") %>%
#   pull(date) %>%
#   min() -> si_stopdate
# 
# si %>%
#   filter(date < si_stopdate,
#          iso3c %in% members_complete$iso3c,
#          iso3c != "COM") %>%
#   group_by(iso3c) %>%
#   arrange(date) %>%
#   group_split() %>%
#   map(mutate, StringencyIndex = zoo::na.locf(StringencyIndex)) %>%
#   bind_rows() %>%
#   as.data.table() -> si
# 
# members_complete[which(!(members_complete$iso3c %in% si$iso3c)),]
# members_complete[which((members_complete$iso3c %in% si$iso3c)),]
# 
# qsave(si, "data/si.qs")
# qsave(oxcgrt, "data/oxcgrt.qs")

###### load all pre-processed data ####
si <- qread("data/si.qs")
oxcgrt_C1 <- qread("data/oxcgrt.qs")

si %>% 
  pivot_wider(names_from = iso3c, values_from = StringencyIndex) %>% 
  right_join(data.frame(date = seq(from = ymd("2020-01-01"), 
                                   to = ymd("2022-12-31"), 
                                   by = 1)),
             by = "date") %>% 
  mutate_at(vars(AGO:ZWE), zoo::na.locf) %>% 
  mutate(source = if_else(date <= max(si$date), "empirical", "assumed")) %>% 
  pivot_longer(cols = AGO:ZWE,
               names_to = "iso3c",
               values_to = "StringencyIndex") -> si

# members[which((members$iso3c %in% si$iso3c)),] -> members
si %>% 
  filter(source == "assumed") %>% 
  group_by(iso3c) %>% 
  filter(date == min(date)) %>%
  filter(iso3c %in% fitted_table$iso3c) %>% 
  filter(StringencyIndex %in% c(76.85, 19.44)) %>% left_join(members, by = "iso3c")
  pull(StringencyIndex) %>% summary

# si %>%
#   ggplot(., aes(x = date, y = StringencyIndex, col = source)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~iso3c) +
#   ggsci::scale_color_lancet() -> p

# ggsave("figs/supplemental/si_imputed.png", plot = p, width = 20, height = 10)

# 
si_missing <- members_complete[which(!members_complete$iso3c %in% si$iso3c),]$iso3c
si_missing %>% 
  map(~which(shape$ISO3_CODE == .)) %>% 
  map(~nb[[.]]) %>% 
  map(~shape[., ]) %>% 
  map(pull, ISO3_CODE) %>% 
  map(~si[si$iso3c %in% ., ]) %>% 
  map(group_by, date) %>% 
  map(summarise, StringencyIndex = mean(StringencyIndex, na.rm = T)) %>% 
  setNames(si_missing) %>% 
  bind_rows(.id = "iso3c") %>% 
  mutate(source = "imputed") %>% 
  dplyr::select(colnames(si)) %>% 
  bind_rows(si) -> si_complete

# si_complete %>% 
#   ggplot(., aes(x = date, y = StringencyIndex, col = source)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~iso3c) +
#   ggsci::scale_color_lancet() -> p
# 
# ggsave("figs/supplemental/si_imputed.png", plot = p, width = 20, height = 10)

