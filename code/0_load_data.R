require(pacman)
p_load(tidyverse, sf, data.table, countrycode, mgcv)

##### load covidm #####
cm_path <- "code/covidm_for_fitting/"
cm_force_rebuild <- F
cm_build_verbose <- T
cm_version <- 2
source(paste0(cm_path, "/R/covidm.R"))

#### Find African Union Members ####
rvest::read_html("https://au.int/en/memberstates") %>% 
  rvest::html_table() %>% 
  .[[1]] %>% .[,2] %>% 
  setNames("country_name") %>% 
  mutate(iso3c = countrycode(country_name, 
                             "country.name", 
                             "iso3c")) -> members


#### get contact matrices ####
path_tmp <- "C:/Users/eideyliu/Dropbox/Github_Data/COVID-Vac_Delay/"
load(paste0(path_tmp, "contact_all.rdata"))
load(paste0(path_tmp, "contact_work.rdata"))
load(paste0(path_tmp, "contact_home.rdata"))
load(paste0(path_tmp, "contact_school.rdata"))
load(paste0(path_tmp, "contact_others.rdata"))

# three countries do not have contact matrices attached
# Saharawi Arab Democratic Republic
# Republic of Seychelles
# Somali Republic
# we remove them here
members[-which(!members$iso3c %in% names(contact_all)),] -> members

#### get shapefile ####

read_sf(paste0(path_tmp, "CNTR_RG_60M_2020_4326.shp")) %>% 
  filter(ISO3_CODE %in% members$iso3c) -> shape
# 6, 13, 33, 38, 41
nb <- spdep::poly2nb(shape)
# 6 = Cape Verde
nb[[6]] <- grep("Gambia|Senegal|Bissau",shape$NAME_ENGL)
# 13
nb[[13]] <- grep("Mozambique",shape$NAME_ENGL)
# 33
nb[[33]] <- grep("Gabon|Cameroon",shape$NAME_ENGL)
# 38
nb[[38]] <- grep("Mozambique",shape$NAME_ENGL)
# 41
nb[[41]] <- grep("Mozambique",shape$NAME_ENGL)

#### get population information ####
# population structure is available for all 52 countries remaining
cm_populations %>% 
  filter(location_type == 4) %>% 
  dplyr::select(name) %>% distinct() %>% 
  mutate(iso3c = countrycode(name, "country.name", "iso3c")) %>% 
  dplyr::filter(iso3c %in% members$iso3c) %>% 
  dplyr::filter(!grepl("\\|",name),
                !is.na(iso3c)) %>% 
  left_join(cm_populations, by = "name") -> pop

#### our world in data ####
###### epi data ####
# owid_epi <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
# # 
# owid_epi[,"location"] %>% distinct() %>%
#   mutate(iso3c = countrycode(location, "country.name", "iso3c")) %>%
#   filter(!is.na(iso3c)) %>%
#   left_join(owid_epi, by = "location") %>%
#   .[, c("location", "iso3c","date", "new_deaths_smoothed", "new_cases_smoothed")] %>%
#   setNames(c("loc", "iso3c","date", "deaths", "cases")) %>%
#   mutate_at(vars(c("deaths", "cases")), ~if_else(is.na(.), 0, .))%>%
#   mutate_at(vars(c("deaths", "cases")), ~if_else(.<0, 0, .)) %>%
#   data.table %>%
#   .[,date := lubridate::ymd(date)] %>% 
#   dplyr::filter(iso3c %in% members$iso3c) -> epi
# 
# qsave(epi, "data/epi.qs")
owid_epi <- qread("data/epi.qs")

###### vaccine coverage data ####
# owid_vac <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") 
# owid_vac[,"location"] %>% distinct() %>%
#   mutate(iso3c = countrycode(location, "country.name", "iso3c")) %>%
#   filter(!is.na(iso3c),
#          iso3c %in% members$iso3c) %>%
#   left_join(owid_vac, by = "location") -> owid_vac
# 
# qsave(owid_vac, "data/owid_vac.qs")
owid_vac <- qread("data/epi.qs")

#### load stringency index ####
source("code/0_1_SI.R")



# gm %>% 
#   ggplot(., aes(x = date, y = get("residential"), group = iso3c)) +
#   geom_line() +
#   facet_wrap(~iso3c)

####### mobility scalers ####
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

gm %>% 
  mutate_at(vars(gm_type), function(x) (x + 100)/100) %>% 
    mutate(work = if_else(work > 1.25, 1.25, work),
           othx = 0.345*retail + 0.445*transit + 0.21*grocery,
           othx = if_else(othx > 1.25, 1.25, othx),
           work = round(work, 2),
           othx = round(othx, 2)) %>% 
  left_join(curves[,c("perc","work_scaler")], by = c("work" = "perc")) %>%
  left_join(curves[,c("perc", "other_scaler")], by = c("othx" = "perc")) %>%
  dplyr::select(-c(grocery, retail, transit, work, othx, parks, residential)) %>%
  rename(work = work_scaler,
         other = other_scaler) -> gm_scaled

gm_scaled %>% 
  ggplot(., aes(x = date, y = work)) +
  geom_line() +
  facet_wrap(~iso3c)


###### project mobility ####
gm %>%
  melt(.,
       id.vars = c("country_name", "iso3c", "date"),
       measures.var = gm_type) %>%
  .[, c("m",
        "dow",
        "doy",
        "d",
        "variable",
        "value") := list(factor(month(date), levels = 1:12),
                         factor(lubridate::wday(date)),
                         lubridate::yday(date),
                         as.numeric(date),
                         factor(variable),
                         (value + 100)/100)] %>%
  left_join(., si %>%
              mutate(d = as.numeric(date)) %>%
              .[,c("d", "iso3c", "StringencyIndex")],
            by = c("iso3c", "d")) %>%
  filter(date < si_stopdate) %>%
  filter(variable %in% c("retail",
                         "transit",
                         "grocery",
                         "work")) %>%
  mutate(iso3c = factor(iso3c),
         date_num = as.numeric(date)) -> tab
#  
fit <- gam(formula = value ~  dow + s(iso3c, bs = "re") + variable +
            dow + s(StringencyIndex) + m + s(date_num),
           data = tab,
           na.action = na.omit)

summary(fit)

CJ(date = seq(range(tab$date)[1],
              as.Date("2022-12-31"),
              by = 1),
   iso3c = unique(tab$iso3c),
   variable = c("retail",
                "transit",
                "grocery",
                "work")) %>%
  .[, c("dow",
        "doy",
        "m",
        "date_num") := list(lubridate::wday(date) %>% factor,
                     lubridate::yday(date),
                     month(date),
                     as.numeric(date))]  %>%
  .[, m := if_else(m == 12, 11, m)] %>%
  .[, m := if_else(m == 1, 2, m)] %>%
  .[, m := factor(m)] %>%
  .[, date := as.character(date)] %>% 
  left_join(si[,c("iso3c","date", "StringencyIndex")] %>% 
              mutate(date = as.character(date)),
            by = c("iso3c", "date")) %>%
  split(by = "variable") %>%
  map(arrange, date) %>%
  bind_rows() -> pre_tab

pre_tab %>% 
  ggplot(., aes(x = date, y = StringencyIndex, 
                color = variable, group = iso3c)) +  
  geom_line() +
  facet_wrap(~iso3c)

#### load epidemic parameters ####
###### Clinical Fraction ####
# (based on Davies et al, Nature paper) 
cf <- c(
  0.2904047, 0.2904047, 0.2070468, 0.2070468, 0.2676134,
  0.2676134, 0.3284704, 0.3284704, 0.3979398, 0.3979398,
  0.4863355, 0.4863355, 0.6306967, 0.6306967, 0.6906705, 0.6906705
)

###### susceptibility ####
# (based on Davies et al, Nature paper)
sus <- c(
  0.3956736, 0.3956736, 0.3815349, 0.3815349, 0.7859512,
  0.7859512, 0.8585759, 0.8585759, 0.7981468, 0.7981468,
  0.8166960, 0.8166960, 0.8784811, 0.8784811, 0.7383189, 0.7383189
)

