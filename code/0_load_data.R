require(pacman)
p_load(tidyverse, sf, data.table, countrycode, mgcv)

##### load covidm #####
cm_path <- "code/covidm_for_fitting/"
cm_force_rebuild <- F
cm_build_verbose <- T
cm_version <- 2
source(paste0(cm_path, "/R/covidm.R"))
source("code/util_functions.R")

#### Find African Union Members ####
# rvest::read_html("https://au.int/en/memberstates") %>% 
#   rvest::html_table() %>% 
#   .[[1]] %>% .[,2] %>% 
#   setNames("country_name") %>% 
#   mutate(iso3c = countrycode(country_name, 
#                              "country.name", 
#                              "iso3c")) -> members
# write_rds(members,"data/members.rds")
members <- read_rds("data/members.rds")

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

#### get contact matrices ####
path_tmp <- "C:/Users/eideyliu/Dropbox/Github_Data/COVID-Vac_Delay/"
load(paste0(path_tmp, "contact_all.rdata"))
load(paste0(path_tmp, "contact_work.rdata"))
load(paste0(path_tmp, "contact_home.rdata"))
load(paste0(path_tmp, "contact_school.rdata"))
load(paste0(path_tmp, "contact_others.rdata"))
members[-which(!members$iso3c %in% names(contact_all)),] -> members
members %>% 
  left_join(pop[,c("name","iso3c")], by = "iso3c") %>% 
  dplyr::select(-country_name) %>% distinct() %>% 
  mutate_all(as.character)-> members

tmp <- cm_parameters_SEI3R("Thailand")
ag_labels <- tmp$pop[[1]]$group_names; rm(tmp)

for(i in 1:nrow(members)){
    cm_matrices[[members$name[i]]]$home <-
      as.matrix(contact_home[[members$iso3c[i]]]) 
    
    cm_matrices[[members$name[i]]]$work <-
      as.matrix(contact_work[[members$iso3c[i]]]) 
    
    cm_matrices[[members$name[i]]]$school <-
      as.matrix(contact_school[[members$iso3c[i]]]) 
    
    cm_matrices[[members$name[i]]]$other <-
      as.matrix(contact_others[[members$iso3c[i]]])
    
    colnames(cm_matrices[[members$name[i]]]$home) <- ag_labels
    colnames(cm_matrices[[members$name[i]]]$work) <- ag_labels
    colnames(cm_matrices[[members$name[i]]]$school) <- ag_labels
    colnames(cm_matrices[[members$name[i]]]$other) <- ag_labels
    
    rownames(cm_matrices[[members$name[i]]]$home) <- ag_labels
    rownames(cm_matrices[[members$name[i]]]$work) <- ag_labels
    rownames(cm_matrices[[members$name[i]]]$school) <- ag_labels
    rownames(cm_matrices[[members$name[i]]]$other) <- ag_labels
}

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
nrow(members)

#### load contact data ####
schedule_raw <- read_rds("data/schedule_raw.rds")

#### load Vaccine Characteristics ####
source("code/0_4_Vac_Char.R")

#### load health system parameters ####
source("code/0_3_HealthSystem.R")

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

