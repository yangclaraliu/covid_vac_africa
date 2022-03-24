require(pacman)
p_load(tidyverse, sf, data.table, countrycode, mgcv,
       magrittr, testthat, ISOcodes)

path_dropbox <- "C:/Users/eideyliu/Dropbox/Github_Data/COVID-19_africa_cdc/"

cost_health_unit <- readxl::read_xlsx(paste0(path_dropbox, "COVID care unit cost - Africa.xlsx")) %>% 
  .[5:nrow(.),]


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
# members[members$country_name == "Somali Republic", "iso3c"] <- "SOM"
# members[members$country_name == "Saharawi Arab Democratic Republic", "iso3c"] <- "ESH"
# 
# cm_populations %>%
#   filter(location_type == 4) %>%
#   dplyr::select(name) %>% distinct() %>%
#   mutate(iso3c = countrycode(name, "country.name", "iso3c")) %>%
#   filter(iso3c %in% members$iso3c,
#          !grepl("\\|",name)) -> cn_dic
# 
# 
# members %>%
#   left_join(cn_dic, by = "iso3c") %>%
#   rename(name_official = country_name,
#          name_internal = name) -> members
# # 
# write_rds(members,"data/members.rds")
members <- read_rds("data/members.rds")
members_complete <- read_rds("data/members.rds")

#### get population information ####
# population structure is available for all 52 countries remaining
cm_populations %>% 
  filter(location_type == 4) %>% 
  dplyr::select(name) %>% distinct() %>% 
  mutate(iso3c = countrycode(name, "country.name", "iso3c")) %>% 
  dplyr::filter(iso3c %in% members_complete$iso3c) %>% 
  dplyr::filter(!grepl("\\|",name),
                !is.na(iso3c)) %>% 
  left_join(cm_populations, by = "name") -> pop

pop %>%
  group_by(name, iso3c) %>% 
  summarise(tot = sum(f + m)) -> vac_denom

#### get contact matrices ####
path_tmp <- "C:/Users/eideyliu/Dropbox/Github_Data/COVID-Vac_Delay/"
load(paste0(path_tmp, "contact_all.rdata"))
load(paste0(path_tmp, "contact_work.rdata"))
load(paste0(path_tmp, "contact_home.rdata"))
load(paste0(path_tmp, "contact_school.rdata"))
load(paste0(path_tmp, "contact_others.rdata"))

members_complete[which(!members_complete$iso3c %in% names(contact_all)),] 

tmp <- cm_parameters_SEI3R("Thailand")
ag_labels <- tmp$pop[[1]]$group_names; rm(tmp)

for(i in 1:nrow(members_complete)){
  
  source <- x <- as.character(members_complete$name_internal[i])
  if(x == "Western Sahara") { source = "Morocco" }
  if(x == "Seychelles") { source = "Madagascar" }
  if(x == "Somalia") { source = "Ethiopia" }
  
  iso_tmp <- members_complete$iso3c[which(members_complete$name_internal == source)]
  
  cm_matrices[[x]]$home <- as.matrix(contact_home[[iso_tmp]]) 
  cm_matrices[[x]]$work <- as.matrix(contact_work[[iso_tmp]]) 
  cm_matrices[[x]]$school <- as.matrix(contact_school[[iso_tmp]]) 
  cm_matrices[[x]]$other <- as.matrix(contact_others[[iso_tmp]])
  
  colnames(cm_matrices[[x]]$home) <- ag_labels
  colnames(cm_matrices[[x]]$work) <- ag_labels
  colnames(cm_matrices[[x]]$school) <- ag_labels
  colnames(cm_matrices[[x]]$other) <- ag_labels
  
  rownames(cm_matrices[[x]]$home) <- ag_labels
  rownames(cm_matrices[[x]]$work) <- ag_labels
  rownames(cm_matrices[[x]]$school) <- ag_labels
  rownames(cm_matrices[[x]]$other) <- ag_labels
}

#### get shapefile ####
read_sf(paste0(path_tmp, "CNTR_RG_60M_2020_4326.shp")) %>% 
  mutate(continent = countrycode(NAME_ENGL,
                                 "country.name",
                                 "continent")) %>% 
  filter(continent == "Africa") %>% 
  filter(ISO3_CODE != "SHN") -> shape

 # 6, 13, 33, 38, 41
nb <- spdep::poly2nb(shape)
shape[c(6,13,26,36,41,44),]
# 6 = Cape Verde
nb[[6]] <- grep("Gambia|Senegal|Bissau",
                shape$NAME_ENGL)
# 13 = Comoros
nb[[13]] <- grep("Mozambique",
                 shape$NAME_ENGL)
# 26 = Seychelles
nb[[26]] <- grep("Kenya|Somali",
                 shape$NAME_ENGL)

# 36 = Sao Tome and Principe 
nb[[36]] <- grep("Gabon|Cameroon",
                 shape$NAME_ENGL)
# 41 = Madagascar
nb[[41]] <- grep("Mozambique",
                 shape$NAME_ENGL)
# 44 = Mauritius
nb[[44]] <- grep("Mozambique",
                 shape$NAME_ENGL)

#### our world in data ####
###
### epi data ####
# #

# owid_epi <- read_csv(url("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"))
# 
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
#   dplyr::filter(iso3c %in% members_complete$iso3c) -> epi
# 
# qsave(epi, "data/epi.qs")
owid_epi <- qread("data/epi.qs")
# which(!(members_complete$iso3c %in% owid_epi$iso3c))



###### vaccine coverage data ####
# owid_vac <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
# owid_vac[,"location"] %>% distinct() %>%
#   mutate(iso3c = countrycode(location, "country.name", "iso3c")) %>%
#   filter(!is.na(iso3c),
#          iso3c %in% members_complete$iso3c) %>%
#   left_join(owid_vac, by = "location") -> owid_vac

# unique(owid_vac$iso3c) %>% length
# 
# qsave(owid_vac, "data/owid_vac.qs")
owid_vac <- qread("data/owid_vac.qs")

# members_complete[which(!(members_complete$iso3c %in% owid_vac$iso3c)),]
# 
# owid_vac %>% 
#   filter(!is.na(people_vaccinated)) %>% 
#   ggplot(., aes(x = date, y = people_vaccinated, group = iso3c)) +
#   geom_line() +
#   facet_wrap(~iso3c)

#### load stringency index ####
source("code/0_1_SI.R")
nrow(members)

#### load contact data ####
schedule_raw <- read_rds("data/schedule_raw.rds")

#### load Vaccine Characteristics ####
source("code/0_4_Vac_Char.R")

#### load health system parameters ####
source("code/0_3_HealthSystem.R")

#### load health econ related functions and data ####
source("code/0_5_HE.R")

#### load costs tables ####
source("code/0_6_costs.R")

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

#### vaccine roll-out schedules ####
ROS <- data.frame(ms0 = c(0,0, 0, 0),
           ms1 = c(0.03, 0.03, 0.03, 0.32),
           ms2 = c(0.1, 0.2, 0.3, 0.8),
           ms3 = c(0.2,0.4, 0.6, 0.8)) %>% 
  rownames_to_column(var = "ROS") %>% 
  pivot_longer(starts_with("ms")) %>% 
  rename(milestone_date = (name),
         coverage = value) %>% 
  mutate(milestone_date = factor(milestone_date,
                                 levels = paste0("ms",0:3),
                                 labels = c("2021-03-01",
                                            "2021-06-30",
                                            "2021-12-31",
                                            "2022-12-31")) %>% 
           ymd)


speed_labels <- c("slow", "medium", "fast")

scientific_10 <- function(x){
  scales::scientific_format()(x) %>%
    sub(pattern = "e\\+",
        x = .,
        replacement = "e") %>%
    sub(pattern = "e00", 
        x = ., 
        replacement = "") %>%
    gsub(pattern = "e",
         replacement = " %*% 10^",
         x = .) %>%
    sub(pattern = "1 %*% ",
        replacement = "",
        x = ., fixed = T) %>%
    parse(text = .)
}

