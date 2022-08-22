require(data.table)
require(tidyverse)

# 2022/05/10
# total = 23347
# paste0("https://api.biorxiv.org/covid19/", seq(1, total, 30)) %>%
#   map(jsonlite::read_json, simplifyVector = TRUE) -> find_all
#
# lapply(find_all, "[[", "messages") %>% bind_rows() %>% data.table -> find_all_records
# lapply(find_all, "[[", "collection") %>% bind_rows() %>% data.table -> find_all_collection
# write_rds(find_all_collection, "data/medrxiv_all_covid.rds")

# for term "(SARS CoV-2 OR COVID-19) AND (vaccin*) AND (prioriti*) AND (model*)"

# mx <- read_rds("data/medrxiv_all_covid.rds") %>%
#   mutate(all_txt = paste(rel_title,
#                          rel_abs))
# # 
# # # (SARS-CoV-2 OR COVID-19) can be ignored because it's the COVID only dataset
# index_topic2 <- grepl("vaccin*|immuni*ation", mx$all_txt, ignore.case = T)
# index_topic3 <- grepl("cost-effectiv*|cost-benefit|cost-utility|return on investment|value proposition|economic analysis|cost effectiv*|cost utility|cost benefit", mx$all_txt, ignore.case = T)
# index_topic4 <- grepl("model*", mx$all_txt, ignore.case = T)
# # 
# selected <- which((index_topic2 + index_topic3 + index_topic4) == 3)
# # 
# mx[selected,c("rel_doi",
#               "rel_link",
#               "rel_title")] %>%
# write_excel_csv(., "data/Lit/medrxiv_0510.csv")

medrxiv <- read_csv("data/Lit/medrxiv_0510.csv")
pubmed <- read_csv("data/Lit/pubmed_0510.csv")

bind_rows(medrxiv[,c(3,1)] %>% setNames(c("Title", "DOI")),
          pubmed[,c("Title","DOI")]) %>% 
  mutate(Title = tolower(Title)) %>% 
  arrange(Title) %>%
  write_excel_csv(., "data/Lit/combined_0510.csv")



