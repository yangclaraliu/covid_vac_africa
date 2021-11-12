params_list <- list()

for(i in 1:nrow(members)){
  params_list[[members$name[i]]] <- gen_country_basics(members$name[i])
}

res_list <- list()

for(i in 1:nrow(members)){
  res_list[[members$name[i]]] <- cm_simulate(params_list[[members$name[i]]])
}

lapply(res_list, "[[", "dynamics") %>% 
  map(filter, compartment == "death_o") %>% 
  map(mutate, date = t + ymd("2020-01-01")) %>% 
  bind_rows() -> tmp

tmp %>% 
  ggplot(., aes(x = date, y = value, group = group, color = group)) +
  geom_line() +
  facet_wrap(~population, scales = "free") -> p

ggsave("figs/intermediate/toyf_4TD.png", p,
       width = 20, height = 15)
