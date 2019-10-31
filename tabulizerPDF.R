# read a PDF


library(tidyverse)
library(tabulizer)

# https://mgei.github.io/post/fuel-consumption/



out1 <- extract_tables("data/2019.10.01_zugelassene Krankenversicherer.pdf", 
                       output = "matrix", encoding = "UTF-8")


out1 %>% as_tibble()


out1 %>% length()
out1 %>% View()

kk_web <- bind_rows(
  out1[[2]][,c(1,3)] %>% as_tibble() %>% filter(row_number() != 1) %>% separate(V2, sep = "\\r", into = paste0("V", 2:13), fill = "left") %>% filter(str_detect(V13,"^www")) %>% select(V1, V2 = V13),
  out1[[3]][,c(1,3)] %>% as_tibble() %>% mutate(V1 = if_else(V1 == "", NA_character_, V1)) %>% fill(V1) %>% filter(str_detect(V2, "^www")),
  out1[[4]][,c(1,3)] %>% as_tibble() %>% mutate(V1 = if_else(V1 == "", NA_character_, V1)) %>% fill(V1) %>% filter(str_detect(V2, "^www")),
  out1[[5]][,c(1,3)] %>% as_tibble() %>% mutate(V1 = if_else(V1 == "", NA_character_, V1)) %>% fill(V1) %>% filter(str_detect(V2, "^www")),
  out1[[6]][,c(1,3)] %>% as_tibble() %>% mutate(V1 = if_else(V1 == "", NA_character_, V1)) %>% fill(V1) %>% filter(str_detect(V2, "^www")),
  out1[[7]][,c(1,3)] %>% as_tibble() %>% mutate(V1 = if_else(V1 == "", NA_character_, V1)) %>% fill(V1) %>% filter(str_detect(V2, "^www")),
  out1[[8]][,c(1,3)] %>% as_tibble() %>% mutate(V1 = if_else(V1 == "", NA_character_, V1)) %>% fill(V1) %>% filter(str_detect(V2, "^www")),
) %>% 
  mutate(weblink = paste0("http://", V2)) %>% 
  rename(VersNr = V1) %>% 
  select(-V2) %>% 
  mutate(VersNr = str_remove(VersNr, " x") %>% as.double())

kk_web %>% write_csv("shiny/data/kk_web.csv")
kk_web %>% saveRDS("shiny/data/kk_web.RDS")