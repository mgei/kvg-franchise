library(tidyverse)
library(readxl)
library(magrittr)


lut <- read_excel("data/aufsichtsdaten-okp-1996-2018.xlsx", sheet = "2018", skip = 3) %>% 
  select(1:2) %>% 
  set_colnames(c("Versicherer", "Versicherung")) %>% 
  mutate(Versicherer = as.double(Versicherer)) %>% 
  filter(!is.na(Versicherer), !is.na(Versicherung))


premia <- read_excel("data/Prämien_CH.xlsx", sheet = "Export")

premia %<>% left_join(lut, by = "Versicherer") %>% 
  select(Versicherer, Versicherung, everything())


kosten <- tibble(KKosten = c(seq(0, 3000, by = 100)))

casedata <- premia %>% 
  filter(str_detect(Versicherung, "^Assura"),
         Unfalleinschluss == "OHN-UNF",
         Kanton == "BS",
         Tariftyp == "TAR-BASE",
         Altersklasse == "AKL-ERW") %>% 
  select(-Hoheitsgebiet) %>% 
  mutate(Franchise = str_remove(Franchise, "FRA-") %>% as.double())
  
combis <- casedata %>% 
  expand(kosten, Franchise) %>% 
  arrange(Franchise)

casedata %>% 
  left_join(combis, by = "Franchise") %>% 
  # compute costs
  rowwise() %>% 
  mutate(KFranchise = min(KKosten, Franchise),
         KSelbstbeh = min(max(700, (KKosten-Franchise)*0.1), 0),
         KPraemie = Prämie*12,
         KTotal = KFranchise + KSelbstbeh + KPraemie) %>% 
  select(Franchise, KKosten, KFranchise, KSelbstbeh, KPraemie) %>% 
  gather(Aufwand, Kosten, -Franchise, -KKosten) %>% 
  filter(Franchise == 300) %>% 
  ggplot(aes(x = Kosten, y = KKosten, fill = Aufwand)) +
  geom_col()
  

ggplot(aes(x = KKosten, y = KTotal, color = as.factor(Franchise), group = Franchise)) +
  geom_line() +
  geom_col(data = . %>% filter(Franchise == 2500), alpha = 0.4, aes(fill = ))
  


         
howmuch <- function(praemie, franchise, krankheitskosten) {
  jahrespraemie <- praemie*12
  selbstbehalt <- max(krankheitskosten*0.1, 700)
}
