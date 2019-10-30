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

lut_gemeinden <- read_excel("data/do-t-09.02-gwr-37.xlsx", sheet = 2)
lut_gemeinden

hmo <- read_excel("data/Regionen mit HMO-Standorten und Einzugsgebiete.xlsx")

lut_hmo <- hmo %>% 
  filter(!is.na(`Gemeinden-BFS`)) %>%
  # summarise(maxgem = max(str_count(`Gemeinden-BFS`, ","))) # 180 is max
  separate(`Gemeinden-BFS`, into = paste0("gem", 1:181), sep = ",", fill = "right") %>% 
  gather(key, Gemeindecode, 
         -Versicherer, -Kanton, -Hoheitsgebiet, -Geschäftsjahr, -Erhebungsjahr, -Region, -Tarif, -Tariftyp, -`HMO-ID`, -Eingeschränkt) %>% 
  select(-key) %>% 
  mutate(Gemeindecode = as.double(Gemeindecode)) %>% 
  full_join(lut_gemeinden, by = c("Gemeindecode" = "GDENR")) %>% 
  mutate(PlzGemeinde = paste(PLZ4, GDENAMK))

# premiaF <- premia %>% 
#   mutate(Franchise = str_remove(Franchise, "FRA-") %>% as.double())

# premiaF %>% write_csv("shiny/data/premia.csv")
# premiaF %>% saveRDS("shiny/data/premia.RDS")

lut_hmo %>% write_csv("shiny/data/lut_hmo.csv")
lut_hmo %>% saveRDS("shiny/data/lut_hmo.RDS")


kosten <- tibble(KKosten = c(0, 500, 1000, 2000, 3000, 5000, 10000, 50000))

kanton <- "BS"
unfall <- "ohne Unfall"
modell <- "Standard" 
alter <- "Erwachsene"

unfalleinschluss <- function(unfall) {
  Unfalleinschluss <- case_when(unfall == "Nein|nein|ohne Unfall|o. Unfall" ~ "OHN-UNF",
                                isFALSE(unfall)       ~ "OHN-UNF",
                                unfall == "Ja|ja|mit Unfall|m. Unfall|Unfalleinschluss"     ~ "MIT-UNF",
                                isTRUE(unfall)        ~ "MIT-UNF",
                                T                     ~ "OHN-UNF")
  return(Unfalleinschluss)
}

tariftyp <- function(modell) {
  Tariftyp <- case_when(modell == "Basis|Standard" ~ "TAR-BASE",
                        modell == "andere|Andere|alternative|Alternative|diverse|Diverse|div" ~ "TAR-DIV",
                        modell == "Hausarzt|Hausarztmodell" ~ "TAR-HAM",
                        modell == "HMO"            ~ "TAR-HMO",
                        T                          ~ "TAR-BASE")
  return(Tariftyp)
}

altersklasse <- function(alter) {
  Altersklasse <- case_when(alter == "Erwachsene" ~ "AKL-ERW",
                            alter == "Jugendliche" ~ "AKL-JUG",
                            alter == "Kinder"     ~ "TAR-KIN",
                            T                     ~ "TAR-ERW")
  return(Altersklasse)
}



premia %>% 
  filter(str_detect(Versicherung, "^CSS")) %>% 
  group_by(Versicherer, Versicherung) %>% 
  count()
  
versicherung <- "CSS Kranken-Versicherung AG"

casedata <- premia %>% 
  filter(Versicherung ==  versicherung,
         Unfalleinschluss == unfalleinschluss(unfall),
         Kanton == kanton,
         Tariftyp == tariftyp(modell),
         Altersklasse == altersklasse(alter)) %>% 
  select(-Hoheitsgebiet) %>% 
  mutate(Franchise = str_remove(Franchise, "FRA-") %>% as.double())
  
combis <- casedata %>% 
  expand(kosten, Franchise) %>% 
  arrange(Franchise)

finaldata <- casedata %>% 
  left_join(combis, by = "Franchise") %>% 
  # compute costs
  rowwise() %>% 
  mutate(KFranchise = min(KKosten, Franchise),
         KSelbstbeh = max(min(700, (KKosten-Franchise)*0.1), 0),
         KPraemie = Prämie*12,
         KTotal = KFranchise + KSelbstbeh + KPraemie) %>% 
  group_by(KKosten) %>% 
  mutate(minFranchise = if_else(KTotal == min(KTotal), "min", "z"),
         minFranchise = if_else(KTotal == max(KTotal), "max", minFranchise))


finaldata %>% 
  select(Franchise, KKosten, KFranchise, KSelbstbeh, KPraemie) %>%
  gather(Aufwand, Kosten, -Franchise, -KKosten) %>% 
  ggplot(aes(x = 1, y = Kosten)) +
  geom_col(aes(fill = factor(Aufwand, levels = c("KSelbstbeh", "KFranchise", "KPraemie"))), width = 0.8) +
  geom_text(data = finaldata, aes(label = scales::number(KTotal, big.mark = "'"), y = 2500, color = minFranchise), size = 4) +
  scale_color_manual(values = c("red", "white", "black"), guide = F) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  labs(fill = "", x = "gewählte Franchise", y = "Krankheitskosten",
       title = "Gesamtkosten Prämie + Franchise + Selbstbehalt",
       subtitle = str_c(versicherung, kanton, alter, modell, unfall, "2020", sep = ", ")) +
  facet_grid(rows = vars(KKosten), cols = vars(Franchise), switch = "both") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_text(size = 15))

