# read a PDF



library(tabulizer)
install.packages("tabulizer")

# https://mgei.github.io/post/fuel-consumption/


library(tidyverse)

out1 <- extract_tables("vkatalog.pdf", output = "matrix", encoding = "UTF-8")