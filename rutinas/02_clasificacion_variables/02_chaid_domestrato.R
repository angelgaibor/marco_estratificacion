rm(list = ls())

library(tidyverse)
library(openxlsx)

upm_domest <- readRDS("intermedios/02_clasificacion_variables/upm_domest.rds")

domest_chaid <- upm_domest %>% 
  filter(n_estratos > 1) %>% 
  mutate(area = substr(domest, 3, 3),
         dominio = substr(domest, 1, 2),
         rnatura = case_when(dominio %in% c("01", "02", "03", "04", "05", "06",
                                            "10", "11", "17", "18", "30", "31",
                                            "32", "33", "34") ~ "1",
                             dominio %in% c("07", "08", "09", "12", "13", "20",
                                            "23", "24", "35", "40", "41", "42",
                                            "43") ~ "2",
                             dominio %in% c("14", "15", "16", "19", "21", "22") 
                             ~ "3",
                             T ~ "9"),
         rn_a = paste0(rnatura, "_", area))

revision_re <- read.xlsx("intermedios/02_clasificacion_variables/value_variables_region_natural_area.xlsx")

wbre <- createWorkbook("wb")

for(i in 26:51){
  addWorksheet(wbre, domest_chaid$domest[i])
  writeData(wbre, revision_re %>% 
              select(variable, categoria, ncategoria = categorias_, 
                     ends_with(domest_chaid$rn_a[i])) %>% 
              mutate(bienestar = ""), sheet = domest_chaid$domest[i])
}

saveWorkbook(wbre, "intermedios/02_clasificacion_variables/value_variables_domest_ag.xlsx",
             overwrite = T)

wbag <- createWorkbook("wb")

for(i in 1:25){
  addWorksheet(wbag, domest_chaid$domest[i])
  writeData(wbag, revision_re %>% 
              select(variable, categoria, ncategoria = categorias_, 
                     ends_with(domest_chaid$rn_a[i])) %>% 
              mutate(bienestar = ""), sheet = domest_chaid$domest[i])
}

saveWorkbook(wbag, "intermedios/02_clasificacion_variables/value_variables_domest_re.xlsx",
             overwrite = T)
