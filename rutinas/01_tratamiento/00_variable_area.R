rm(list=ls())

library(rio)
library(tidyverse)
library(magrittr)

pob <- import("insumos/01_tratamiento/censo/pob_2022.csv")

auxiliar <- pob %>% 
  mutate(I01 = str_pad(I01, 2, "left", "0"),
         I02 = str_pad(I02, 2, "left", "0"),
         I03 = str_pad(I03, 2, "left", "0"),
         id_par = paste0(I01, I02, I03),
         amadis = ifelse(I04 < 999, "ama", "dis")) %>% 
  group_by(id_par, amadis) %>% 
  summarise(per = n()) %>% 
  ungroup() %>% 
  mutate(area = case_when(amadis == "ama" & per >= 2000 ~ 1,
                          T ~ 2))

saveRDS(auxiliar, "intermedios/01_tratamiento/parroquia_amadis_area.rds")

# upm_ciu <- readRDS("F:/!INEC/marco_upm/pedidos/04_pareto/upm_ciu.rds")
# 
# lol <- upm_ciu %>% 
#   mutate(amadis = ifelse(substr(id_upm, 7, 7) == "9", "dis", "ama"),
#           id_par = substr(id_upm, 1, 6)) %>% 
#   group_by(id_par, amadis, area) %>% 
#   summarise()
# 
# prueba <- full_join(auxiliar, lol, by = c("id_par", "amadis"))
# 
# sum(prueba$area.x != prueba$area.y)
