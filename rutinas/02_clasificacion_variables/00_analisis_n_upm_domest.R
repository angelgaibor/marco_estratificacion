rm(list = ls())

library(tidyverse)

man_sec_upm <- readRDS("insumos/01_tratamiento/man_sec_upm_final_dmq.rds")

auxiliar_area <- readRDS("intermedios/01_tratamiento/parroquia_amadis_area.rds")

upm_domest <- man_sec_upm %>% 
  mutate(amadis = ifelse(substr(man_sec, 7, 9) == "999", "dis", "ama"),
         id_par = substr(man_sec, 1, 6)) %>% 
  left_join(auxiliar_area %>%
              select(id_par, amadis, area),
            by = c("id_par", "amadis")) %>% 
  mutate(provincia = substr(id_upm, 1, 2),
         id_canton = substr(id_upm, 1, 4),
         domgeo = case_when(id_canton=="1701"  ~ "33",
                            id_canton=="0901" & area==1  ~ "42",
                            id_canton=="0101"  ~ "30",
                            id_canton=="0701" & area==1  ~ "40",
                            id_canton=="1801"  ~ "34",
                            id_canton=="0801" & area==1  ~ "41",
                            id_canton=="2301"  ~ "35",
                            id_canton=="1308" & area==1 ~ "43",
                            id_canton=="1101"  ~ "32",
                            id_canton=="0601"  ~ "31",
                            T  ~ provincia)) %>% 
  
  mutate(domest = paste0(domgeo, area)) %>%
  group_by(id_upm, domest) %>% 
  summarise() %>% 
  group_by(domest) %>% 
  count() %>% 
  mutate(n_estratos = case_when(n < 200 ~ 1,
                                n >= 200 & n< 300 ~ 2,
                                n >= 300 ~ 3,
                                T ~ NA))

saveRDS(upm_domest, "intermedios/02_clasificacion_variables/upm_domest.rds")
