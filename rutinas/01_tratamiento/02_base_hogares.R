rm(list=ls())

library(rio)
library(tidyverse)
library(magrittr)

#cargamos la base de marco viviendas ya trabajada
marco_viv <- readRDS("productos/01_tratamiento/marco_viv.rds")

hog <- import("insumos/01_tratamiento/censo/hog_2022.csv")

### Población

hog_pp <- hog %>% rename_all(tolower) %>%
  mutate(provincia = str_pad(i01, 2, "left", pad = "0"),
         canton = str_pad(i02, 2, "left", pad = "0"),
         parroquia = str_pad(i03, 2, "left", pad = "0"),
         zona = str_pad(i04, 3, "left", pad = "0"),
         sector = str_pad(i05, 3, "left", pad = "0"),
         manzana = str_pad(i06, 3, "left", pad = "0"),
         man_sec = ifelse(zona == "999", 
                          paste0(provincia, canton, parroquia, zona, sector),
                          paste0(provincia, canton, parroquia, zona, sector, manzana)),
         manz_loc = ifelse(is.na(i06) & !is.na(i07),  i07, NA), 
         manz_loc = ifelse(is.na(i07) & !is.na(i06),  i06, manz_loc),
         manz_loc = str_pad(manz_loc, 3, "left", pad = "0"),
         edificio = str_pad(i08, 3, "left", pad = "0"),
         vivienda = str_pad(i10, 3, "left", pad = "0"),
         id_sector = paste0(provincia, canton, parroquia, zona, sector),
         id_manzana = paste0(provincia, canton, parroquia, zona, sector,manz_loc),
         id_edificio = paste0(id_manzana, edificio),
         id_vivienda = paste0(id_edificio, vivienda),
         hogar = as.character(inh),
         id_hogar = paste0(id_edificio, vivienda, hogar)) %>% 
  left_join(
    marco_viv %>% mutate(one = 1) %>% select(id_vivienda, one, v0201),
    by = "id_vivienda")

# Formacion del marco de hogares y agregamos las variables
# id_upm y area

man_sec_upm <- readRDS("insumos/01_tratamiento/man_sec_upm_final_dmq.rds")

auxiliar_area <- readRDS("intermedios/01_tratamiento/parroquia_amadis_area.rds")

marco_hog <- hog_pp %>%
  filter(v0201 %in% c(1,2)) %>% 
  mutate(id_par = paste0(provincia, canton, parroquia),
         amadis = ifelse(i04 < 999, "ama", "dis")) %>% 
  left_join(man_sec_upm %>% 
              select(man_sec, id_upm), 
            by = "man_sec") %>% 
  left_join(auxiliar_area %>% 
              select(-per), 
            by = c("id_par", "amadis")) %>% 
  left_join(marco_viv %>% select(v01:totper, id_vivienda), 
            by= "id_vivienda")

# Guardar las bases
export(marco_hog, "productos/01_tratamiento/marco_hog.rds")
