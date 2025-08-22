rm(list = ls())

library(tidyverse)

marco_per <- readRDS("productos/01_tratamiento/marco_per.rds")
marco_viv <- readRDS("productos/01_tratamiento/marco_viv.rds")
marco_hog <- readRDS("productos/01_tratamiento/marco_hog.rds")

# base de personas
n_distinct(marco_per$id_persona)
n_distinct(marco_per$id_persona) == dim(marco_per)[1]
n_distinct(marco_per$id_vivienda) == dim(marco_viv)[1]
n_distinct(marco_per$id_hogar) == dim(marco_hog)[1]
table(marco_per$area, useNA = "ifany")
sum(n_distinct(marco_per$id_upm))
sum(is.na(marco_per$id_upm))

# base de hogares
n_distinct(marco_hog$id_hogar)
n_distinct(marco_hog$id_hogar) == dim(marco_hog)[1]
n_distinct(marco_hog$id_vivienda) == dim(marco_viv)[1]
table(marco_hog$area, useNA = "ifany")
sum(n_distinct(marco_hog$id_upm))
sum(is.na(marco_hog$id_upm))

# base de viviendas
n_distinct(marco_viv$id_vivienda)
n_distinct(marco_viv$id_vivienda) == dim(marco_viv)[1]
sum(marco_viv$totper) == dim(marco_per)[1]
table(marco_viv$area, useNA = "ifany")
sum(n_distinct(marco_viv$id_upm))
sum(is.na(marco_viv$id_upm))

# upm perdida 
man_sec_upm <- readRDS("insumos/01_tratamiento/man_sec_upm_final_dmq.rds")
unique(man_sec_upm$id_upm)[!unique(man_sec_upm$id_upm) %in% unique(marco_hog$id_upm)]
# upm perdidas en la base a estratificar "0805559001" "1601590001"