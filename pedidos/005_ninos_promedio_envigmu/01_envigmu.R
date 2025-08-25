rm(list = ls())

library(tidyverse)
library(rio)
library(arrow)

source("rutinas/funciones/open_marco.R")
source("rutinas/funciones/save_marco.R")

bdd <- open_marco("pedidos/005_ninos_promedio_envigmu/base_personas/")

hogar <- bdd |> 
  mutate(mujer = case_when(p02 == 2 & p03 >= 15 ~ 1,
                           T ~ 0),
         nino = case_when(p03 < 5 ~ 1,
                          T ~ 0)) |> 
  group_by(id_hogar) |> 
  summarise(hog_muj = max(mujer),
            hog_nin = max(nino),
            num_nin_hog = sum(nino))

provincia <- hogar |> 
  group_by(pro = substr(id_hogar, 1, 2)) |> 
  summarise(num_hog = n(),
            num_hog_muj = sum(hog_muj),
            num_hog_muj_nin = sum(hog_muj * hog_nin),
            num_nin = sum(num_nin_hog),
            num_nin_hog_muj = sum(num_nin_hog * hog_muj))

saveRDS(provincia, 
        "pedidos/005_ninos_promedio_envigmu/resumen_provincia.rds")