rm(list = ls())

library(tidyverse)
library(rio)

censo <- import("productos/01_tratamiento/marco_per.rds")

resumen_censo_pro <- censo |> 
  group_by(i01, id_hogar, id_vivienda) |> 
  summarise(tot_per12_hog = sum(p03 >= 12)) |> 
  ungroup() |> 
  group_by(pro = i01) |> 
  summarise(tot_hog = n(),
            tot_viv = n_distinct(id_vivienda),
            tot_per_12 = sum(tot_per12_hog),
            tot_hog_per12 = sum(tot_per12_hog > 0)) |> 
  ungroup() |> 
  mutate(pro = str_pad(pro, 2, "left", "0"),
         prop_hog_per12 = tot_hog_per12/tot_hog,
         prop_hog_viv = tot_hog/tot_viv)

resumen_censo_can <- censo |> 
  filter(i01 %in% c( 17, 9) & i02 == 1) |> 
  group_by(i01, i02, id_hogar, id_vivienda) |> 
  summarise(tot_per12_hog = sum(p03 >= 12)) |> 
  ungroup() |> 
  group_by(pro = i01, can = i02) |> 
  summarise(tot_hog = n(),
            tot_viv = n_distinct(id_vivienda),
            tot_per_12 = sum(tot_per12_hog),
            tot_hog_per12 = sum(tot_per12_hog > 0)) |> 
  ungroup() |> 
  mutate(id_can = paste0(str_pad(pro, 2, "left", "0") , str_pad(can, 2, "left", "0") ),
         prop_hog_per12 = tot_hog_per12/tot_hog,
         prop_hog_viv = tot_hog/tot_viv)

resultado <- resumen_censo_pro |> 
  rename(dominio = pro) |> 
  rbind(resumen_censo_can |> 
          select(dominio = id_can, tot_hog, tot_viv, tot_per_12, tot_hog_per12,
                 prop_hog_per12, prop_hog_viv))

export(resultado, "pedidos/004/info_censo_eut.rds")
