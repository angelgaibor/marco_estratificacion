rm(list = ls())

library(tidyverse)
library(openxlsx)

tamanio <- read.xlsx("pedidos/001/tam_enighur.xlsx")

marco_upm <- readRDS("D:/MAG/marco_estratificacion/productos/04_marco/marco_upm.rds")

distribucion <- marco_upm |> 
  group_by(pro, domest, estrato) |> 
  summarise(Mh = sum(Mi),
            Nh = n()) |> 
  ungroup() |> 
  mutate(dominio = case_when(substr(domest, 1, 2) == "31" ~ "06",
                             T ~ substr(domest, 1, 2))) |> 
  left_join(tamanio, by = "dominio") |> 
  group_by(dominio) |> 
  mutate(prop = Mh/sum(Mh),
         num_estratos = n_distinct(estrato)) |> 
  ungroup() |> 
  mutate(nh0 = n0 * prop,
         nh1 = pmax(3, floor(nh0)),
         dif = nh0 - nh1) |>
  arrange(dominio, desc(dif)) |> 
  group_by(dominio) |> 
  mutate(resto = n0 - sum(nh1),
         orden = row_number()) |> 
  ungroup() |> 
  mutate(extra1 = case_when(orden <= resto ~ 1, T ~ 0),
         nh2 = nh1 + extra1)

sum(distribucion$nh2) == sum(tamanio$n0)

saveRDS(distribucion |> select(estrato, nh = nh2), "pedidos/001/distribucion_enighur.rds")
