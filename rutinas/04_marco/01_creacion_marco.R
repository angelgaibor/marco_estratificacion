rm(list = ls())

library(tidyverse)

#Se abre el archivo de correspondecia mansec upm

man_sec_upm <- readRDS("insumos/01_tratamiento/man_sec_upm_final_dmq.rds")

# Abrimos el archivo de particion de manzanas

particion_manzanas <- readRDS("insumos/04_marco/particion_manzanas_li_60.rds")

# se genera un objeto con el número de particiones por super manzana

resumen_super_manzanas <- particion_manzanas |> 
  group_by(man_sec = mansec) |> 
  summarise(ngrupo = max(grupo))

# se genera un objeto por con el número de viviendas por upm y grupo

resumen_viv_grupo <- particion_manzanas |> 
  left_join(man_sec_upm |> 
              select(mansec = man_sec, id_upm), by = "mansec") |> 
  group_by(id_upm, grupo) |> 
  summarise(Mip = sum(viv))

# juntamos la estratificación realizada por domest

index <- substr(list.files("productos/03_estratificacion/estratos/"), 1, 3)

estratos <- vector("list", 0)

for(i in 1 : length(index)){
  estratos[[i]] <- readRDS(paste0("productos/03_estratificacion/estratos/", index[i], ".rds")) %>% 
    mutate(domest = index[i])
}

estratos <- do.call(rbind, estratos) %>% 
  mutate(estrato = paste0(domest, est_e1)) %>% 
  select(id_upm, estrato)

# se abre el archivo con los domest

upm_domest <- readRDS("intermedios/02_clasificacion_variables/upm_domest.rds") %>% 
  filter(!domest %in% c("08NA", "16NA"))

# se identifica los domest donde no se realizó la estratificación

domest_no_est <- upm_domest$domest[!upm_domest$domest %in% index]

# se abre el archivo auxiliar para agregar el área

auxiliar_area <- readRDS("intermedios/01_tratamiento/parroquia_amadis_area.rds")

marco <- man_sec_upm %>% 
  # se incluye el número de grupos en los que se dividió cada super manzana
  left_join(resumen_super_manzanas, by = "man_sec") |> 
  mutate(ngrupo = ifelse(is.na(ngrupo), 1, ngrupo)) |> 
  # se agrega la variable de área
  mutate(id_par = substr(id_upm, 1, 6),
         amadis = ifelse(substr(id_upm, 7, 7) != "9", "ama", "dis"),
         id_canton = substr(id_upm, 1, 4)) %>% 
  left_join(auxiliar_area %>% 
              select(-per), 
            by = c("id_par", "amadis")) %>% 
  # se incluye el dominio de estratificación
  mutate(domgeo = case_when(id_canton == "0101"  ~ "30",
                            id_canton == "0601"  ~ "31",
                            id_canton == "1101"  ~ "32",
                            id_canton == "1701"  ~ "33",
                            id_canton == "1801"  ~ "34",
                            id_canton == "0701" & area == 1  ~ "40",
                            id_canton == "0801" & area == 1  ~ "41",
                            id_canton == "0901" & area == 1  ~ "42",
                            id_canton == "1308" & area == 1 ~ "43",
                            id_canton == "2301" & area == 1 ~ "44",
                            T  ~ substr(id_upm, 1, 2)),
         domest = paste0(domgeo,area),
         domest = case_when(substr(id_upm, 1, 2) == "20" ~ "209",
                            T ~ domest)) %>% 
  # se incluye los estratos en los domest estratificados
  left_join(estratos, by = "id_upm") %>% 
  mutate(estrato = case_when(is.na(estrato) & domest %in% domest_no_est ~ paste0(domest, "9"),
                             T ~ estrato),
         # se corrige el estrato para las manzanas perdidas por no tener personas en el censo
         estrato = case_when(man_sec == "080555999001" ~ "0821",
                             man_sec == "160159001001001" ~ "1629",
                             domest == "209" ~ "2099",
                             T ~ estrato),
         domest = substr(estrato, 1, 3),
         area = substr(estrato, 3, 3)) %>% 
  # se agrega la base a nivel de upm 
  group_by(id_upm, domest, area, estrato) %>% 
  summarise(Mi = sum(viv_ocu),
            ngrupo = max(ngrupo)) %>% 
  ungroup() |> 
  mutate(pro = substr(id_upm, 1, 2)) |> 
  # se repite el identificador de upm por el número de grupos
  group_by(id_upm, pro, domest, area, estrato, Mi) |> 
  expand(grupo = seq(1:ngrupo))


marco_01 <- marco |> 
  # se incluye el número de viviendas por grupo 
  left_join(resumen_viv_grupo, by = c("id_upm", "grupo")) |> 
  # se corrige el número de viviendas para las upm partidas
  mutate(Mi = ifelse(is.na(Mip), Mi, Mip)) |> 
  # se verifica que no se repita el id_upm grupo
  group_by(id_upm, grupo) |> 
  mutate(control = n()) |> 
  ungroup()

max(marco_01$control)

# se calcula el número de viviendas de las manzanas dentro de donas
# hubieron casos en las que súper manzanas se juntaron con manzanas
#contenidas en su interior

apoyo <- man_sec_upm |> 
  group_by(id_upm) |> 
  summarise(viv = sum(viv_ocu)) |> 
  left_join(marco_01 |> 
              group_by(id_upm) |> 
              summarise(vivmenos = sum(Mi)),
            by = "id_upm") |> 
  mutate(resto = viv - vivmenos) |> 
  filter(resto != 0)

sum(apoyo$id_upm %in% resumen_viv_grupo$id_upm) == dim(apoyo)[1]

# se incluye el número de viviendas de las manzanas absorvidas por las
# súper manzanas al primer grupo

marco_fin <- marco_01 |> 
  left_join(apoyo |> select(id_upm, resto), by = "id_upm") |> 
  mutate(resto = ifelse(is.na(resto), 0, resto),
         Mi = ifelse(grupo == 1, Mi + resto, Mi)) |> 
  mutate(id_upm_final = paste0(id_upm,
                               str_pad(grupo, 2, "left", "0"))) |> 
  select(id_upm = id_upm_final, pro, domest, area, estrato, Mi)

sum(marco_fin$Mi) == 4595701

apply(is.na(marco_fin), 2, sum)

saveRDS(marco_fin, "productos/04_marco/marco_upm.rds")  

control <- marco_fin %>% 
  group_by(pro, estrato) %>% 
  summarise(n_upm = n())
  
