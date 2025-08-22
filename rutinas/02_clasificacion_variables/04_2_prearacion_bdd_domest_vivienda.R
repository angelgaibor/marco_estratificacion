# Librerías ---------------------------------------------------------------
rm(list=ls())
gc()
library(rio)
library(janitor)
library(tidyverse)

# Lectura de bases --------------------------------------------------------

viviendas1 <- import("productos/01_tratamiento/marco_viv.rds") %>% 
  mutate(id_canton = paste0(provincia,canton),
         domgeo = case_when(id_canton == "0101"  ~ "30",
                            id_canton == "0601"  ~ "31",
                            id_canton == "1101"  ~ "32",
                            id_canton == "1701"  ~ "33",
                            id_canton == "1801"  ~ "34",
                            id_canton == "0701" & area == 1  ~ "40",
                            id_canton == "0801" & area == 1  ~ "41",
                            id_canton == "0901" & area == 1  ~ "42",
                            id_canton == "1308" & area == 1 ~ "43",
                            id_canton == "2301" & area == 1 ~ "44",
                            T  ~ provincia),
         domest = paste0(domgeo,area))

hogares1 <- import("intermedios/02_clasificacion_variables/hogares_var_categorias.rds")
personas1 <- import("intermedios/02_clasificacion_variables/personas_var_categorias.rds")

upm_domest <- readRDS("intermedios/02_clasificacion_variables/upm_domest.rds") |> 
  filter(n_estratos > 1)

domest1 <- unique(upm_domest$domest)


for (i in 1:length(domest1)) {

  viviendas <- viviendas1 %>% 
    filter(domest==domest1[i])

  hogares_condicion <- hogares1 %>% 
    filter(domest==domest1[i])
  
  personas_condicion <- personas1 %>% 
    filter(domest==domest1[i])
  
  domin <- readRDS("intermedios/02_clasificacion_variables/variables_categorias_final.rds") %>% 
    filter(bienestar == 1, domest == domest1[i])
  
  tebien <- domin$categoria[domin$variable == "techo"]
  pabien <- domin$categoria[domin$variable == "paredes"]
  pibien <- domin$categoria[domin$variable == "piso"]
  r_bien <- domin$categoria[domin$variable == "r_agua"]
  p_bien <- domin$categoria[domin$variable == "p_agua"]
  cxbien <- domin$categoria[domin$variable == "cx_sh"]
  
  vivienda_condicion <- viviendas %>% 
    # Dimensión 1: Características de la vivienda
    mutate( 
      # Material de techo v03: El material predominante del techo o cubierta de la vivienda es de?:
      techo=ifelse(v03 %in% tebien & v04==1,1, 0),
      # Materiales de paredes exteriores V03: El material predominante de las paredes exteriores de la vivienda es de?:
      paredes=ifelse(v05 %in% pabien & v06==1,1, 0),
      # Materiales del piso V05: El material predominante del piso de la vivienda es de?:
      piso=ifelse(v07 %in% pibien & v08==1,1, 0),
      
      # Dimensión 2: Acceso a servicios básicos de la vivienda 
      # ¿El agua que recibe la vivienda es? por tubería
      r_agua=ifelse(v09 %in% r_bien,1,0),
      # ¿El agua que recibe la vivienda proviene o es suministrada por?
      p_agua=ifelse(v10 %in% p_bien,1, 0),
      # Servicio higiénico de la vivienda? El servicio higiénico o escusado de la vivienda es:     
      cx_sh=ifelse(v11 %in% cxbien, 1, 0 ), 
      # Servicio de luz de la vivienda? El servicio de luz (energía) eléctrica de la vivienda proviene principalmente de:     
      luz=ifelse(v12 == 1 | v13 %in% c(1:4) , 1, 0 ), 
      # Eliminación de la basura de la vivienda? V14. Principalmente ?Cómo se elimina la basura de la vivienda:     
      basura=ifelse(v14 %in% c(1,2), 1, 0)
    )
  
  # Eliminacion de variables
  names <- c("v01","v0201","v03","v04","v05","v06","v07","v08", "v10","v09","v11","v12","v13","v14")
  
  vivienda_condicion <- vivienda_condicion |> 
    select(!all_of(names))
  
  #Guardo la data de vivienda con los 1 y 0
  save(vivienda_condicion,hogares_condicion,personas_condicion, file= paste0("productos/02_clasificacion_variables/",domest1[i],".RData"))
  print(domest1[i])
}







