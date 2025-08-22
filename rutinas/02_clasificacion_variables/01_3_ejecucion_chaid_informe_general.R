rm(list = ls())
gc()

library(tidyverse)
library(openxlsx)
library(rio)
library(rmarkdown)

upm_domest <- readRDS("intermedios/02_clasificacion_variables/upm_domest.rds")

#####

# load("productos/01_tratamiento/enemdu_variables_chaid.RData")
# 
# pobreza_01 <- pobreza %>% 
#   mutate(provincia = substr(id_vivienda,1,2),
#          id_canton = substr(id_vivienda,1,4),
#          domgeo = case_when(id_canton=="1701"  ~ "33",
#                             id_canton=="0901" & area==1  ~ "42",
#                             id_canton=="0101"  ~ "30",
#                             id_canton=="0701" & area==1  ~ "40",
#                             id_canton=="1801"  ~ "34",
#                             id_canton=="0801" & area==1  ~ "41",
#                             id_canton=="2301"  ~ "35",
#                             id_canton=="1308" & area==1 ~ "43",
#                             id_canton=="1101"  ~ "32",
#                             id_canton=="0601"  ~ "31",
#                             T  ~ provincia),
#          domest = paste0(domgeo,area),
#          ndomgeo = case_when(domgeo == "01" ~ "Provincia Azuay",
#                              domgeo == "02" ~ "Provincia Bolívar",
#                              domgeo == "03" ~ "Provincia Cañar",
#                              domgeo == "04" ~ "Provincia Carchi",
#                              domgeo == "05" ~ "Provincia Cotopaxi",
#                              domgeo == "06" ~ "Provincia Chimborazo",
#                              domgeo == "07" ~ "Provincia El Oro",
#                              domgeo == "08" ~ "Provincia Esmeraldas",
#                              domgeo == "09" ~ "Provincia Guayas",
#                              domgeo == "10" ~ "Provincia Imbabura",
#                              domgeo == "11" ~ "Provincia Loja",
#                              domgeo == "12" ~ "Provincia Los Ríos",
#                              domgeo == "13" ~ "Provincia Manabí",
#                              domgeo == "14" ~ "Provincia Morona Santiago",
#                              domgeo == "15" ~ "Provincia Napo",
#                              domgeo == "16" ~ "Provincia Pastaza",
#                              domgeo == "17" ~ "Provincia Pichincha",
#                              domgeo == "18" ~ "Provincia Tungurahua",
#                              domgeo == "19" ~ "Provincia Zamora Chinchipe",
#                              domgeo == "20" ~ "Provincia Galápagos",
#                              domgeo == "21" ~ "Provincia Sucumbíos",
#                              domgeo == "22" ~ "Provincia Orellana",
#                              domgeo == "23" ~ "Provincia Santo Domingo",
#                              domgeo == "24" ~ "Provincia Santa Elena",
#                              domgeo == "30" ~ "Cantón Cuenca",
#                              domgeo == "31" ~ "Cantón Riobamba",
#                              domgeo == "32" ~ "Cantón Loja",
#                              domgeo == "33" ~ "Cantón Quito",
#                              domgeo == "34" ~ "Cantón Ambato",
#                              domgeo == "35" ~ "Cantón Santo Domingo",
#                              domgeo == "40" ~ "Cantón Machala",
#                              domgeo == "41" ~ "Cantón Esmeraldas",
#                              domgeo == "42" ~ "Cantón Guayaquil",
#                              domgeo == "43" ~ "Cantón Manta",
#                              T ~ "dominio no definido"),
#          ndomest = case_when(domgeo %in% c("01", "06", "11", "17", "18", 
#                                            "23", "07", "08", "09", "13") & area == 1 ~ 
#                                paste0("Resto ", ndomgeo, " Urbano"),
#                              domgeo %in% c("01", "06", "11", "17", "18", "23") & area == 2 ~ 
#                                paste0("Resto ", ndomgeo, " Rural"),
#                              area == 1 ~ paste0(ndomgeo, " Urbano"),
#                              area == 2 ~ paste0(ndomgeo, " Rural"),
#                              T ~ "algo hicimos mal")) %>% 
# 
#   mutate(rnatura = case_when(provincia %in% c("01", "02", "03", "04", "05", "06",
#                                               "10", "11", "17", "18") ~ "Sierra",
#                              provincia %in% c("07", "08", "09", "12", "13", "23",
#                                               "24", "20") ~ "Costa",
#                              provincia %in% c("14", "15", "16", "19", "21", "22") 
#                              ~ "Amazonía",
#                              T ~ "no hay región"))
# 
# saveRDS(pobreza_01,
#         "intermedios/02_clasificacion_variables/enemdu_variables_chaid_domest.rds")
#####
pobreza <- readRDS("intermedios/02_clasificacion_variables/enemdu_variables_chaid_domest.rds")

pobreza_01 <- pobreza %>% 
  #filter(area == 1)
  #filter(area == 2)
  #filter(rnatura == "Sierra")
  #filter(rnatura == "Costa")
  #filter(rnatura == "Amazonía")
  #filter(rnatura == "Sierra" & area == 1)
  #filter(rnatura == "Sierra" & area == 2)
  #filter(rnatura == "Costa" & area == 1)
  filter(rnatura == "Costa" & area == 2)
  #filter(rnatura == "Amazonía" & area == 1)
  #filter(rnatura == "Amazonía" & area == 2)


# chaid

# nombre_domest <- "Nacional Urbano" # 141342
# domestrato <- "02_Urbano"
# nombre_domest <- "Nacional Rural" # 141342
# domestrato <- "03_Rural"
# nombre_domest <- "Región Natural Sierra" # 161679
# domestrato <- "04_Sierra"
# nombre_domest <- "Región Natural Costa" # 141342
# domestrato <- "05_Costa"# nombre_domest <- "Región Natural Costa" # 141342
# domestrato <- "05_Costa"
# nombre_domest <- "Región Natural Amazonía" # 141342
# domestrato <- "06_Amazonía"
# nombre_domest <- "Región Natural Sierra Urbano" # 141342
# domestrato <- "07_Sierra_Urbano"
# nombre_domest <- "Región Natural Sierra Rural" # 141342
# domestrato <- "08_Sierra_Rural"
# nombre_domest <- "Región Natural Costa Urbano" # 141342
# domestrato <- "09_Costa_Urbano"
nombre_domest <- "Región Natural Costa Rural" # 141342
domestrato <- "10_Costa_Rural"
# nombre_domest <- "Región Natural Amazonía Urbano" # 141342
# domestrato <- "11_Amazonía_Urbano"
# nombre_domest <- "Región Natural Amazonía Rural" # 141342
# domestrato <- "12_Amazonía_Rural"



  rmarkdown::render(input = "rutinas/02_clasificacion_variables/55_chaid_informe.Rmd",
                    output_format = "html_document",
                    output_file = paste0(domestrato,"_chaid"),
                    output_dir = "intermedios/02_clasificacion_variables/html_general/",
                    clean = T,
                    #params = list(subpoblacion = indice[i])
  )
