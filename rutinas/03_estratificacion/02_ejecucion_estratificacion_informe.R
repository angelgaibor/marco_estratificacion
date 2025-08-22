rm(list = ls())
gc()

library(tidyverse)
library(openxlsx)
library(rio)
library(rmarkdown)

source("rutinas/03_estratificacion/funciones.R", encoding = "UTF-8")

upm_domest <- readRDS("intermedios/02_clasificacion_variables/upm_domest.rds") |> 
  filter(n_estratos > 1)

indice <- unique(upm_domest$domest)

for (j in 1:length(indice)){ 
  
  n_est <- upm_domest$n_estratos[upm_domest$domest == indice[j]]
  
  indicadores <- readRDS(paste0("intermedios/03_estratificacion/indicadores/",indice[j],".rds")) 
  load(paste0("productos/02_clasificacion_variables/",indice[j],".RData"))
  
  rmarkdown::render(input = "rutinas/03_estratificacion/55_rmarkdown_estratificacion.Rmd",
                    output_format = "html_document",
                    output_file = indice[j],
                    output_dir = "intermedios/03_estratificacion/html",
                    clean = T,
                    params = list(subpoblacion = indice[j])
                    )
  saveRDS(auxiliar, file=paste0("intermedios/03_estratificacion/estratos/", indice[j],".rds"))
  
  print(indice[j])
  
}
