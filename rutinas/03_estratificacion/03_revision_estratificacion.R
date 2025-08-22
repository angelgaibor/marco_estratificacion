rm(list = ls())

library(tidyverse)
source("rutinas/03_estratificacion/funciones.R", encoding = "UTF-8")
# Juntar la estratificación de todos los domest

upm_domest <- readRDS("intermedios/02_clasificacion_variables/upm_domest.rds") %>% 
  filter(n_estratos > 1)

index <- upm_domest$domest

estratos <- vector("list", 0)

for(i in 1 : length(index)){
  estratos[[i]] <- readRDS(paste0("intermedios/03_estratificacion/estratos/", index[i], ".rds")) %>% 
    mutate(domest = index[i])
}

estratos_ini <- do.call(rbind, estratos)

# Determinación si el escenario 1 o 2 es el elegido
apoyo_escenario <- estratos_ini %>% 
  group_by(domest, edg_e1, edg_e2) %>% 
  summarise() %>% 
  ungroup() %>% 
  summarise(edg_e1 = sum(edg_e1),
            edg_e2 = sum(edg_e2))
# Dado que el efecto de diseño generalizado total del escenario 1 es mejor que el
# del escenario 2 se elige el primer escenario para la estratificación

# Analizar si se tiene el número de upm adecuado por estrato
apoyo_n_upm <- estratos_ini %>% 
  group_by(domest, estrato = est_e1) %>% 
  summarise(n_upm = n()) %>% 
  mutate(control = ifelse(n_upm < 100, 1, 0)) %>% 
  group_by(domest) %>% 
  summarise(control = max(control)) %>% 
  ungroup() %>% 
  filter(control == 1)

# Actualizamos los domest donde se debe volver a estratificar

upm_domest_01 <- upm_domest %>% 
  mutate(control = ifelse(domest %in% apoyo_n_upm$domest, 1, 0),
         n_estratos_01 = n_estratos - control)

saveRDS(upm_domest_01, "productos/03_estratificacion/upm_estratos_final.rds")

# Se excluye el domest 041 ya que al reestratificar no es adecuado tener dos estratos ya que
# se distribuye como 1: 84 y 2 :298 por lo que se decide colapsar el estrato 1 y 2 de la
# estratificación a 3 estratos
indice <- upm_domest_01$domest[upm_domest_01$n_estratos != upm_domest_01$n_estratos_01 & 
                                upm_domest_01$n_estratos_01 > 1]

for (j in 1:length(indice)){ 
  
  n_est <- upm_domest_01$n_estratos_01[upm_domest$domest == indice[j]]
  
  indicadores <- readRDS(paste0("intermedios/03_estratificacion/indicadores/",indice[j],".rds")) 
  load(paste0("productos/02_clasificacion_variables/",indice[j],".RData"))
  
  rmarkdown::render(input = "rutinas/03_estratificacion/55_rmarkdown_estratificacion.Rmd",
                    output_format = "html_document",
                    output_file = indice[j],
                    output_dir = "productos/03_estratificacion/html",
                    clean = T,
                    params = list(subpoblacion = indice[j])
  )
  saveRDS(auxiliar, file=paste0("productos/03_estratificacion/estratos/", indice[j],".rds"))
  
  print(indice[j])
  
}


estratos_01 <- vector("list", 0)

for(i in 1 : length(indice)){
  estratos_01[[i]] <- readRDS(paste0("productos/03_estratificacion/estratos/", indice[i], ".rds")) %>% 
    mutate(domest = indice[i])
}

estratos_ini_01 <- do.call(rbind, estratos_01)

# Dado que el efecto de diseño generalizado total del escenario 1 es mejor que el
# del escenario 2 se elige el primer escenario para la estratificación

# Analizar si se tiene el número de upm adecuado por estrato
apoyo_n_upm_01 <- estratos_ini_01 %>% 
  group_by(domest, estrato = est_e1) %>% 
  summarise(n_upm = n()) %>% 
  mutate(control = ifelse(n_upm < 100, 1, 0)) %>% 
  group_by(domest) %>% 
  summarise(control = max(control)) %>% 
  ungroup() %>% 
  filter(control == 1)

# Abrimos la estratificación inicial del 041
est_041 <- readRDS(paste0("intermedios/03_estratificacion/estratos/", 
                                               "041", ".rds")) %>% 
  mutate(est_e1 = case_when(est_e1 == 2 ~ 1, 
                            est_e1 == 3 ~ 2,
                            est_e1 == 1 ~ 1))

saveRDS(est_041, file=paste0("productos/03_estratificacion/estratos/", "041",".rds"))


indice <- upm_domest_01$domest[upm_domest_01$n_estratos == upm_domest_01$n_estratos_01 & 
                                 upm_domest_01$n_estratos_01 > 1]

for (i in 1: length(indice)){
  file.copy(from = paste0("intermedios/03_estratificacion/html/", indice[i], ".html"), 
            to = paste0("productos/03_estratificacion/html/", indice[i], ".html"))
  
  file.copy(from = paste0("intermedios/03_estratificacion/estratos/", indice[i], ".rds"), 
            to = paste0("productos/03_estratificacion/estratos/", indice[i], ".rds"))
}

