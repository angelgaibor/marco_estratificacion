rm(list = ls())

library(tidyverse)
library(openxlsx)

# Archivo rafita
domestrato_re <- getSheetNames("intermedios/02_clasificacion_variables/value_variables_domest_re_01.xlsx")

clasificacion_re <- vector("list", 0)

for(i in 1:length(domestrato_re)){
  clasificacion_re[[i]] <- read.xlsx("intermedios/02_clasificacion_variables/value_variables_domest_re_01.xlsx",
                                     sheet = i) %>% 
    mutate(domest = domestrato_re[i]) %>% 
    select(domest, variable, categoria, bienestar)
}

clasificacion_01 <- do.call(rbind, clasificacion_re)

domestrato_ag <- getSheetNames("intermedios/02_clasificacion_variables/value_variables_domest_ag_01.xlsx")

clasificacion_ag <- vector("list", 0)

for(i in 1:length(domestrato_ag)){
  clasificacion_ag[[i]] <- read.xlsx("intermedios/02_clasificacion_variables/value_variables_domest_ag_01.xlsx",
                                     sheet = i) %>% 
    mutate(domest = domestrato_ag[i]) %>% 
    select(domest, variable, categoria, bienestar)
}

clasificacion_02 <- do.call(rbind, clasificacion_ag)

domestrato_wc <- getSheetNames("intermedios/02_clasificacion_variables/categorias_dominio_will.xlsx")

clasificacion_wc <- vector("list", 0)

for(i in 1:length(domestrato_wc)){
  clasificacion_wc[[i]] <- read.xlsx("intermedios/02_clasificacion_variables/categorias_dominio_will.xlsx",
                                     sheet = i) %>% 
    mutate(domest = domestrato_wc[i]) %>% 
    select(domest, variable, categoria, bien_old = bienestar) %>% 
    filter(variable %in% c("techo", "piso", "paredes", "p_agua", "r_agua", "cx_sh"))
}

clasificacion_old <- do.call(rbind, clasificacion_wc)

clasificacion_new <- rbind(clasificacion_01, clasificacion_02) %>%
  # La parte rural del cantón santo domingo se junta con el resto rural de la
  # provincia santo domingo ya que el número de upm en el domestrato 232 es 
  # muy pequeño por lo tanto santo domingo canto pasa a ser un dominio unicamente
  # urbano por lo que su código pasa de 35 a 44
  filter(domest != "232") %>% 
  mutate(domest = case_when(domest == "352" ~ "232",
                            domest == "351" ~ "441",
                            T ~ domest)) %>% 
  # Se determinó que en todas las variables al menos una categoría está asociada con bienestar
  mutate(bienestar = case_when(categoria == 1 ~ 1,
                               # Se generaliza que el sh inodoro con biodigestor es bienestar y sh inodoro 
                               # con pozo séptico en el área urbana no es bienestar
                               variable == "cx_sh" & categoria == 3 ~ 1,
                               variable == "cx_sh" & categoria == 2 & substr(domest, 3, 3) == 1 ~ 0,
                               T ~ bienestar))

control <- clasificacion_new %>% 
  left_join(clasificacion_old, by = c("domest", "variable", "categoria")) %>% 
  mutate(bien_old = ifelse(is.na(bien_old), 0, bien_old),
         control = abs(bienestar - bien_old)) %>% 
  group_by(domest) %>% 
  summarise(control = sum(control, na.rm = T))

saveRDS(clasificacion_new, "intermedios/02_clasificacion_variables/variables_categorias_final.rds")

# upm_domest <- readRDS("intermedios/02_clasificacion_variables/upm_domest.rds") %>% 
#   mutate(domest = case_when(domest == "352" ~ "232",
#                             domest == "351" ~ "441",
#                             T ~ domest)) %>% 
#   group_by(domest) %>% 
#   summarise(n = sum(n),
#             n_estratos = max(n_estratos))
# 
# saveRDS(upm_domest, "intermedios/02_clasificacion_variables/upm_domest.rds")
