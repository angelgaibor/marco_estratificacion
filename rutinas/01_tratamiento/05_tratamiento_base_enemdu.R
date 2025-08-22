rm(list = ls())
gc()

library(foreign)
library(tidyverse)
library(rio)
library(haven)

# Relacion de las variables que se utilizaran en la estratificacion
# con la pobreza por ingresos

# fexp por hogar integrado ------------------------------------------------

viviendas <- readRDS("insumos/01_tratamiento/enemdu/enemdud_viviendas_2023_publicada.rds") %>% 
  select(-c(area,ciudad,conglomerado,panelm,
            vivienda,hogar,estrato,fexp,upm,mes))

personas <- readRDS("insumos/01_tratamiento/enemdu/enemdud_personas_2023_publicada.rds") 

base <- import("insumos/01_tratamiento/enemdu/fexp_hog_int_enemdu_2023.rds") %>% 
  mutate(id_vivienda = paste0(id_upm,panelm,vivienda,mes),
         id_hogar = paste0(id_upm,panelm,vivienda,hogar,mes),
         id_persona = paste0(id_upm, panelm, vivienda, hogar, persona = p01,mes))

table(nchar(base$id_persona))

personas <- left_join(personas, base %>% select(id_persona, fexp_cal_hog), by = "id_persona")

data <- full_join(viviendas,personas %>% select(-c(id_vivienda,periodo)),by="id_hogar")

#Identificación de los NA en la variable dependiente pobreza.
table(data$pobreza, useNA = "ifany")

#Identificación de los NA en la variable dependiente desempleo
table(data$desempleo, useNA = "ifany")


# Reservamos los NA, para predecir la clasificación
data <- data %>% 
  select(area,id_vivienda,id_hogar,
         p36,desempleo,condact,empleo,
         pobreza,vi03a, vi03b,vi05a,vi05b,
         vi04a, vi04b,vi10,vi10a,
         vi09,vi12,vi13,vi07b,
         vi08,vi11,p05a,
         p05b,p15,
         vi07,
         p10a,p10b,p03,p07,
         p11,
         p20,p21,
         p02,fexp_cal_hog, p04) %>% 
  group_by(id_hogar) %>% 
  mutate(total=n(),
         hacinam = ifelse(vi07==0 & total %in% c(1,2),1,
                          ifelse(vi07==0 & total>2, 0,
                                 ifelse(total/vi07>3, 0, 1))),
         nini= ifelse((p07==2 & p20==2),1, 0 ),
         id_hm  = ifelse(p02==2 & p04==1, 1, 
                         ifelse(p04==2, 2, ifelse(p04==3,3,0))),
         n_jefes_m= sum(id_hm==1,na.rm = T),
         n_conyugues= sum(id_hm==2,na.rm = T),
         n_hijos= sum(id_hm==3,na.rm = T),
         id_hmuj= ifelse(n_jefes_m==1 & n_conyugues==0 & n_hijos>=1,1,0),
         ta_juventud= ifelse(p03 >= 15 & p03 <=29, 1, 0),
         ta_envejecimiento=ifelse(p03 >= 65, 1, 0),
         nm5a= ifelse(p03<=5, 1, 0),
         mer= ifelse(p02==2 & p03<=49 & p03>=15, 1, 0),
         tn5= sum(nm5a, na.rm=T),
         tm15_49= sum(mer, na.rm = T),
         ni_mj= ifelse(is.nan(tn5/tm15_49) | is.infinite(tn5/tm15_49), 0, round(tn5/tm15_49,2)),
         pob15a65 = ifelse((p03 >= 15 & p03 <= 65), 1, 0), 
         pob15_65 = ifelse((p03 < 15 | p03 > 65), 1, 0),
         SUM_pob15a65= sum(pob15a65, na.rm = T),
         SUM_pob15_65= sum(pob15_65, na.rm = T),
         depen =  round(SUM_pob15_65/SUM_pob15a65,2))



data <- data %>%
  mutate(a_escola = case_when(
    p10a==1 & p03>=5 ~ 0,
    p10a==2 & p10b==1 & p07==2 & p03>=5~ 3,
    p10a==2 & p10b==2 & p07==1 & p03>=5~ 3,
    p10a==2 & p10b==2 & p07==2 & p03>=5~ 5,
    p10a==2 & p10b==3 & p07==1 & p03>=5~ 5,
    p10a==2 & p10b==3 & p07==2 & p03>=5~ 7,
    p10a==4 & p10b==1 & p07==1 & p03>=5~ 1,
    p10a==4 & p10b==1 & p07==2 & p03>=5~ 2,
    p10a==4 & p10b==2 & p07==1 & p03>=5~ 2,
    p10a==4 & p10b==2 & p07==2 & p03>=5~ 3,
    p10a==4 & p10b==3 & p07==1 & p03>=5~ 3,
    p10a==4 & p10b==3 & p07==2 & p03>=5~ 4,
    p10a==4 & p10b==4 & p07==1 & p03>=5~ 4,
    p10a==4 & p10b==4 & p07==2 & p03>=5~ 5,
    p10a==4 & p10b==5 & p07==1 & p03>=5~ 5,
    p10a==4 & p10b==5 & p07==2 & p03>=5~ 6,
    p10a==4 & p10b==6 & p07==1 & p03>=5~ 6,
    p10a==4 & p10b==6 & p07==2 & p03>=5~ 7,
    p10a==6 & p10b==1 & p07==1 & p03>=5~ 7,
    p10a==6 & p10b==1 & p07==2 & p03>=5~ 8,
    p10a==6 & p10b==2 & p07==1 & p03>=5~ 8,
    p10a==6 & p10b==2 & p07==2 & p03>=5~ 9,
    p10a==6 & p10b==3 & p07==1 & p03>=5~ 9,
    p10a==6 & p10b==3 & p07==2 & p03>=5~ 10,
    p10a==6 & p10b==4 & p07==1 & p03>=5~ 10,
    p10a==6 & p10b==4 & p07==2 & p03>=5~ 11,
    p10a==6 & p10b==5 & p07==1 & p03>=5~ 11,
    p10a==6 & p10b==5 & p07==2 & p03>=5~ 12,
    p10a==6 & p10b==6 & p07==1 & p03>=5~ 12,
    p10a==6 & p10b==6 & p07==2 & p03>=5~ 13,
    p10a==5 & p10b==1 & p07==2 & p03>=5~ 1,
    p10a==5 & p10b==2 & p07==1 & p03>=5~ 1,
    p10a==5 & p10b==2 & p07==2 & p03>=5~ 2,
    p10a==5 & p10b==3 & p07==1 & p03>=5~ 2,
    p10a==5 & p10b==3 & p07==2 & p03>=5~ 3,
    p10a==5 & p10b==4 & p07==1 & p03>=5~ 3,
    p10a==5 & p10b==4 & p07==2 & p03>=5~ 4,
    p10a==5 & p10b==5 & p07==1 & p03>=5~ 4,
    p10a==5 & p10b==5 & p07==2 & p03>=5~ 5,
    p10a==5 & p10b==6 & p07==1 & p03>=5~ 5,
    p10a==5 & p10b==6 & p07==2 & p03>=5~ 6,
    p10a==5 & p10b==7 & p07==1 & p03>=5~ 6,
    p10a==5 & p10b==7 & p07==2 & p03>=5~ 7,
    p10a==5 & p10b==8 & p07==1 & p03>=5~ 7,
    p10a==5 & p10b==8 & p07==2 & p03>=5~ 8,
    p10a==5 & p10b==9 & p07==1 & p03>=5~ 8,
    p10a==5 & p10b==9 & p07==2 & p03>=5~ 9,
    p10a==5 & p10b==10 & p07==1 & p03>=5~ 9,
    p10a==5 & p10b==10 & p07==2 & p03>=5~ 10,
    p10a==7 & p10b==1 & p07==1 & p03>=5~ 10, 
    p10a==7 & p10b==1 & p07==2 & p03>=5~ 11,
    p10a==7 & p10b==2 & p07==1 & p03>=5~ 11,
    p10a==7 & p10b==2 & p07==2 & p03>=5~ 12,
    p10a==7 & p10b==3 & p07==1 & p03>=5~ 12,
    p10a==7 & p10b==3 & p07==2 & p03>=5~ 13,
    p10a==8 & p10b==1 & p07==1 & p03>=5~ 13,
    p10a==8 & p10b==1 & p07==2 & p03>=5~ 14,
    p10a==8 & p10b==2 & p07==1 & p03>=5~ 14,
    p10a==8 & p10b==2 & p07==2 & p03>=5~ 15,
    p10a==8 & p10b==3 & p07==1 & p03>=5~ 15,
    p10a==8 & p10b==3 & p07==2 & p03>=5~ 16,
    p10a==9 & p10b==1 & p07==1 & p03>=5~ 13,
    p10a==9 & p10b==1 & p07==2 & p03>=5~ 14,
    p10a==9 & p10b==2 & p07==1 & p03>=5~ 14,
    p10a==9 & p10b==2 & p07==2 & p03>=5~ 15,
    p10a==9 & p10b==3 & p07==1 & p03>=5~ 15,
    p10a==9 & p10b==3 & p07==2 & p03>=5~ 16,
    p10a==9 & p10b==4 & p07==1 & p03>=5~ 16,
    p10a==9 & p10b==4 & p07==2 & p03>=5~ 17,
    p10a==9 & p10b==5 & p07==1 & p03>=5~ 17,
    p10a==9 & p10b==5 & p07==2 & p03>=5~ 18,
    p10a==9 & p10b==6 & p07==1 & p03>=5~ 18,
    p10a==9 & p10b==6 & p07==2 & p03>=5~ 19,
    p10a==9 & p10b==7 & p07==1 & p03>=5~ 19,
    p10a==9 & p10b==7 & p07==2 & p03>=5~ 20,
    p10a==9 & p10b==8 & p07==1 & p03>=5~ 20,
    p10a==9 & p10b==8 & p07==2 & p03>=5~ 21,
    p10a==10 & p10b==1 & p07==1 & p03>=5~ 18,
    p10a==10 & p10b==1 & p07==2 & p03>=5~ 19,
    p10a==10 & p10b==2 & p07==1 & p03>=5~ 19,
    p10a==10 & p10b==2 & p07==2 & p03>=5~ 20,
    p10a==10 & p10b==3 & p07==1 & p03>=5~ 20,
    p10a==10 & p10b==3 & p07==2 & p03>=5~ 21,
    p10a==10 & p10b==4 & p07==1 & p03>=5~ 21,
    p10a==10 & p10b==4 & p07==2 & p03>=5~ 22,
    p10a==10 & p10b==5 & p07==1 & p03>=5~ 22,
    p10a==10 & p10b==5 & p07==2 & p03>=5~ 23,
    p10a==10 & p10b==6 & p07==1 & p03>=5~ 23,                        
    p10a==10 & p10b==6 & p07==2 & p03>=5~ 24,
    p10a==99 & p03>=5 ~ 99), 
    a_escola = ifelse(is.na(a_escola), 0, a_escola))

summary(data$a_escola)

data <- data %>%
  mutate(esc_ea=ifelse(a_escola > 13,1,0))

pobreza <- data %>% 
  filter(!is.na(pobreza))


names(pobreza) <- c("area","id_vivienda","id_hogar","cond_inac","desempleo",
                    "condact","empleo","pobreza", "techo", "estado_techo", 
                    "paredes","estado_paredes","piso", "estado_piso","p_agua", 
                    "r_agua","tipo_sh", "luz", "basura", "exc_cocina",
                    "combustible","exc_ducha","seguros_1","seguros_2", "autoidentifica",
                    "dormitorios", "nivel_instruccion", "año_aprobado","edad","asiste_clases",
                    "leer_escribir","trabajo","actividad","sexo","fexp_cal_hog",
                    "parentesco","per_hog","hacinam","nini","id_hm",
                    "n_jefes_m","n_conyugues","n_hijos", "id_hmuj","ta_juventud",
                    "ta_envejecimiento","nm5a","mer","tn5","tm15_49",
                    "ni_mj","pob15a65","pob15_65","SUM_pob15a65","SUM_pob15_65",
                    "depen","a_escola","esc_ea")

pobreza <- pobreza %>% 
  mutate(across(!c(fexp_cal_hog,edad,id_vivienda,depen,ni_mj), as.factor))

summary(pobreza)

# Guardar la base de datos
save(pobreza, file = "productos/01_tratamiento/enemdu_variables_chaid.RData")
