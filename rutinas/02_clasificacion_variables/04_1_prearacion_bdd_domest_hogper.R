rm(list=ls())

library(rio)
library(tidyverse)

hogares <- readRDS("productos/01_tratamiento/marco_hog.rds")
personas <- import("productos/01_tratamiento/marco_per.rds") 

personas <- personas %>%
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

hogares <- hogares %>%
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


# Hogares -----------------------------------------------------------------

# Número de personas en el hogar
n_personas <- personas %>% group_by(id_hogar) %>% 
  summarise(N = n())

hogares <- hogares %>% left_join(n_personas, by = "id_hogar")

summary(hogares$N)

#Creando el año de escolaridad
personas <- personas %>%
  mutate(a_escola = case_when(
    p17==1 & p03>=5 ~ 0,
    # niv instrucc # grado   #asiste # edad
    p17==5 & p18==1 & p15==1 & p03>=5~ 1,
    p17==6 & p18==1 & p15==2 & p03>=5~ 1,
    p17==6 & p18==2 & p15==1 & p03>=5~ 1,
    p17==5 & p18==1 & p15==2 & p03>=5~ 2,
    p17==5 & p18==2 & p15==1 & p03>=5~ 2,
    p17==6 & p18==2 & p15==2 & p03>=5~ 2,
    p17==6 & p18==3 & p15==1 & p03>=5~ 2,
    p17==5 & p18==1 & p15==2 & p03>=5~ 3,
    p17==5 & p18==2 & p15==1 & p03>=5~ 3,
    p17==5 & p18==2 & p15==2 & p03>=5~ 3,
    p17==5 & p18==3 & p15==1 & p03>=5~ 3,
    p17==6 & p18==3 & p15==2 & p03>=5~ 3,
    p17==6 & p18==4 & p15==1 & p03>=5~ 3,
    p17==5 & p18==3 & p15==2 & p03>=5~ 4,
    p17==5 & p18==4 & p15==1 & p03>=5~ 4,
    p17==6 & p18==4 & p15==2 & p03>=5~ 4,
    p17==6 & p18==5 & p15==1 & p03>=5~ 4,
    p17==5 & p18==2 & p15==2 & p03>=5~ 5,
    p17==5 & p18==3 & p15==1 & p03>=5~ 5,
    p17==5 & p18==4 & p15==2 & p03>=5~ 5,
    p17==5 & p18==5 & p15==1 & p03>=5~ 5,
    p17==6 & p18==5 & p15==2 & p03>=5~ 5,
    p17==6 & p18==6 & p15==1 & p03>=5~ 5,
    p17==5 & p18==5 & p15==2 & p03>=5~ 6,
    p17==5 & p18==6 & p15==1 & p03>=5~ 6,
    p17==6 & p18==6 & p15==2 & p03>=5~ 6,
    p17==6 & p18==7 & p15==1 & p03>=5~ 6,
    p17==5 & p18==3 & p15==2 & p03>=5~ 7,
    p17==5 & p18==6 & p15==2 & p03>=5~ 7,
    p17==5 & p18==1 & p15==1 & p03>=5~ 7,
    p17==6 & p18==7 & p15==2 & p03>=5~ 7,
    p17==6 & p18==8 & p15==1 & p03>=5~ 7,
    p17==6 & p18==8 & p15==2 & p03>=5~ 8,
    p17==6 & p18==9 & p15==1 & p03>=5~ 8,
    p17==7 & p18==1 & p15==2 & p03>=5~ 8,
    p17==7 & p18==2 & p15==1 & p03>=5~ 8,
    p17==7 & p18==2 & p15==2 & p03>=5~ 9,
    p17==7 & p18==3 & p15==1 & p03>=5~ 9,
    p17==6 & p18==9 & p15==2 & p03>=5~ 9,
    p17==6 & p18==10 & p15==1 & p03>=5~ 9,
    p17==7 & p18==3 & p15==2 & p03>=5~ 10,
    p17==7 & p18==4 & p15==1 & p03>=5~ 10,
    p17==6 & p18==10 & p15==2 & p03>=5~ 10,
    p17==8 & p18==1 & p15==1 & p03>=5~ 10, 
    p17==7 & p18==4 & p15==2 & p03>=5~ 11,
    p17==7 & p18==5 & p15==1 & p03>=5~ 11,
    p17==8 & p18==1 & p15==2 & p03>=5~ 11,
    p17==8 & p18==2 & p15==1 & p03>=5~ 11,
    p17==7 & p18==5 & p15==2 & p03>=5~ 12,
    p17==7 & p18==6 & p15==1 & p03>=5~ 12,
    p17==8 & p18==2 & p15==2 & p03>=5~ 12,
    p17==8 & p18==3 & p15==1 & p03>=5~ 12,
    p17==7 & p18==6 & p15==2 & p03>=5~ 13,
    p17==8 & p18==3 & p15==2 & p03>=5~ 13,
    p17==9 & p18==1 & p15==1 & p03>=5~ 13,
    p17==11 & p18==1 & p15==1 & p03>=5~ 13,
    p17==9 & p18==1 & p15==2 & p03>=5~ 14,
    p17==9 & p18==2 & p15==1 & p03>=5~ 14,
    p17==11 & p18==1 & p15==2 & p03>=5~ 14,
    p17==11 & p18==2 & p15==1 & p03>=5~ 14,
    p17==9 & p18==2 & p15==2 & p03>=5~ 15,
    p17==9 & p18==3 & p15==1 & p03>=5~ 15,
    p17==11 & p18==2 & p15==2 & p03>=5~ 15,
    p17==11 & p18==3 & p15==1 & p03>=5~ 15,
    p17==9 & p18==3 & p15==2 & p03>=5~ 16,
    p17==11 & p18==3 & p15==2 & p03>=5~ 16,
    p17==11 & p18==4 & p15==1 & p03>=5~ 16,
    p17==11 & p18==4 & p15==2 & p03>=5~ 17,
    p17==11 & p18==5 & p15==1 & p03>=5~ 17,
    p17==11 & p18==5 & p15==2 & p03>=5~ 18,
    p17==11 & p18==6 & p15==1 & p03>=5~ 18,
    p17==12 & p18==1 & p15==1 & p03>=5~ 18,
    p17==11 & p18==6 & p15==2 & p03>=5~ 19,
    p17==11 & p18==7 & p15==1 & p03>=5~ 19,
    p17==12 & p18==1 & p15==2 & p03>=5~ 19,
    p17==12 & p18==2 & p15==1 & p03>=5~ 19,
    p17==11 & p18==7 & p15==2 & p03>=5~ 20,
    p17==11 & p18==8 & p15==1 & p03>=5~ 20,
    p17==12 & p18==2 & p15==2 & p03>=5~ 20,
    p17==12 & p18==3 & p15==1 & p03>=5~ 20,
    p17==11 & p18==8 & p15==2 & p03>=5~ 21,
    p17==12 & p18==3 & p15==2 & p03>=5~ 21,
    p17==12 & p18==4 & p15==1 & p03>=5~ 21,
    p17==12 & p18==4 & p15==2 & p03>=5~ 22,
    p17==12 & p18==5 & p15==1 & p03>=5~ 22,
    p17==12 & p18==5 & p15==2 & p03>=5~ 23,
    p17==12 & p18==6 & p15==1 & p03>=5~ 23,                        
    p17==12 & p18==6 & p15==2 & p03>=5~ 24,
    p17==13 & p18==2 & p15==2 & p03>=5~ 20,
    p17==13 & p18==3 & p15==1 & p03>=5~ 20,
    p17==13 & p18==8 & p15==2 & p03>=5~ 21,
    p17==13 & p18==3 & p15==2 & p03>=5~ 21,
    p17==13 & p18==4 & p15==1 & p03>=5~ 21,
    p17==13 & p18==4 & p15==2 & p03>=5~ 22,
    p17==13 & p18==5 & p15==1 & p03>=5~ 22,
    p17==13 & p18==5 & p15==2 & p03>=5~ 23,
    p17==13 & p18==6 & p15==1 & p03>=5~ 23,                        
    p17==13 & p18==6 & p15==2 & p03>=5~ 24,
    p18==99 & p03>=5 ~ 99), 
    a_escola= ifelse(is.na(a_escola), 0, a_escola))

# Dimensión 3: Condición del hogar ----------------------------------------
# estas líneas son para considerar únicamente los hogares con niños que vivan con
# solo con papá o mamá
aux <- personas %>% 
  mutate(hijo = ifelse(p01==3,1,0)) %>% 
  group_by(id_hogar) %>% 
  summarise(hijo2 = sum(hijo, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hijo = ifelse(hijo2>0,1,NA)) %>% 
  select(-hijo2)
table(aux$hijo, useNA = "ifany")

# Dimensión 3: Condición del hogar ----------------------------------------

hogares_condicion <- hogares |> 
  left_join(personas %>% select(id_hogar,p01) %>% 
              mutate(no_monoparental = ifelse(p01==1 | p01==2, 1, 0 )) %>% #identificación del jefe de hogar con conyuge
              group_by(id_hogar) %>% 
              summarise(no_monoparental= sum(no_monoparental, na.rm = T)),
            by= "id_hogar") |> 
  left_join(aux, by="id_hogar") %>% 
  mutate( 
    # No monoparental
    no_monoparental= ifelse(hijo==1 & no_monoparental==2, 1, 
                            ifelse(hijo==1 & no_monoparental ==1,0,NA)),
    # No hacinamiento H01: numero de cuartos exclusivos para dormir
    no_hacin = ifelse(h01==0 & N %in% c(1,2),1,
                      ifelse(h01==0 & N>2, 0,
                             ifelse(N/h01>3, 0, 1))),
    # Agua segura
    agua_seg = ifelse(h06 %in% c(2,3,4,5), 1, 0)) %>% 
  select(-hijo)

table(hogares_condicion$no_monoparental, useNA = "ifany")
table(hogares_condicion$no_hacin, useNA = "ifany")

hogares_condicion <- hogares_condicion |> 
  left_join(personas %>% select(id_hogar, p03) |> mutate(pob15a65 = ifelse(p03 >= 15 & p03 <= 65, 1, 0),  
                                                         pob15_65 = ifelse(pob15a65==0, 1, 0)) |> 
              group_by(id_hogar) |> 
              summarise(dependencia= ifelse((sum(pob15_65, na.rm = T)/sum(pob15a65, na.rm = T))<1.25, 1, 0)),
            by= "id_hogar") 

# Escolaraidad del jefe de hogar
hogares_condicion <- hogares_condicion |> 
  left_join(personas |> filter(p01==1) %>% 
              mutate(escolaridad=ifelse(a_escola > 12,1,0)) |> 
              select(id_hogar, escolaridad), 
            by= "id_hogar")


# Dimensión 4: Patrimonio de los hogares ----------------------------------

hogares_condicion <- hogares_condicion %>%
  mutate(
    # Principal combustible del hogar (1 adecuado, 0 inadecuado)
    combustible= ifelse(h05 %in%  c(1:3), 1, 0),
    # Teléfono convencional del hogar (0 no tiene, 1 si tiene)
    telefono= ifelse(h1001==1, 1, 0),
    # Teléfono celular del hogar (o no tiene 1 si tiene)
    celular= ifelse(h1002==1, 1, 0),
    # Servicio de tv pagada (o no tiene 1 si tiene)
    cable= ifelse(h1003==1, 1, 0),
    # Servicio de internet fijo (0 no tiene 1 si tiene)
    internet= ifelse(h1004==1, 1, 0),
    # Computadora del hogar (0 no tiene 1 si tiene)
    computadora=  ifelse(h1005==1, 1, 0),
    # Refrigeradora del hogar (o no tiene 1 si tiene)
    refrigeradora= ifelse(h1006==1, 1, 0),
    # Lavadora del hogar (o no tiene 1 si tiene)
    lavadora= ifelse(h1007==1, 1, 0),
    # Secadora del hogar (o no tiene 1 si tiene)
    secadora= ifelse(h1008==1, 1, 0),
    # Horno microondas del hogar (o no tiene 1 si tiene)
    microondas= ifelse(h1009==1, 1, 0),
    # Máquina extractora de olores del hogar (o no tiene 1 si tiene)
    ext_olores= ifelse(h1010==1, 1, 0),
    # Automóvil del hogar (o no tiene 1 si tiene)
    auto= ifelse(h1011==1, 1, 0),
    # Motocicleta del hogar (o no tiene 1 si tiene)
    moto= ifelse(h1012==1, 1, 0),
    # Espacio para cocinar del hogar (o no tiene 1 si tiene)
    cocina= ifelse(h02==1, 1, 0),
    # Servicio higiénico exclusivo del hogar (0 no es exclusivo 1 si es exclusivo)
    sh= ifelse(h03==1, 1, 0),
    # Ducha del hogar (0 no tiene 1 si tiene)
    ducha= ifelse(h04 == 1, 1, 0))

# Eliminacion de variables
names <- c("h01","h02","h03", "h04","h05","h06","h1001","h1002","h1003","h1004",
           "h1005","h1006","h1007","h1008","h1009","h1010","h1011","h1012","N" )
hogares_condicion <- hogares_condicion |> 
  select(!all_of(names))

# Guardo la base de hogares que tiene los 1 y 0
saveRDS(hogares_condicion, file= "intermedios/02_clasificacion_variables/hogares_var_categorias.rds")
rm(hogares, hogares_condicion)

table(personas$p03,useNA = "ifany")
sum(is.na(personas))


# Personas ----------------------------------------------------------------

personas_condicion <- personas %>% 
  mutate( 
    # Dimensión 5: Educación --------------------------------------------------
    # En los últimos 3 meses ha utilizado celular, internet, computadora o tablet
    alf_dig = ifelse(p03 < 10,NA,
                     ifelse(p2101 == 1 & p2102 == 1 & p2103 == 1 & p2104 ==1, 1, 0)), 
    # p15==2 no asiste entonces en todos es cero
    EGB = ifelse(p03<5 | p03>14,NA,ifelse( p15 == 2, 0, 
                                           # p18 es Ninguno  o Preescolar
                                           ifelse( p17 %in% c(1:3) | 
                                                     #p17 es primario y está dentro de primer a sexto grado
                                                     (p17==5 & p18 %in% c(1:6)) | 
                                                     #p17 es secundario y está dentro de primer a tercer curso
                                                     (p17==7 & p18 %in% c(1:3)) |
                                                     # SISTEMA ACTUAL educación básica
                                                     (p17==6 & p18 %in% c(1:10)), 1, 0))),
    # p15==2 no asiste entonces en todos es cero
    Bachillerato = ifelse(p03<15 | p03>17,NA,ifelse(p15==2, 0, 
                                                    # SISTEMA ANTIGUO, secundaria de cuarto a sexto curso 
                                                    ifelse((p17==7 & p18 %in% c(4:6))|
                                                             # SISTEMA ACTUAL, bachillerato de 1 a 3 curso
                                                             (p17==8 & p18 %in% c(1:3)) , 1, 0 ))),
    #Ciclo Postbachillerato y superior
    # p17 %in% c(9:11), 1, 0 ))),
    # sabe leer y escribir  Alfabetismo */
    alfabetismo= ifelse(p03 < 15,NA,ifelse( p19 == 1, 1, 0)),
    
    # Dimensión 6: Capacidad de generación de ingresos de los hogares ---------
    # Población en edad de trabajar (PET)
    PET = ifelse(p03 >= 15, 1, 0), 
    PEA = ifelse(p03<15, NA,ifelse(p22==7 & p25==2, 0,1)),
    ocupados = ifelse(p03<15,NA, ifelse( p22 %in% c(1:6), 1, 0)),
    NTI= ifelse(p03<5 | p03>17,NA,ifelse(p22==7, 1, 0)),
    # Personas que trabajan o estudian
    NoNINIS = ifelse(p03 < 18 | p03 > 24,NA,ifelse(p22 %in% c(1:6) | p22==7 & p25==1 & p26==3, 1, 0)),
    # Cobertura de seguro
    seguro = ifelse(p30 %in% c(1:6) , 1, 0),
    
    # Dimensión 7: Maternidad temprana ----------------------------------------
    
    # Mujeres  que no tuvieron  hijos
    mj_sinhijos= ifelse(p02==1 | (p03<12 | p03>19) ,NA,ifelse(p02==2  & (p3203==0),1,0)),
    
    # Dimensión 8: Demografia ----------------------------------------
    # Mujeres
    mujeres = ifelse(p02==2, 1, 0),
    # Hombres
    hombres = ifelse(p02==1, 1, 0),
    # tasa de juventud
    juventud = ifelse(p03 >= 15 & p03 <=29, 1, 0),
    # tasa de envejecimiento
    envejecimiento = ifelse(p03 >= 65, 1, 0),
    # etnia 1=INDIGENA Y MONTUBIOS; 3=BLANCOS, MESTIZOS, OTROS; 2= RESTO
    I_M = ifelse(p11 %in% c(1,5),  1, 0) ,
    B_M_O= ifelse(p11 %in% c(6:8), 1, 0),
    A_M_N= ifelse(p11 %in% c(2:4), 1, 0) )

# Eliminacion de variables
names <- c("p01","p11","p15","p16","p17","p18","p19","p20","p2101",
           "p2102","p2103","p2104","p22","p25","p26","p30",
           "p3201","p3202","p3203")

personas_condicion <- personas_condicion |> 
  select(!all_of(names))

saveRDS(personas_condicion, file= "intermedios/02_clasificacion_variables/personas_var_categorias.rds")
