rm(list = ls())
gc()

library(tidyverse)
library(openxlsx)
library(magrittr)
library(rio)

upm_domest <- readRDS("intermedios/02_clasificacion_variables/upm_domest.rds") |> 
  filter(n_estratos > 1)

domest1 <- unique(upm_domest$domest)

for (i in 1:length(domest1)) {
  
  # VIVIENDA ---------------------------------------------------------------
  
  #Cargo la base de 1  y 0 para la vivienda
  
  load(paste0("productos/02_clasificacion_variables/",domest1[i],".RData"))
  
  vivienda_condicion = vivienda_condicion #%>%
  
  # Indicadores Dimensión 1: Características de la vivienda ----------------------
  
  dim1 <- vivienda_condicion %>% 
    group_by(id_upm,domest) %>% 
    summarise(n=n(),
              #material predominante del techo o cubierta por sector censal
              techo= sum(techo, na.rm=T),
              #material predominante de las paredes exteriores por sector censal
              paredes= sum(paredes, na.rm = T),
              #material predominante del piso
              piso= sum(piso, na.rm = T)) %>% 
    mutate(techo=round(100*techo/n,2),
           paredes=round(100*paredes/n,2),
           piso=round(100*piso/n,2)) %>% 
    select(id_upm, domest,techo,paredes,piso)
  
  # Indicadores Dimensión 2:  ----------------------
  
  dim2 <- vivienda_condicion %>%  
    group_by(id_upm, domest) %>% 
    summarise(n=n(),
              #De dónde proviene principalmente el agua que recibe la vivienda
              p_agua=sum(p_agua,na.rm = T),
              #El agua que recibe la vivienda es
              r_agua=sum(r_agua,na.rm = T),
              #El servicio higiénico o escusado de la vivienda es:
              cx_sh=sum(cx_sh,na.rm = T),
              #Servicio de luz de la vivienda
              luz=sum(luz,na.rm = T),
              #Eliminación de la basura de la vivienda
              basura=sum(basura,na.rm = T)) %>% 
    mutate(p_agua=round(100*p_agua/n,2),
           r_agua=round(100*r_agua/n,2),
           cx_sh=round(100*cx_sh/n,2),
           luz=round(100*luz/n,2),
           basura=round(100*basura/n,2)) %>% 
    select(id_upm, domest,p_agua,r_agua,cx_sh,luz,basura)
  
  rm(vivienda_condicion)
  
  # HOGAR -------------------------------------------------------------------
  
  # Indicadores Dimensión 3:  ----------------------
  dim3 <- hogares_condicion %>%
    group_by(id_upm, domest) %>%
    summarise(n= n(), #número de hogares por sector censal
              hac_hog_sec= sum(no_hacin,na.rm = T), 
              agua_segura_sec = sum(agua_seg,na.rm = T),
              no_monoparental= sum(no_monoparental,na.rm = T),
              dependencia= sum(dependencia, na.rm = T)) %>% 
    mutate(no_hacin = round(100*hac_hog_sec/n,2),
           agua_seg = round(100*agua_segura_sec/n,2),
           no_monoparental = round(100*no_monoparental/n,2),
           dependencia = round(100*dependencia/n, 2)) %>% 
    select(id_upm, domest,no_hacin, agua_seg, no_monoparental,dependencia)
  
  # Indicadores Dimensión 4:  ----------------------
  dim4 <- hogares_condicion %>% 
    group_by(id_upm, domest) %>% 
    summarise(n= n(),
              combustible = sum(combustible, na.rm=T), 
              telefono = sum(telefono, na.rm=T), 
              celular = sum(celular, na.rm=T), 
              cable = sum(cable, na.rm=T),
              internet = sum(internet, na.rm=T),
              computadora = sum(computadora, na.rm=T),
              refrigeradora = sum(refrigeradora, na.rm=T),
              lavadora = sum(lavadora, na.rm=T),
              secadora = sum(secadora, na.rm=T),
              microondas = sum(microondas, na.rm=T),
              ext_olores = sum(ext_olores, na.rm=T),
              auto = sum(auto, na.rm=T),
              moto = sum(moto, na.rm=T),
              cocina = sum(cocina, na.rm=T), 
              sh = sum(sh, na.rm=T), 
              ducha = sum(ducha, na.rm=T)) %>% 
    mutate(combustible = round(100*combustible/n,2), 
           telefono = round(100*telefono/n,2),
           celular = round(100*celular/n,2),
           cable = round(100*cable/n,2),
           internet = round(100*internet/n,2),
           computadora = round(100*computadora/n,2),
           refrigeradora = round(100*refrigeradora/n,2),
           lavadora = round(100*lavadora/n,2),
           secadora = round(100*secadora/n,2),
           microondas = round(100*microondas/n,2),
           ext_olores = round(100*ext_olores/n,2),
           auto = round(100*auto/n,2),
           moto = round(100*moto/n,2),
           cocina = round(100*cocina/n,2),
           sh = round(100*sh/n,2),
           ducha = round(100*ducha/n,2)) %>%
    select(id_upm, domest,combustible,telefono,celular, cable,internet,computadora,
           refrigeradora,lavadora,secadora,microondas,ext_olores,auto,moto,cocina,sh,ducha)
  
  # Indicadores Dimensión 5:  ----------------------
  
  dim5_1 <- hogares_condicion %>%
    group_by(id_upm, domest) %>%
    summarise(n= n(),
              escolaridad= sum(escolaridad,na.rm = T)) %>% 
    mutate(escolaridad = round(100*escolaridad/n,2)) %>% 
    select(id_upm, domest,escolaridad)
  
  #Borro lo que no sirve
  rm(hogares_condicion)
  
  # PERSONAS ----------------------------------------------------------------
  
  # Indicadores Dimensión 5:  ----------------------
  
  # Tasa neta de asistencia de educación general básica (5 a 14 años)
  dim5_2 <- personas_condicion %>%
    filter(p03 >= 5 & p03 <= 14) %>% 
    group_by(id_upm, domest) %>%
    summarise(n=n(),
              EGB= sum(EGB,na.rm = T)) %>%
    mutate(EGB=round(100*EGB/n,2)) %>%
    select(id_upm, domest,EGB)
  
  # Tasa neta de asistencia ajustada en bachillerato (15 a 17 años)
  dim5_3 <- personas_condicion %>% 
    filter(p03 >= 15 & p03 <= 17) %>% 
    group_by(id_upm, domest) %>% 
    summarise(n=n(),
              Bachillerato= sum(Bachillerato,na.rm = T)) %>%
    mutate(Bachillerato=round(100*Bachillerato/n,2)) %>%
    select(id_upm, domest,Bachillerato)
  
  # ALFABETISMO
  dim5_4 <- personas_condicion %>% 
    filter(p03>=15) %>% 
    group_by(id_upm, domest) %>% 
    summarise(alfabetismo= sum(alfabetismo, na.rm=T),
              totperm15= n()) %>% 
    mutate(alfabetismo = round(100*alfabetismo/totperm15,2)) %>%
    select(id_upm, domest,alfabetismo)
  
  ## Tasa de Alfabetismo Digital
  dim5_5 <- personas_condicion %>%
    filter(p03 >= 10) %>% 
    group_by(id_upm, domest) %>%
    summarise(n=n(),
              alf_dig = sum(alf_dig, na.rm = T)) %>% 
    mutate(alf_dig= round(100*alf_dig/n,2)) %>%
    select(id_upm, domest,alf_dig)
  
  dim5 <- dim5_1 %>% 
    full_join(dim5_2, by=c("id_upm", "domest")) %>% 
    full_join(dim5_3, by=c("id_upm", "domest")) %>% 
    # full_join(dim5_4, by=c("id_upm", "domest"))  %>% 
    full_join(dim5_5, by=c("id_upm", "domest"))
  
  rm(dim5_1,dim5_2,dim5_3,dim5_5)
  
  # Indicadores Dimensión 6:  ----------------------
  
  dim6_1 <- personas_condicion %>%
    group_by(id_upm, domest) %>% 
    summarise(n = n(), 
              PET = sum(PET, na.rm=T), 
              PEA = sum(PEA, na.rm=T), 
              ocupados = sum(ocupados, na.rm=T),
              seguro = sum(seguro, na.rm=T)) %>% 
    mutate(TPB = round(100*PEA/n,2), 
           TPG = round(100*PEA/PET,2),
           TOG = round(100*ocupados/PEA,2),
           seguro = round(100*seguro/n,2)) %>%
    select(id_upm, domest,TPB, TPG, TOG, seguro)
  
  # tasa de no trabajo infantil de 5 a 17 años
  dim6_2 <-  personas_condicion %>% 
    filter(p03>=5 & p03<=17)%>%
    group_by(id_upm, domest) %>%
    summarise(n=n(),
              NTI = sum(NTI, na.rm = T)) %>%
    mutate(NTI= round(100*NTI/n,2)) %>%
    select(id_upm, domest,NTI)
  
  # tasa de jóvenes que trabajan o estudian (18 a 24 años)
  dim6_3 <-  personas_condicion %>% 
    filter(p03>=18 & p03<= 24)%>%
    group_by(id_upm, domest) %>%
    summarise(n=n(),
              NoNINIS = sum(NoNINIS, na.rm = T)) %>%
    mutate(NoNINIS= round(100*NoNINIS/n,2)) %>%
    select(id_upm, domest,NoNINIS)
  
  
  dim6 <- dim6_1 %>% 
    full_join(dim6_2, by=c("id_upm", "domest")) %>% 
    full_join(dim6_3, by=c("id_upm", "domest")) 
  
  rm(dim6_1,dim6_2,dim6_3)
  
  # Agregado a nivel de sector censal
  dim7 <- personas_condicion %>%
    # Mujeres de 12 a 19 años y que responden a tener hijos
    filter( p02==2 & p03 %in% c(12:19) ) |>
    group_by(id_upm, domest) %>%
    summarise(
      # total mujeres de 12 a 19 años por sector censal
      n= n(),
      # Mujeres de 12 a 19 que tuvieron su ultimo hijo nacido vivo entre nov2009-nov2010 por sector censal
      mj_sinhijos =sum(mj_sinhijos,na.rm=T)) |>
    mutate(
      # mujeres adolescentes sin hijos
      mj_sinhijos=  round(100*(mj_sinhijos/n), 2)
    ) %>%
    select(id_upm, domest, mj_sinhijos )
  
  
  # Indicadores Dimensión 8:  ----------------------
  dim8<- personas_condicion %>%
    group_by(id_upm, domest) %>%
    summarise(n=n(),
              #Número de MUJERES y HOMBRES a nivel censal
              mujeres= sum(mujeres, na.rm=T),
              hombres = sum(hombres, na.rm = T),
              #Juventud
              juventud = sum(juventud,na.rm = T),
              # envejecimiento
              envejecimiento = sum(envejecimiento,na.rm = T),
              #Etnias
              I_M = sum(I_M, na.rm = T),
              A_M_N = sum(A_M_N, na.rm = T),
              B_M_O = sum(B_M_O, na.rm = T)) %>%
    mutate(mujeres= round(100*mujeres/n,2),
           hombres = round(100*hombres/n, 2),
           juventud = round(100*juventud/n,2),
           envejecimiento = round(100*envejecimiento/n,2),
           I_M = round(100*I_M/n,2),
           A_M_N = round(100*A_M_N/n,2),
           B_M_O = round(100*B_M_O/n,2)) %>% 
    select(id_upm, domest, mujeres, hombres, 
           juventud, envejecimiento, I_M,
           A_M_N,B_M_O)
  
  #Unión de todas las dimensiones en una sola data a nivel de sector censal
  indicadores <- dim1 %>%
    full_join(dim2, by=c("id_upm", "domest")) %>%
    full_join(dim3, by=c("id_upm", "domest")) %>%
    full_join(dim4, by=c("id_upm", "domest")) %>%
    full_join(dim5, by=c("id_upm", "domest")) %>%
    full_join(dim6, by=c("id_upm", "domest")) %>%
    full_join(dim7, by=c("id_upm", "domest")) %>%
    full_join(dim8, by=c("id_upm", "domest"))
  
  # indicadores_urbano <- indicadores
  
  # Guardo la base
  saveRDS(indicadores, file = paste0("intermedios/03_estratificacion/indicadores/",domest1[i],".rds"))
  
  print(domest1[i])
  
}
