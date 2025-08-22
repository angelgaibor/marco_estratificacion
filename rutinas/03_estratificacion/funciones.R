graf_corr <- function(aux, num=0.3){
  corrplot(aux,
           diag = F,
           insig='blank',
           cl.ratio = 0.1, 
           col = c("blue", COL2('RdYlBu', 18),"#9F00FF"),
           #cl.length = 21,
           tl.cex = 0.5,
           tl.col= "black",
           number.cex = num, #tamaño de los números
           number.digits = 2,
           method = "number",
           type="upper")
}


mean_var <- function(x, m2, s2){
  
  m1 <- mean(x)
  s1 <- sd(x)
  if(s1>0){
    y <- m2 + (x-m1)*(s2/s1)}else{
      print("sd es positiva")
    }
  
}




# Generación de los 5 métodos a nivel de UPM -----------------------------------------------
metodos_univ <- function(y_trans, H){
  
  #Creo una base donde se van a quedar las estratificaciones a nivel de sector censal
  univariado <- data.frame(id_sector= indicadores$id_sector) 
  
  #guardo la primera componente
  Factor_Posit <- y_trans
  
  #Percentiles
  estratif_percentil  <- quantile(Factor_Posit,
                                  probs = seq(0, 1, by = 1 / H),)
  univariado$Percentiles <- cut(
    Factor_Posit,
    breaks = estratif_percentil,
    include.lowest = T,
    right = F,
    label = 1:H
  )
  
  #Dalenius
  estratif_danelniusH3 <-
    strata.cumrootf(
      x = Factor_Posit , #componente principal
      Ls = H,
      CV = 0.03,
      alloc = c(0.5, 0, 0.5) #afijacion proporcional
    )
  univariado$Dalenius <- estratif_danelniusH3$stratumID
  
  #LH_Sethi
  estratif_LH_SethiH3 <-
    strata.LH(
      x = Factor_Posit ,
      Ls = H,
      CV = 0.03,
      alloc = c(0.5, 0, 0.5),
      algo = "Sethi"
    )
  univariado$LH_Sethi <- estratif_LH_SethiH3$stratumID
  
  # LH_Kozak
  estratif_LH_KozakH3 <-
    strata.LH(
      x = Factor_Posit ,
      Ls = H,
      CV = 0.03,
      alloc = c(0.5, 0, 0.5),
      algo = "Kozak"
    )
  univariado$LH_Kozak <- estratif_LH_KozakH3$stratumID
  
  #Geometrico
  estratif_geoH3 <- strata.geo(
    x = Factor_Posit ,
    Ls = H,
    CV = 0.03,
    alloc = c(0.5, 0, 0.5)
  )
  univariado$Geometric <- estratif_geoH3$stratumID
  
  # # #Genético
  # baseframe <- data.frame(id = indicadores$id_sector,
  #                         X1 = Factor_Posit,
  #                         Y1 = Factor_Posit,
  #                         domainvalue = 1)
  # 
  # basestrata <- buildStrataDF(baseframe, progress = F, verbose = F)
  # 
  # baseerrors <- data.frame(DOM = "DOM1", CV1 = 0.03, domainvalue = 1)
  # 
  # baseframe[is.na(baseframe)] <- 0
  # 
  # solution <- optimizeStrata2(nStrata = H,
  #                             errors = baseerrors,
  #                             framesamp = baseframe,
  #                             parallel = FALSE,
  #                             writeFiles = FALSE,
  #                             showPlot = FALSE)
  # 
  # univariado$Genetico <- solution$framenew$STRATO


  return(univariado)
}

# Función Jarque y K means
Estratif_kmeans <- function(Matriz_Estratificacion,
                            num_estratos,
                            variable_evaluacion_orden_estrato,
                            inter.max = 100,
                            metodo = metodo) {
  X <- Matriz_Estratificacion
  H <- num_estratos
  
  if (metodo == "Jarque") {
    x1 <- scale(X, center = F, scale = apply(X, 2, sd))
    
    set.seed(12345)
    kmedias <- kmeans(
      x = x1,
      centers = H,
      algorithm  = "MacQueen",
      iter.max = inter.max
    )
  }
  
  if (metodo == "Kmeans") {
    set.seed(12345)
    kmedias <- kmeans(
      x = X,
      centers = H,
      algorithm  = "MacQueen",
      iter.max = inter.max
    )
  }
  
  vct_estratoJarque <- kmedias$cluster
  
  # Con no hacinado establecer el orden del estrato--CORRER SI SE FIJA UNA VARIABLE (cx_sh el de menor deff)
  vctr_orden_estrato <-
    sort(tapply(X[[variable_evaluacion_orden_estrato]], vct_estratoJarque, FUN = mean))
  valores_origen <- names(vctr_orden_estrato)
  valores_destino <- 1:H
  estrato <-
    plyr::mapvalues(x = vct_estratoJarque, valores_origen, valores_destino)
  

#dESCRIPTIVOS DE LOS ESTRATOS INICIALES ASIGNADOS POR   KMEANS
  df_eval <- Matriz_Estratificacion |> mutate(Jarque= vct_estratoJarque)
  #Creo la matriz para evaluar cuál es el orden de los estratos
  prueba <- data.frame()
  for(i in names(Matriz_Estratificacion)){
    prueba <- prueba |> rbind(data.frame(Variable= rep(i,length(unique(df_eval$Jarque))),df_eval |> 
      group_by(Jarque) |> 
      summarise(min= min(get(i)), media = mean(get(i)),
                q_25 = as.numeric(quantile(get(i), 0.25)), 
                q_50= median(get(i)), 
                q_75= as.numeric(quantile(get(i), 0.75)),
                max= max(get(i) ))))
  }
  
  resultado <- list(estrato, prueba)
  return(resultado)
}

metodos_multi <- function(df_metodos,
                          Matriz_Estratificacion, 
                          num_estratos,
                          variable_evaluacion_orden_estrato,
                          inter.max = 100){
  
  aux <- Estratif_kmeans(
    Matriz_Estratificacion = Matriz_Estratificacion,
    num_estratos = num_estratos,
    variable_evaluacion_orden_estrato = variable_evaluacion_orden_estrato,
    metodo = "Jarque"
  )
  
  #k-means de Jarque
  df_metodos$Jarque <-aux[[1]]
  
  df_eval_jarque <- aux[[2]]
  
  #K-means
  
  aux <- Estratif_kmeans(
    Matriz_Estratificacion,
    num_estratos,
    variable_evaluacion_orden_estrato,
    metodo = "Kmeans"
  )
  
  df_metodos$Kmeans <-aux[[1]]
  
  df_eval_kmeans <- aux[[2]]
  names(df_eval_kmeans)[which(names(df_eval_kmeans)=="Jarque")] <- "Kmeans"
    
  
  # # #Genético
  # baseframe <- Matriz_Estratificacion |>
  #   mutate(id_sector= df_metodos$id_sector,
  #          domainvalue=1)
  # 
  # #Construyo la base para correr el algoritmo genético (esta debe ser la estructura)
  # baseframe <- buildFrameDF(df= baseframe,
  #              id= "id_sector",
  #              domainvalue = "domainvalue",
  #              X = names(Matriz_Estratificacion),
  #              Y = names(Matriz_Estratificacion))
  # 
  # #Base de la estructura para crear los estratos
  # basestrata <- buildStrataDF(baseframe,
  #                             progress = F,
  #                             verbose = F)
  # 
  # #Base de errores
  # baseerrors <- data.frame(DOM = "DOM1",
  #                          CV1 = 0.03,
  #                          domainvalue = 1)
  # 
  # #Reemplazo los NA con 0 (en el caso de que existan)
  # baseframe[is.na(baseframe)] <- 0
  # 
  # 
  # #Corro el algoritmo para la generación de los estratos
  # solution <- optimizeStrata2(nStrata = num_estratos,
  #                             errors = baseerrors, 
  #                             framesamp = baseframe,
  #                             parallel = FALSE,
  #                             writeFiles = FALSE,
  #                             showPlot = FALSE)
  # 
  # #Guardo el estrato asignado por el algoritmo genético en la base df_metodos
  # df_metodos$Genetico_mult <- solution$framenew$STRATO
  # 
  # #Creo la matriz de descriptivos en base al estrato obtenido por el genético multi
  # df_eval <- Matriz_Estratificacion |> mutate(Gen_multi= solution$framenew$STRATO)
  # #Creo la matriz para evaluar cuál es el orden de los estratos
  # prueba <- data.frame()
  # for(i in names(Matriz_Estratificacion)){
  #   prueba <- prueba |> rbind(data.frame(Variable= rep(i,length(unique(df_eval$Gen_multi))),df_eval |> 
  #                                          group_by(Gen_multi) |> 
  #                                          summarise(min= min(get(i)), media = mean(get(i)),
  #                                                    q_25 = as.numeric(quantile(get(i), 0.25)), 
  #                                                    q_50= median(get(i)), 
  #                                                    q_75= as.numeric(quantile(get(i), 0.75)),
  #                                                    max= max(get(i) ))))
  # }
  # 
  # df_eval_gen_multi <- prueba
  # 
  # 
  # res <- list(df_metodos, df_eval_jarque, df_eval_kmeans, df_eval_gen_multi)
  
  res <- list(df_metodos, df_eval_jarque, df_eval_kmeans)
  
  return(res)
  
}

# deff para cada variable (base de totales)
deff_estratificado <- function(datos,
                               txt_variable_evaluacion, # si tiene o no (1 o 0)
                               txt_estrato) {
  datos <- datos |> filter(!is.na(get(txt_variable_evaluacion))) 
  modelo <-
    lm(datos[[txt_variable_evaluacion]]  ~ datos[[txt_estrato]] )
  resumen <- summary(modelo)
  deff <- 1 - resumen$r.squared
  return(deff)
}


deff_datas <- function(data_censo, metodo, df_univariado, variables_estratificacion){
  
  #Identifico las variables que se van a evaluar en el hogar
  variable_evaluacion <- intersect(colnames(data_censo), variables_estratificacion)
  
  
  ############## Evaluación de los estratos #############################
  #A nivel de hogar
  df <- data_censo |>  
    select(c(id_sector, all_of(variable_evaluacion))) |> 
    left_join(df_univariado %>%select("id_sector", all_of(metodo)),
              by = "id_sector")
  
  aux_deff <- as.numeric(length(variable_evaluacion))
  for (i in 1:length(variable_evaluacion)) {
    aux_deff[i] <-
      deff_estratificado(df, variable_evaluacion[i], metodo)
  }
  names(aux_deff) <- variable_evaluacion
  
  rm(data_censo, df)
  
  return(aux_deff)
  
}

mx_coincidencias <- function(data){
  var_names <- names(data)
  N <- nrow(data)
  
  res <- data.frame()
  j <- 1
  for (i in var_names){
    for (j in var_names) {
    
      res[i,j] <- sum(data[[i]]== data[[j]], na.rm=T)/N  
    }
    
  }
  
  return(res)
}


