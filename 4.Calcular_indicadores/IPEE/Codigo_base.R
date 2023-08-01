###############################################################################
####################### Codigo Marialina Nunez Grahmann #######################  
###############################################################################

#CODIGO BASE CÁLCULO IPEE 

##Primero instalamos paquetes
#install.packages("tibble")
#install.packages("actuar")

calcular_IPEE<-function(pp_acumulada_mensual,Carac_gamma,acumulacion){
# Cargamos los paquetes
library(lmomco) #necesario para la ecuación

#Limpiar variables (ctrl+l para limpiar consola)
#rm(list=ls())  

#Definir carpeta de trabajo
#setwd("C:/Users/Mari/Desktop/Mari/Universidad/Magister/Sequia/Ingeniera de Proyectos/Codigo en R/IPEE_mi_codigo/Codigo Final")

calculo_ipee<-function(BH,xi,alpha, kappa){
  library(SPEI)
  library(lmomco)
  # BH es el acumulado (promedio) de los ultimos "n" meses 
  # del balance precipitacion-evapotranspiracion
  # Coeficientes xi, alpha y kappa de la distributcion log-logistica
  # obtenidos del periodo de referencia
  
  if (is.na(BH) || is.na(xi) || is.na(alpha) || is.na(kappa)){
    ind_IPEE <- NA
  }
  else {
    parametros<-c(xi, alpha, kappa)
    param<-list(type='glo',para=parametros,source='parglo')
    ind_IPEE<-qnorm(cdfglo(BH, param))}
  #Caso de que de -Inf
  if (is.infinite(ind_IPEE) && (ind_IPEE < 0)){
    ind_IPEE<- -3}
  #Caso de que de Inf
  if (is.infinite(ind_IPEE)){
    ind_IPEE<- 3}
  
  return(ind_IPEE)}
################################################################################
#######NECESITAMOS OBTENER LA BBDD DEL BALANCE HÍDRICO#####################
################################################################################

##CARGAMOS CÓDIGO QUE TOMA PP DIARIAS MENSUALES Y LAS CONVIERTE EN TOTAL MENSUAL

#source(paste0(getwd(),"/","Calculo_ET0.R"))


#SI YA TENEMOS EL TOTAL MENSUAL, CARGAMOS EL ARCHIVO, DE LO CONTRARIO DESCOMEN_
#TAMOS LA LINEA ANTERIOR Y COMENTAMOS LO SIGUIENTE

##############################EMPEZAMOS A COMENTAR##############################

bh_mensual<-read.csv(paste0(getwd(),"/","Balance_hidrico_bbdd_total.csv")) #USA FILTRO 70% EN PP MENSUAL

rownames(bh_mensual)<-bh_mensual[,1]
bh_mensual <- subset(bh_mensual, select = -1) #Convertimos primera columna en nombres de filas


nombres_corregidos<-c() #creamos vector vacío para rellenar con datos correctos de código de estación (eliminar X inicial y cambiar . por -)
contador<-1
for (i in colnames(bh_mensual)){
  codigo_provisorio_est<-gsub("\\.", "-", i)
  nombres_corregidos[contador]<-gsub("X", "", codigo_provisorio_est)
  contador=contador+1
}
names(bh_mensual)<-nombres_corregidos

#############################TERMINAMOS DE COMENTAR#############################


################################################################################
########################Cambiamos formato nombre estación#######################
################################################################################

#Los parámetros de la función Gamma están según otro formato: es necesario cambiar el nombre de las estaciones en el DataFrame

#Descargamos la BBDD de metadatos de estaciones DGA
metadatos<-read.csv(paste0(getwd(),"/","Metadatos_estaciones_pp_DGA.csv"))

#Además, vamos a hacer que los datos esten escritos de manera más cómoda e universal 
metadatos_a_parms_loglogic<-read.csv(paste0(getwd(),"/","Simil estaciones.csv"))

#Cambiamos el nombre en metadatos para que después se utilice siempre el mismo nombre
nombre_met_a_formato_loglogis<-c()

contador<-1
for (j in metadatos[,2]){
  contador_nombre<-1
  for (i in metadatos_a_parms_loglogic[,1]){
    if (i==j){
      nombre_met_a_formato_loglogis[contador]<-metadatos_a_parms_loglogic[contador_nombre,3]
    }
    contador_nombre<-contador_nombre+1
  }
  contador<-contador+1
}

metadatos[,3]<-nombre_met_a_formato_loglogis #ahora ponemos los nombres en sistema menos "complicado"


#creamos listas vacias para almacenar las características de las estaciones
contador_nombres<-1
lista_nombres<-c()
lista_codigo<-c()
lista_LAT<-c()
lista_LON<-c()
lista_ALTURA<-c()
lista_REGION<-c()
lista_COMUNA<-c()
lista_CUENCA<-c()
lista_SUBCUENCA<-c()

#buscamos en los metadatos el código y lo cambiamos por el nombre de la estación
for (i in colnames(bh_mensual)){
  contador_metadatos<-1
  for (j in metadatos[,2]){
    if (i==j){
      lista_codigo[contador_nombres]<-metadatos[contador_metadatos,2]
      lista_nombres[contador_nombres]<-metadatos[contador_metadatos,3]
      lista_LAT[contador_nombres]<-metadatos[contador_metadatos,4]
      lista_LON[contador_nombres]<-metadatos[contador_metadatos,5]
      lista_ALTURA[contador_nombres]<-metadatos[contador_metadatos,8]
      lista_REGION[contador_nombres]<-metadatos[contador_metadatos,15]
      lista_COMUNA[contador_nombres]<-metadatos[contador_metadatos,17]
      lista_CUENCA[contador_nombres]<-metadatos[contador_metadatos,18]
      lista_SUBCUENCA[contador_nombres]<-metadatos[contador_metadatos,19]
    }
    contador_metadatos<-contador_metadatos+1
  }
  contador_nombres<-contador_nombres+1
}


################################################################################
#####################Cálculo de precipitaciones acumuladas######################
################################################################################

#ahora hacemos agregación de caudal para luego obtener ICE6
bh_6meses<-bh_mensual[13:nrow(bh_mensual),]#usamos mismo formato para las fechas y las estaciones, empezando desde el año 1991.
bh_6meses[,] <- NA #generamos valores de NA en donde pondremos los promedios acumulados

#ahora obtenemos los valores acumulados del promedio de caudal de 6 meses (promedio 6 meses de caudal considerando el mes de cálculo)
for (col in c(1:ncol(bh_6meses))){ 
  for (fila in 1:nrow(bh_6meses)){
    datos_a_promediar <- bh_mensual[(fila+7):(fila+12),col]
    datos_no_na <- sum(complete.cases(datos_a_promediar))
    if (datos_no_na>=5){ #usamos un 82% de filtro (deben haber al menos 5 meses para obtener el promedio)
      a_promediar <- na.omit(datos_a_promediar)
    }
    else {
      a_promediar <- datos_a_promediar
    }
    bh_6meses[fila,col] <- mean(datos_a_promediar)
  }
}

#ahora hacemos un simil para la acumulación de 12 meses (promedio 12 meses de caudal considerando el mes de cálculo)
bh_12meses<-bh_mensual[13:nrow(bh_mensual),] #usamos mismo formato para las fechas y las estaciones, empezando desde el año 1991.
bh_12meses[,] <- NA  #generamos valores de NA en donde pondremos los promedios acumulados


#ahora obtenemos los valores acumulados del promedio de caudal para luego calcular IPE12
for (col in c(1:ncol(bh_12meses))){ 
  for (fila in 1:nrow(bh_12meses)){
    datos_a_promediar <- bh_mensual[(fila+1):(fila+12),col]
    datos_no_na <- sum(complete.cases(datos_a_promediar))
    if (datos_no_na>=10){ #usamos un 82% de filtro (deben haber al menos 5 meses para obtener el promedio)
      a_promediar <- na.omit(datos_a_promediar)
    }
    else {
      a_promediar <- datos_a_promediar
    }
    bh_12meses[fila,col] <- mean(datos_a_promediar)
  }
}

################################################################################
#############################CÁLCULO INDICADOR ICE##############################
################################################################################

source(paste0(getwd(),"/","calculo_IPEE_correccion.R"))  ##MISMO CÓDIGO QUE PARA IPE

#Para esto necesitamos exportar las características de la función Gamma para cada estación
Carac_loglogis<-read.csv(paste0(getwd(),"/","Coeficientes_IPEE_1991_2020_v1.csv"))

##en caso que el código se lea de forma XCODIGO.DIGITO en vez de CODIGO-DIGITO, lo arreglamos
nombres_corregidos<-c() #creamos vector vacío para rellenar con datos correctos de código de estación (eliminar X inicial y cambiar . por -)
contador<-1
for (i in colnames(Carac_loglogis[,3:ncol(Carac_loglogis)])){
  codigo_provisorio_est<-gsub("\\.", "-", i)
  nombres_corregidos[contador]<-gsub("X", "", codigo_provisorio_est)
  contador=contador+1
}

#una vez corregidos los códigos, es necesario que estos se cambien en el Data Frame
nuevas_columnas_inicio<-colnames(Carac_loglogis[,1:2])
nuevas_columnas<-c(nuevas_columnas_inicio,nombres_corregidos)

names(Carac_loglogis)<-nuevas_columnas #definimos nuevas columnas


####Una vez cargados los parámetros de la función Gamma que caracteriza cada registro, es posible, 
####utilizando la función Cálculo IPE, obtener los valores para la acumulación deseada

################################################################################
####################################IPE 6#######################################
################################################################################

IPEE6_MATRIZ <- bh_6meses #[,1:5] para prueba
IPEE6_MATRIZ[,] <- NA

#Limpiamos los parámetros de la función gamma por la que acumulación que nos sirve.
Loglogis_6meses <-subset(Carac_loglogis,Acum==6)

contador_estacion_ipee6<-1
for (i in colnames(IPEE6_MATRIZ[,])){
  contador_estacion_loglogis<-3
  for (j in colnames(Loglogis_6meses[,3:ncol(Loglogis_6meses)])){
    if(i==j){
      contador_fecha<-1
      for (fecha_dato in rownames(IPEE6_MATRIZ)){
        k<-as.integer(substr(fecha_dato, 1, 2))
        BH<-bh_6meses[contador_fecha,contador_estacion_ipee6]
        IPEE6_MATRIZ[contador_fecha,contador_estacion_ipee6]<-calculo_ipee(BH,Loglogis_6meses[k,contador_estacion_loglogis],Loglogis_6meses[k+12,contador_estacion_loglogis],Loglogis_6meses[k+24,contador_estacion_loglogis])
        contador_fecha<-contador_fecha+1
      }
    }
    contador_estacion_loglogis<-contador_estacion_loglogis+1
  }
  contador_estacion_ipee6<-contador_estacion_ipee6+1
}

write.csv(IPEE6_MATRIZ, file = "IPEE6_MATRIZ_coefv2.csv")

################################################################################
####################################IPE 12######################################
################################################################################

IPEE12_MATRIZ <- bh_12meses #[,1:5] para prueba
IPEE12_MATRIZ[,] <- NA

#Limpiamos los parámetros de la función gamma por la que acumulación que nos sirve.
Loglogis_12meses <-subset(Carac_loglogis,Acum==12)

contador_estacion_ipee12<-1
for (i in colnames(IPEE12_MATRIZ[,])){
  contador_estacion_loglogis<-3
  for (j in colnames(Loglogis_12meses[,3:ncol(Loglogis_12meses)])){
    if(i==j){
      contador_fecha<-1
      for (fecha_dato in rownames(IPEE12_MATRIZ)){
        k<-as.integer(substr(fecha_dato, 1, 2))
        BH<-bh_12meses[contador_fecha,contador_estacion_ipee12]
        IPEE12_MATRIZ[contador_fecha,contador_estacion_ipee12]<-calculo_ipee(BH,Loglogis_12meses[k,contador_estacion_loglogis],Loglogis_12meses[k+12,contador_estacion_loglogis],Loglogis_12meses[k+24,contador_estacion_loglogis])
        contador_fecha<-contador_fecha+1
      }
    }
    contador_estacion_loglogis<-contador_estacion_loglogis+1
  }
  contador_estacion_ipee12<-contador_estacion_ipee12+1
}



#Fin
}
#write.csv(IPEE12_MATRIZ, file = "IPEE12_MATRIZ_coefv2.csv")


##########VAMOS A ESCRIBIR LAS CARACTERÍSTICAS DE LAS ESTACIONES##########
#prueba <- data.frame(matrix(nrow = 8, ncol = ncol(IPEE12_MATRIZ)))
#prueba[1,] <- lista_nombres
#prueba[2,] <- lista_LAT
#prueba[3,] <- lista_LON
#prueba[4,] <- lista_ALTURA
#prueba[5,] <- lista_REGION
#prueba[6,] <- lista_COMUNA
#prueba[7,] <- lista_CUENCA
#prueba[8,] <- lista_SUBCUENCA
#nombres_filas <- c("Nombre estacion","Latitud","Longitud","Altira","Region","Comuna","Cuenca","Subcuenca")
#colnames(prueba) <- colnames(IPEE12_MATRIZ)
#rownames(prueba) <- nombres_filas

#write.csv(prueba, file = "IPEE_caracteristicas.csv")
#write.csv(prueba, file = "IPE6_caracteristicas.csv")
