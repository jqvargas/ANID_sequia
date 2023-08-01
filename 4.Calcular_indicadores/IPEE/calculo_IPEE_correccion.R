###############################################################################
################# Codigo Juan de Dios: Modificacion Marialina ################# 
###############################################################################

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
