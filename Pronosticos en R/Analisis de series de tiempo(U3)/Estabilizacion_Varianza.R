# Script que busca el mejor lambda para estabilizar la varianza según el procedimeinto de Guerrero
library(car)
library(readxl)
Actividad_3_6 <- read_excel("~/MEGAsync/MAC/Materias/Pronosticos/Hojas de calculo/Actividad 3.6.xlsx")
Hoja<-Actividad_3_6

Hoja.ts <- ts(Hoja$Valor,start=1971,freq=12)
Hoja.ts[1:15]

# Valores necesarios
R=11
H=10
lambdas<-c(-2,-1.5,-0.5,-0.005579,0,0.5,1,1.5,2) # Posibles valores de lamba
valor_lambda(Hoja.ts,R,H,lambdas)
# Se calculan los coeficientes de variación y se regresa el valor lambda que da el menor de ellos
valor_lambda<-function(Hoja.ts,R,H,lambdas){
  N=length(Hoja.ts)
  Hoja.ts<-as.numeric(Hoja.ts)
  media_h<-c()
  s_h<-c()
  
  #Calculo de medias y desviaciones
  for (i in seq(R,N,by=R) ){
    u=Hoja.ts[(i-R+1):i]   # El +1 es por que el operador : es inclusivo de ambos lados
    media_h=c(media_h,mean(u))
    s_h=c(s_h,sqrt(var(u)))
  }
  
  # Calculo de tabla de potencias de lambda para conseguir el mejor coeficiente
  min_coef<-(50000000000)
  mejor_lambda<-0
  for (l in lambdas){
    columna<-c()
    for (i in 1:H){
      columna<-c(columna,s_h[i]/(media_h[i]^(1-l)))
    }
    media_l<-mean(columna)
    de_l<-sqrt(var(columna))
    coef_var<-(de_l/media_l)
    coef_var
    if (coef_var<min_coef){
      min_coef<-coef_var
      mejor_lambda<-l
    }
  }
  mejor_lambda
}

