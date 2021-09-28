# Script que muestra como calcular la varianza de una serie de tiempo siguiendo el procedimiento de guerrero
# con un ejemplo.
# Se elimino el ultimo valor del archivo del profesor por que es más instructivo así
library(car)
library(readxl)
Actividad_3_6 <- read_excel("~/MEGAsync/MAC/Materias/Pronosticos/Hojas de calculo/Actividad 3.6.xlsx")
Hoja<-Actividad_3_6

Hoja.ts <- ts(Hoja$Valor,start=1971,freq=12)
Hoja.ts[1:15]

# Siguiendo el procedimiento de guerreo
# Sea N=119,  se propone H=11, entonces dejamos fuera a n=9 y R=( (N-n)/H ) =10 por lo tanto n<R
# Serán así 11 grupos de tamaño 10

# Funciones para calcular varianza en serie de tiempo
grupos_serie<-function(Hoja.ts,R){
  N=length(Hoja.ts)
  #H=10
  Grupo <-c()
  ini=1
  ultimo=tail(seq(R,N,by=R),n=1)
  grupo<-'I'
  for (i in seq(R,N,by=R) ){
    Grupo[(i-R+1):i]<-grupo   # El +1 es por que el operador : es inclusivo de ambos lados
    grupo<-paste(grupo,'I',sep='')
  }
  valor<-as.numeric(Hoja.ts[1:ultimo])
  New_Hoja<-data.frame(valor,Grupo)
  New_Hoja
}

varianza_ts<-function(Hoja.ts,R){
  N=length(Hoja.ts)
  vari<-c()
  for (i in seq(R,N,by=R) ){ # 10 20 30 ....
    vari<-c( vari, var(Hoja.ts[(i-R+1):i]) )   # El +1 es por que el operador : es inclusivo de ambos lados
  }
  vari
}

varianza_ts_plot<-function(vari){
  plot.ts(vari,main='Varianza de Serie de tiempo',xlab='Grupo de tiempo',ylab='Varianza',col='red')  
}


R=10
Hoja_g<-grupos_serie(Hoja.ts,R)

varianza_ts(Hoja.ts,R)

varianza_ts_plot(varianza_ts(Hoja.ts,R))

# Hipótesis nula: Las varianzas son iguales(Homogeneas)
# Si el p-value es menor a la significacia se rechaza la hipótesis nula
leveneTest(Hoja_g$valor,group = Hoja_g$Grupo)
