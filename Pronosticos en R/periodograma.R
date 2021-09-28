# Prueba de Periodograma
# Librerias
# Enlace a la descripción https://www.rdocumentation.org/packages/descomponer/versions/1.5
library(descomponer)

# Lectura de datos
www <- "bookwebfiles_Cowpertwait, Metcalfe/global.dat"# Esta es una serie que yo descargue, se pude usar cualquiera
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(1870, 12),
                fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
Global.annual
plot(Global.ts)
Global.ts
plot(Global.annual)

Global.ts_per<-periodograma(Global.ts)
gperiodograma(Global.ts)# Con esto se gráfica el periodograma


# Función ---------------------
# No se si haya otra función en el mismo paquete de descomponer para graficar el periodograma integrado
periodograma_integrado<-function(serie){
  serie_per<-periodograma(serie)
  dense_serie<-serie_per[4]
  n=dim(dense_serie)[1]
  ST<-sum(dense_serie)
  sub_sum <- function(i,periodograma_){
    sum=0
    for (j in 1:i){
      sum = sum+periodograma_[j,]
    }
    sum 
  }
  S_i<-list()
  for (i in 1:n){
    S_i[i]<-sub_sum(i,dense_serie)/ST
  }
  S_i
}
library(readxl)
datos1<-read_excel("MEGAsync/Pronosticos/Practicas_Calc/datos1.xlsx")
datos1.ts<-ts(datos1[2],start=1,frequency = 1)
datos1.ts
plot(datos1.ts)
#Periodograma
gperiodograma(datos1.ts)
#Periodograma integrado
S_i<-periodograma_integrado(datos1.ts)
plot.ts(S_i,col='cyan',ylab='Cociente',main="Periodograma integrado")

#Periodograma integrado con limites para test
gtd(datos1.ts)

td(datos1.ts)
gdt(datos1.ts)
