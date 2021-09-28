# Pruebas de comandos para el análisis
library(descomponer)
# Datos: Carga directa de serie de tiempo ----
# Cantidad de pasajeros por mes en estados unidos en miles
data("AirPassengers")
AP<-AirPassengers
summary(AP)
AP
plot(AP,col='red',xlab='Tiempo',ylab='Thousands',main='Air Passengers')
AP
xtable(AP, digits = 0)

cycle(AP)
## Convertir cycle(AP) a caracteres y en forma de factor para poder aplicar el vector de colores
Month <-  factor(cycle(AP), levels = 1:12, labels = month.abb)
myColors <- ifelse(levels(Month)== 'Jan', "cyan" , 
                   ifelse(levels(Month)=='Jul', rgb(0.8,0.1,0.3,0.6),
                          ifelse(levels(Month)=='Aug','red',"gray90" )
                          ) )
boxplot(AP ~ Month,xlab = 'Meses',col=myColors)



myColors
# Análisis de autocorrelación
ac<-acf(AP)
# Aparecen las autocorrelaciones con lags de diferencia de acuerdo a los meses, la segunda autocorrelacion
# correspone al segundo mes y así sucesivamente
ac
plot(ac)
#Los punteos son por (-1/n)-(2/sqrt(n))
#! Pendiente: Intervalos de confianza

# La autocorrelación parcial no se evalua con un lag=0 
pac<-pacf(AP)
pac
plot(pac)

# Periodograma
# Frecuencias $\lambda_{j}=\frac{2\pi j}{T}$

per<-periodograma(AP)
per
gperiodograma(AP)
gtd(AP)
plot.ts(periodograma_integrado(AP))
