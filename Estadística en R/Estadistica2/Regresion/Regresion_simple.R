library(readxl)
Datos_14_16 <- read_excel("Datos_14_16.xlsx")
View(Datos_14_16)
#grafica
pairs(Datos_14_16)
plot(Datos_14_16$Periodo_2016)
#correlacion
cor(Datos_14_16)
#significancia
#H0 no existe correlacion 
cor.test(x = Datos_14_16$Periodo_2014,
         y = Datos_14_16$Periodo_2016, 
         alternative = "two.sided",
         conf.level  = 0.95,
         method      = "pearson")
#Coeficiente de determinacion R²
R2_pearson <- cor(x =  Datos_14_16$Periodo_2014,
                  y = Datos_14_16$Periodo_2016,
                  method = "pearson")
R2_pearson<-R2_pearson^2
R2_pearson
#lm crea un modelo lineal donde P2016 depende de P2014
regresion<- lm(Periodo_2016 ~ Periodo_2014, data=Datos_14_16)
summary(regresion)
plot(Datos_14_16$Periodo_2014,Datos_14_16$Periodo_2016,
     xlab="2014",ylab = "2016")#muestra los datos
abline(regresion)#linea recta
mean(Datos_14_16$Periodo_2016)
