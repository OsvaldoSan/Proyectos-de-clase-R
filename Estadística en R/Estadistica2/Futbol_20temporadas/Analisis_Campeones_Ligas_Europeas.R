# Análissi para saber si hay diferencia estadística entre los puntocs conseguidos por los
# campeones de las principales ligas europeas desde 1980 hasta 2020

#Carga de datos
library(readxl)
Datos <- read_excel("Datos.xlsx", sheet = "Sheet1", range = "M14:O30")
Datos$Intervalo<-factor(Datos$Intervalo)
Puntos<-Datos$Puntos
Intervalo<-factor(Datos$Intervalo)
Bloque<-factor(Datos$Bloque)
View(Datos)
#Caja
library(ggplot2)
ggplot(data=Datos,aes(x=Intervalo,y=Puntos,color=Intervalo))+
  geom_boxplot() +
  theme_bw()+
  labs(y="Observaciones")


#ANOVA
modelo<-aov(Puntos~Intervalo+Bloque,Datos)
summary(modelo)


#supuestos
library(lmtest)
qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)
chisq.test(Datos$Puntos)
plot(modelo)
bptest(modelo)
?bptest


#Friedman test
?friedman.test()
friedman.test(Puntos,groups = Intervalo,blocks = Bloque,
                 Puntos~Intervalo|Bloque,Datos)
  