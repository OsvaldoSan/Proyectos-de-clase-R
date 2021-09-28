# Ejercicio rápido de factoriales

library(readxl)
datos <- read_excel("Datos.xlsx")
View(datos)
str(datos)
#Cambio de variables
Cristal <- datos$Cristal
Fosforo <- datos$Fosforo
Respuesta <- datos$Interaccion
names(datos)<-c("Fosforo","Cristal","Respuesta")
#Modelo
mod<-aov(Respuesta~Cristal*Fosforo,datos)
summary(mod)
#Interaccion
library(GGally)
library(ggplot2)
interaction.plot(Fosforo,Cristal, Respuesta, data = datos, col = 1:3, type = "b")
interaction.plot(Cristal, Fosforo, Respuesta, data = datos,col = 1:3, type = "b")

ggplot(data = datos, aes(x = Fosforo, y = Respuesta, colour = Cristal,
                         group = Cristal)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Respuesta)') +
  theme_bw()

ggplot(data = datos, aes(x = Cristal, y = Respuesta, colour = Fosforo,
                         group = Fosforo)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Respuesta)') +
  theme_bw()
