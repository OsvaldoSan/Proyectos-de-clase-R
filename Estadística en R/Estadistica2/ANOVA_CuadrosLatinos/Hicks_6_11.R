# Script para probar ejecuciones para el problema 6-11 de Hicks
library(readxl)
factorial<- read_excel("Factorial1.xlsx")
factorial$Laboratorio<-factor(factorial$Laboratorio)
factorial$Temperatura<-factorial$Temperatura

mod3<-aov(Observaciones~Laboratorio+Temperatura+Mix+Laboratorio*Temperatura+
            Laboratorio*Mix+Temperatura*Mix+Laboratorio*Temperatura*Mix,data = factorial)
Observaciones<-factorial$Observaciones
Laboratorio<-factor(factorial$Laboratorio)
Temperatura<-factor(factorial$Temperatura)
Mix<-factorial$Mix
summary(mod3)
interaction.plot(Laboratorio,Temperatura,Mix,Observaciones,col = 1:4,fun = "mean")
warnings()
ggplot(data = factorial, aes(x = Temperatura, y = Observaciones, colour = Laboratorio,
                         group = Laboratorio)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Observaciones)') +
  theme_bw()
ggplot(data = factorial, aes(x = Temperatura, y = Observaciones, colour = Mix,
                             group = Mix)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Observaciones)') +
  theme_bw()
