# Primeras pruebas para obtener el mejor análisis de los campeones europeos

library(readxl)
#Datos <- read_excel("Datos.xlsx", sheet = "Sheet1", range = "A14:C32")
Datos <- read_excel("Datos.xlsx", sheet = "Sheet1", range = "M14:O30")
Datos$Intervalo<-factor(Datos$Intervalo)
View(Datos)
Puntos<-Datos$Puntos
Intervalo<-factor(Datos$Intervalo)
Bloque<-factor(Datos$Bloque)
 

#ANOVA
modelo<-aov(Puntos~Intervalo+Bloque,Datos)
summary(modelo)
plot(modelo)  

#supuestos
library(lmtest)
qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)
chisq.test(Datos$Puntos)
plot(modelo)
bptest(modelo)
?bptest
#
library(car)
nuevo_modelo<-Anova(modelo,white.adjust = TRUE)
summary(nuevo_modelo)
nuevo_modelo
bptest(nuevo_modelo)


#nonparametric
library(WRS2)

pbad2way(Puntos ~ Intervalo + Bloque+ Intervalo:Bloque,data = Datos,est = "median",
         nboot = 12) 

pbad2way(formula=modelo, data = Datos, est = "mom", nboot = 599, pro.dis = FALSE)
?pbad2way
#sandwich
library(sandwich)
mod=lm(Puntos~Intervalo+Bloque,data=Datos,na.action = na.omit)
s<-sqrt(diag(vcovHC(mod)))
s
anova_mod<-aov(s)
summary(anova_mod)
bptest(anova_mod)
#mod = lm ( Alertness  ̃ Dosage , data =1, na . action = na . omit )
#PostHoc
Tuckey
?friedman.test()
f<-friedman.test(Puntos,groups = Intervalo,blocks = Bloque,
              Puntos~Intervalo|Bloque,Datos)
summary(f)
f
#cosas raras
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("mixOmics")
install.packages("RVAideMemoire")
perm.anova(Puntos~Intervalo | Bloque,nperm = 49)
  

#Optimización
library(dplyr)
(x <- matrix(1:9/12, 3, 3) %>% fractional)

library(MASS)
library(dplyr)
x=5
xr<-1/x %>% fractional
xr
x<-c(2,3,4,5,6,7,8,9)
x<-(4/x) %>% fractional

x<- 4*x
x<-x-1
x<-x*-1
x<-x*(3/4)
x
