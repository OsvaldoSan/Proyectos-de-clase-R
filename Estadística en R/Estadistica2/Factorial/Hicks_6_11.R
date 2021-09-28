# Script de pruebas para documentar en el archivo Marckdown
library(readxl)
factorial<- read_excel("Factorial1.xlsx")
attach(factorial)
factorial$Laboratorio<-factor(factorial$Laboratorio)
factorial$Temperatura<-factorial$Temperatura
factorial$Mix<-factor(factorial$Mix)
Observaciones<-factorial$Observaciones
Laboratorio<-factor(factorial$Laboratorio)
Temperatura<-factor(factorial$Temperatura)

Mix<-factor(factorial$Mix)
Observaciones<-factorial$Observaciones
Laboratorio<-(factorial$Laboratorio)
Temperatura<-(factorial$Temperatura)


mod3<-aov(Observaciones~Laboratorio+Temperatura+Mix+Laboratorio*Temperatura+
            Laboratorio*Mix+Temperatura*Mix+Laboratorio*Temperatura*Mix)
summary(mod3)

#---------------------alternativa con lm
m3<-lm(Observaciones~(Laboratorio+Mix+Temperatura)^3)
mod3<-aov(m3)
summary(mod3)

#----continuacion



plot.design(factorial, fun="mean", main=" Gráfica de efectos principales", ylab= "Duración", xlab="Factor")

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
ggplot(data = factorial, aes(x = Laboratorio, y = Observaciones, colour = Mix,
                             group = Mix)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Observaciones)') +
  theme_bw()
#post hoc

pairwise.t.test(x = factorial$Observaciones, g = factorial$Temperatura, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")
pairwise.t.test(x = factorial$Observaciones, g = factorial$Mix, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")
pairwise.t.test(x = factorial$Observaciones, g = factorial$Laboratorio, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

#Estadistica
hist(Observaciones)
hist(1/Observaciones)
obs<-(1/log(Observaciones))
hist(obs)
hist(factorial$Observaciones)
factorial$Observaciones<-Observaciones
shapiro.test(1/Observaciones)
qqnorm(1/Observaciones)

## box cox
library(MASS)#para box cox
obs<-log10(Observaciones)
m0<-lm(obs~(Laboratorio+Temperatura+Mix)^3)
am0<-aov(m0)
summary(am0)
plot(am0)

bptest(am0)
bptest(mod3)
shapiro.test(am0$residuals)
chisq.test(log10(Observaciones))

bc<-boxcox(am0, lam = seq(-1, 1, 1/10))
abline(bc,v=-0.6)
lambda <- bc$x[which.max(bc$y)]

m3<-lm(((Observaciones^lambda-1)/lambda) ~ (Laboratorio+Temperatura+Mix)^3)
am3<-aov(m3)
summary(am3)

obs<-Observaciones^lambda
m1<-lm(Observaciones^lambda~(Laboratorio+Temperatura+Mix)^3)
am1<-aov(m1)
summary(am1)
summary(am0)
qqnorm(am1$residuals)
qqline(am1$residuals)
shapiro.test(am1$residuals)
bptest(am3)
chisq.test(obs)
hist(obs)
pairwise.t.test(x = obs, g = factorial$Laboratorio, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")
pairwise.t.test(x = obs, g = factorial$Mix, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")
pairwise.t.test(x = obs, g = factorial$Temperatura, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")
Temperatura<-factor(Temperatura)
factorial$Temperatura<-Temperatura

## cajas
ggplot(data=factorial,aes(x=Mix,y=Observaciones,color=Temperatura))+
  geom_boxplot() +
  theme_bw()+
  labs(y="Observaciones")
ggplot(data=factorial,aes(x=Laboratorio,y=Observaciones,color=Temperatura))+
  geom_boxplot() +
  theme_bw()+
  labs(y="Observaciones")

ggplot(data=factorial,aes(x=Mix,y=Observaciones,color=Laboratorio))+
  geom_boxplot() +
  theme_bw()+
  labs(y="Observaciones")

ggplot(data=factorial,aes(x=Temperatura,y=Observaciones,color=Laboratorio))+
  geom_boxplot() +
  theme_bw()+
  labs(y="Observaciones")

ggplot(data=factorial,aes(x=Mix,y=obs,color=Laboratorio))+
  geom_boxplot() +
  theme_bw()+
  labs(y="Observaciones")

ggplot(data=factorial,aes(x=Temperatura,y=obs,color=Laboratorio))+
  geom_boxplot() +
  theme_bw()+
  labs(y="Observaciones")



