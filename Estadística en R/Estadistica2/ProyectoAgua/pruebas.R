# Pruebas para el modelo del Agua

library(readxl)
Datos_1<-read_excel("Datos_1.xlsx")
names(Datos_1)<-c("y","x1","x2","x3","x4","x5")
plot(Datos_1$x1,Datos_1$y)#casi con aglomeracion al principio
plot(Datos_1$x2,Datos_1$y)#mas dispersa
plot(Datos_1$x3,Datos_1$y)
plot(Datos_1$x4,Datos_1$y)
fit<-lm(y~.,Datos_1)

summary(fit)
ggpairs(Datos_1, lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"),title = "Graficas de dispersión")


plot(Datos_1$x1/Datos_1$y,Datos_1$x1)
abline( nuevo_ajuste, col = "#e74c3c" ) # añadir la recta
plot(Datos_1$y*40,nuevo_ajuste$fitted.values/20)
#Este es el metodo de seleccion de variables, nos arroja un modelo mas optimo en funion de todas las variables que tenemos
nuevo_ajuste<-step(object =fit,direction = "both",trace = 1 )
summary(nuevo_ajuste)
summary(fit)

nuevo_ajuste$coefficients[1]+(1300635*nuevo_ajuste$coefficients[2])+(15*nuevo_ajuste$coefficients[3])+(131472*nuevo_ajuste$coefficients[4])+(575*nuevo_ajuste$coefficients[5])+(3939*nuevo_ajuste$coefficients[6])

mean(Datos_1$y)/57670

#------------------linealidad post-------------------------
library(ggplot2)
library(gridExtra)#para graficas juntas
plot1 <- ggplot(data = Datos_1, aes(x1, nuevo_ajuste$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = Datos_1, aes(x2, nuevo_ajuste$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = Datos_1, aes(x5, nuevo_ajuste$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2, plot3)


#-----------------colinealidad
library(corrplot)
corrplot(cor(dplyr::select(Datos_1, x1,x2,x5)),
         method = "number", tl.col = "black",title = "Matriz de correlaciones",bg="black")
?corrplot
vif(nuevo_ajuste)
#----normalidad
qqnorm(nuevo_ajuste$residuals)
qqline(nuevo_ajuste$residuals)
shapiro.test(nuevo_ajuste$residuals)
#---homocedasticidad--
bptest(nuevo_ajuste)
library(lmtest)
#----no autocorrelacion
dwtest(nuevo_ajuste,alternative = "two.sided")

#...intervalos
val<-data.frame(Datos_1$x1[i]+600000,Datos_1$x2[i],Datos_1$x5[i])
names(val2)<-c("x1","x2","x5")
confint(nuevo_ajuste,alpha=0.01)
predict(nuevo_ajuste,val2,alpha=0.05,interval = "predict")
predict.lm(nuevo_ajuste)
?rbind
?data.frame
val<-data.frame(Datos_1$x1[19]+600000,Datos_1$x2[19],1326.08)#data para usar
val2<-data.frame(Datos_1$x1[j]+100000,Datos_1$x2[j],614.3)#data para usar

names()
