#Factores-----
##Renglon
strip<-c("I","II","III","IV","V","I","II","III","IV","V","I","II","III","IV","V","I","II","III","IV","V","I","II","III","IV","V")
##Columna
posicion<-c("1","1","1","1","1","2","2","2","2","2","3","3","3","3","3","4","4","4","4","4","5","5","5","5","5")
#Letras Latinas
electrodos<-c("A","E","D","C","B","B","A","E","D","C","C","B","A","E","D","D","C","B","A","E","E","D","C","B","A")
#Observaciones
time<-c(3.5,2.6,2.9,2.5,2.1,2.1,3.3,2.6,2.9,2.3,2.5,2.1,3.5,3.0,3.7,3.5,2.5,2.7,3.3,3.2,2.4,2.7,2.9,2.3,3.5)

# convertir a tipos factor
strip<-factor(strip)
posicion<-factor(posicion)

datos<-data.frame(strip=strip,posicion=posicion,electrodos
                  =electrodos,time=time)
#Ejecutar anova
anova_cl<-aov(time~strip+posicion+electrodos,datos)
summary(anova_cl)
# post-hoc----
pairwise.t.test(x = datos$time, g = datos$electrodos, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")
# supuests-----
shapiro.test(anova_cl$residuals)#normalidad
bptest(anova_y)#homocedasticidad
chisq.test(datos_y$time_y)#independencia

# Youden --------
posicion_y<-c("1","1","1","1","1","2","2","2","2","2","3","3","3","3","3","4","4","4","4","4")
## Renglon
strip_y<-c("I","II","III","IV","V","I","II","III","IV","V","I","II","III","IV","V","I","II","III","IV","V")
# Letras Latinas
electrodos_y<-c("A","E","D","C","B","B","A","E","D","C","C","B","A","E","D","D","C","B","A","E")
# Observaciones
time_y<-c(3.5,2.6,2.9,2.5,2.1,2.1,3.3,2.6,2.9,2.3,2.5,2.1,3.5,3.0,3.7,3.5,2.5,2.7,3.3,3.2)
posicion_y<-factor(posicion_y)

datos_y<-data.frame(strip_y=strip_y,posicion_y=posicion_y,electrodos_y
                   =electrodos_y,time_y=time_y)
anova_y<-aov(time_y~electrodos_y+strip_y+posicion_y,datos_y)
summary(anova_y)
drop1(anova_y,test = "F")
?drop1
pairwise.t.test(x = datos_y$time_y, g = datos_y$electrodos_y, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

leveneTest( anova_cl$residuals~anova_cl$fitted.values,anova_cl,center = "median")
leveneTest(time ~ electrodos,datos,center = "median")
