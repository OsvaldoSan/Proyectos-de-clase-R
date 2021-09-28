# Script para probar comandos para el diseño de exprimentos y ANOVA del archivo R marckdwn

#-------------------librerias----------------------
library(ggplot2)
library(GGally)
require(nortest)
#----datos-------
#recubrimiento <-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
recubrimiento <-c('I','I','I','I','I','II','II','II','II','II','III','III','III','III','III','IV','IV','IV','IV','IV')
observaicones <-c(56.0,55,62.0,59,60,64,61,50,55,56,45,46,45,39,43,42,39,45,43,41)#lechugas
anova<-aov(observaicones~recubrimiento)
#anova<-aov(recubrimiento~observaicones)
datos<-data.frame(recubrimiento=recubrimiento,observaicones= observaicones)
#analisis de datos---------------------------------
table(datos$recubrimiento)
aggregate(observaicones ~ recubrimiento, data = datos, FUN = mean)
aggregate(observaicones ~ recubrimiento, data = datos, FUN = sd)
#en group es necesario colocar el tratamiento para que agrupe en funcion del tratamiento en caso de que recubrimiento sea numero
#si recubirmiento es caracter se hace la tabla bonita
ggplot(data=datos,aes(x=recubrimiento,y=observaicones,color=recubrimiento))+
    geom_boxplot() +
    theme_bw()+
    labs(y="Observaciones")
summary(anova)
?ggplot
#supuestos a valores------------------------------------------------------------
shapiro.test(nuevo_ajuste$residuals)
require(nortest)#para kolmogorov-smirnov con ajuste de lillie
by(data = datos,INDICES = datos$recubrimiento,FUN = function(x){ shapiro.test(x$observaicones)})

fligner.test(observaicones~recubrimiento)

require(car)
leveneTest(observaicones ~ recubrimiento,datos,center = "median")

#poc-hoc...holm-banferroni
pairwise.t.test(x = datos$observaicones, g = datos$recubrimiento, p.adjust.method = "holm"
                ,pool.sd = TRUE, paired = FALSE, alternative = "two.sided")
