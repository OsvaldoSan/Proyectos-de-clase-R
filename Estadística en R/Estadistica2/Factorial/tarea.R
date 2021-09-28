# Pequeño análisis sobre una tarea de la clase de estadística 2

tarea <- read.csv("~/MEGAsync/Estadistica/Estadistica_ll/R/Factorial/tarea.txt", sep="")
attach(tarea)
modelo<-lm(observaciones~(cristal+fosforo)^3)
modeloanova<-aov(modelo)
summary(modeloanova)

## ---Comentarios sobre el modelo-------

#Este anova con los datos sin codificar nos muestra que no hay diferencias significativas
#en las medias del cristal y fosforo lo cual difiere en gran manera de los datos codificados
#que los dan p-values significativos para estos factores, lo unico que tienen en comun el
#anova de los datos sin codificar y el anova de los datos codificados es que muestran que no hay
#interaccion significativa; estos resultados son algo perturbadores por que la codificacion de los datos
# hecha de una manera tan empirica lleva a diferencias muy grandes que podrian llegar a ser graves.