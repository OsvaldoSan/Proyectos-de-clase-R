# Ejemplos de estadística no parametrica

library(dplyr)
require(coin)#libreria para wilcoxsign_test
despues<- select(Datos, 'sujeto 1') %>% unlist
antes<- select(Datos, 'sujeto 2') %>% unlist
datos <- data.frame(antes=antes, despues=despues)
wilcoxsign_test(antes ~ despues, data = datos, distribution = "exact")
wilcoxsign_test(antes ~ despues, data = datos,
                distribution = "asymptotic",conf.level=0.95)
wilcox.test(x = antes, y = despues, alternative = "two.sided",
            mu = 0, paired = TRUE
            ,conf.level = 0.95,conf.int=TRUE)
#---Ejemplo 1
antes_ej2<-c(147.0,183.5,232.1,161.6,197.5,206.3,177.0,215.4,147.7,208.1,166.8,131.9,150.3,
             197.2,159.8,171.7)
despues_ej2<-c(137.9,176.2,219,163.8,193.5,201.4,180.6,203.2,149,195.4,158.5,134.4,
               149.3,189.1,159.1,173.2)
wilcox.test(x = antes_ej2, y = despues_ej2, alternative = "two.sided", mu = 0, paired = TRUE
            ,conf.level = 0.95,conf.int=TRUE)

# Ejemplo 2

X<-select(Datos4,x) %>% unlist
Y<-select(Datos4,Y) %>% unlist
wilcox.test(x=Y, y=X, alternative = "two.sided",mu=0,
            pired=FALSE,conf.int =0.918 )
#La prueba no es pareada
#su W es nuestra U en la formula de los documentos del profesor
#prueba con z
datos<- data.frame(grupo=rep(c("A","B") , c(10,10))
                   , valores= c(X,Y))
wilcox_test(valores ~ grupo, data=datos,
            distribution="exact",conf.int=0.96, alternative)
