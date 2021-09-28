# Experimento de compartamiento bayesoano

f<-function(x,th) dunif(x,min = 0, max = th)
f(1.5,2)

#Definimos el espacio parametrico
Theta<-c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2)

priori<-function(th) 2*(th-1)#*(th %in% Theta)
integrate(priori,1,1.5)

#Muestra
muestra<-function(n,th)runif(n,min=0,max = th)
muestra(30,1.5)
#b)
#(1/(2-log(4)))
n<-30
w<-function() 2*( (( 2^(-n+2) )/(-n+2)) - (( 1 )/(-n+2)) - (( 2^(-n+1) )/(-n+1)) + (( 1 )/(-n+1))  )

posteriori<-function(th) ( (2*(th-1)) / (th^n) ) * (1/w())
integrate(posteriori,1,1.1)

posteriori(2,3)#ejemplo
  
predic_post<- function(x,n) ((((2^(-n+2))-2)/(-n+1))+(((2^(-n+1))-2)/n))*(1/(2-log(4)))
predic_post(0.9,3)

library(dplyr)
library(fractional)
library(expm)
v<-c(0,1,0,0.1,0.15,0,0,
     0,0,1,0,0.15,0,0,
     1,0,0,0,0,0,0,
     0,0,0,0.2,0.1,0.3,0.4,
     0,0,0,0,0.1,0.7,0.6,
     0,0,0,0.2,0.5,0,0,
     0,0,0,0.5,0,0,0)
a<-matrix(v,ncol=7,nrow=7)
a%^%10000000
qv<-c(0.7,-0.4,-0.7,0.6)
qmat<-matrix(qv,ncol = 2,nrow = 2)
qmat
inv_qmat<-solve(qmat)
inv_qmat

v<-c(0.8,0,0.5,0,0.2,0.2,0.5,0.4,0,0,0,0.5,0.1,0,0,0,0,0,0.3,0.4,0,0,0,0.7,0.4)
mat<-matrix(v,ncol=5,nrow=5)
mat%^%100000000


