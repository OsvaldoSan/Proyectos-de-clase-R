# Simulacion de caminata aleatoria -----
# 100 obsesrvaciones de 1 y -1 con igual probabilidad de aparecer, con inicio en 0
x=rbinom(100,1,0.5)
for (i in 1:length(x)){
  if(x[i]==0){
    x[i]=-1
  }
}
x[1]=0
x
w=cumsum(x)
w
plot.ts(w,col='cyan')
## Versión en programación funcional 
set.seed(1237430)
transform_to_minus1<-function(x) if(x==0) -1 else x
x=rbinom(100,1,0.5)
x_n=sapply(x,transform_to_minus1)
w=cumsum(x_n)
plot.ts(w,col='green')
# Simulación de ruido blanco ---------------------------------
# Ruido blanco gaussiano
ruido_blanco=rnorm(300,0,1)
ruido_blanco
plot.ts(ruido_blanco,main='Ejemplo de ruido blanco',col='red',xlab='Tiempo',ylab='Valores')
