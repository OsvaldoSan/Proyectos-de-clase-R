
# Cargar datros

# Crear muestra
edades<-round(runif(20,18,30))

# Mostrar datos
frecuencia<-transform(table(edades),Rel_freq=prop.table(Freq),Acumulado=cumsum(Freq))

# https://cosmosweb.champlain.edu/people/stevens/WebTech/R/Chapter-3-R.pdf
# https://statsandr.com/blog/descriptive-statistics-in-r/#contingency-table


p
# Creación de frecuencia de clase
datos_s=sort(datos)
clase=1
frecuencia_c=integer(nrow(p))
p[clase,]$lim_inferior
for (i in datos_s){
  if((i >= p[clase,]$lim_inferior) && (i < p[clase,]$lim_superior)){
    frecuencia_c[clase]=frecuencia_c[clase]+1
  }
  else{
    clase=clase+1
    frecuencia_c[clase]=frecuencia_c[clase]+1
  }
    
}

#Calcular frecuencia relativa
tot=sum(frecuencia_c)
frecuencia_rel=numeric(nrow(p))
for (i in 1:length(frecuencia_c)){
  frecuencia_rel[i]=frecuencia_c[i]/tot
}

tabla_final=cbind(p,fc=frecuencia_c,frc=frecuencia_rel,Fc=cumsum(frecuencia_c),Frc=cumsum(frecuencia_rel))


cut(datos, breaks = 8, right = FALSE,labels = tabla_final$Xc)
mediana_agrupados<-function(datos,tabla_final){
  n=length(datos)
  for (i in 1:nrow(tabla_final) ){
    if(tabla_final$Frc[i]>=0.50){
      Lc=tabla_final$lim_inferior[i]
      A=tabla_final$lim_superior[i]-Lc
      nc=tabla_final$fc[i]
      if((i-1)>=1){
        Nc=tabla_final$Frc[i-1]
      }
      else{
        Nc=0
      }
    break
    }
  }
  M=Lc+(A*(((n/2)-Nc)/nc))
  M
}


mean(abs(datos-mean(datos)))
