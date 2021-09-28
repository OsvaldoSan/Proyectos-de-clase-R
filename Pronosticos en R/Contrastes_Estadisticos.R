# Librerias
library(zoo) # para rollmean
# Libreria para errores
library(Metrics) # MAE
library(MLmetrics) # MAPE,R2_Score
library(stats) # Criterios de información

# Carga de datos
gas <- scan('http://verso.mat.uam.es/~joser.berrendero/datos/gas6677.dat')
plot(gas)

gas.ts = ts(gas, start = c(1966,1), frequency = 12)
print(gas.ts)

mean_val1<-rollmean(gas.ts,5,align = "right")
gas.obs<-gas.ts[5:140]

## Errores
error<-function(observed,predicted){
  observed-predicted
}

gas_error<-error(gas.obs,mean_val1)
# ME
ME<-mean(error(gas.obs,mean_val1))
ME

# MAE
mae_gas<-mae(gas.obs,mean_val1)
mae_gas

# MSE-ECM

gas_mse<-mse(gas.obs,mean_val1)
gas_mse

gas_errEst<-sqrt(gas_mse)
gas_errEst

# Percentage error o error relativo/porcentual
PE<-function(observed,predicted){
  n=length(observed)
  res=c()
  for(i in 1:n){
    res[i]=((observed[i]-predicted[i])/observed[i])*100
  }
  res
}

gas_pe<-PE(gas.obs,mean_val1)
gas_pe

gas_pme<-mean(gas_pe) # PME
gas_pme


# MAPE  Mean absolute percentage error
gas_mape<-MAPE(mean_val1,gas.obs)
gas_mape

# R-squared

gas_r2<-R2_Score(y_pred = mean_val1,gas.obs) # In MLmetrics

adj


# Criterios de información
model <- arima(x=gas.ts, order=c(1,0,1))
model

AIC(model)

bic<-AIC(model,k = log(length(sunspots)))
bic

# Según varias Fuentes bic es igual a sic

sic<-function(k,error)#n=T k grados de libertad, según guia de clase
{
  T=length(error)
  suma=0
  for (i in 1:T){
    suma=suma+error[i]^2
  }
  suma=suma/T
  u=T^(k/T)
  u*suma
} # Nota importante: No se cuales deben ser los grados de libertad

sic(1,gas_error)

