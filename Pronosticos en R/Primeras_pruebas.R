# Series de tiempo en R, primer acercamiento

# 1.4.1 Obtención de serie de tiempo directamente -------
data(AirPassengers)
AP<- AirPassengers
# Con class sabemos que tipo de dato es AP
class(AP)
start(AP);end(AP);frequency(AP)
#Funciones genericas aplicadas a AP
summary(AP)
plot(AP,ylab="Passengers (1000's)")
AP

aggregate(AP)
plot(aggregate(AP))
cycle(AP)
boxplot(AP ~ cycle(AP))

## Boxplot con colores ejemplo

###Create data
names <- c(rep("Maestro", 20) , rep("Presto", 20) , 
           rep("Nerak", 20), rep("Eskimo", 20), rep("Nairobi", 20), rep("Artiko", 20))
value <- c(  sample(3:10, 20 , replace=T) , sample(2:5, 20 , replace=T) , 
             sample(6:10, 20 , replace=T), sample(6:10, 20 , replace=T) , 
             sample(1:7, 20 , replace=T), sample(3:10, 20 , replace=T) )
data <- data.frame(names,value)

### Prepare a vector of colors with specific color for Nairobi and Eskimo
myColors <- ifelse(levels(data$names)=="Nairobi" , rgb(0.1,0.1,0.7,0.5) , 
                   ifelse(levels(data$names)=="Eskimo", rgb(0.8,0.1,0.3,0.6),
                          "grey90" ) )
class(data$names)
### Build the plot
boxplot(data$value ~ data$names , 
        col=myColors , 
        ylab="disease" , xlab="- variety -")
myColors

# 1.4.2 Serie de tiempo desde internet ---------------
help("attach")
www <- "Datos/Cowpertwait_Data/Maine.dat"
Maine.month <- read.table(www, header = TRUE)

attach(Maine.month)
class(Maine.month)
# Va a tomar la primera observación como si fuera del primer mes de 1996(En este caso podria omitirse el vector c)
# Y va a cambiar de tiempo cuando hayan pasado 12 observaciones
Maine.month.ts <- ts(unemploy,start = c(1996,1),freq=12)
Maine.month.ts

Maine.annual.ts <-aggregate(Maine.month.ts)/12
Maine.annual.ts

layout(1:2)
plot(Maine.month.ts,ylab = "Uneployed (%)")
plot(Maine.annual.ts, ylab="unemployed (%)")

# Extrae las observaciones de febrero unicamente a lo largo de los años
Maine.Feb <-  window(Maine.month.ts, start = c(1996,2), freq = TRUE)
Maine.Feb
Maine.Aug <- window(Maine.month.ts, start = c(1996,8), freq = TRUE)
Maine.Aug
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)

mean(Maine.month.ts)
mean(Maine.Feb)

Feb.ratio
Aug.ratio

www <- "bookwebfiles_Cowpertwait, Metcalfe/USunemp.dat"
US.month<- read.table(www,header=T)
attach(US.month)
US.month.ts <- ts(USun,start=c(1996,1),end=c(2006,10),freq=12)
layout(1:1)
plot(US.month.ts, ylab="unemployed (%)")

# 1.4.3 Multiple time series
www <- "bookwebfiles_Cowpertwait, Metcalfe/cbe.dat"
CBE <- read.table(www,header = T)
CBE[1:2,]
class(CBE)

Elec.ts <- ts(CBE[,3],start=1958,freq=12)
Beer.ts <- ts(CBE[,2],start=1958,freq=12)
Choc.ts <- ts(CBE[,1],start=1958,freq=12)

cbind(Elec.ts,Beer.ts,Choc.ts)
plot(cbind(Elec.ts,Beer.ts,Choc.ts))

# Intersección de series
AP.elec <- ts.intersect(AP,Elec.ts)
start(AP.elec)
end(AP.elec)
AP.elec[1:3,]

AP <- AP.elec[,1]; Elec <- AP.elec[,2]
layout(1:2)
plot(AP,
       main = "", ylab = "Air passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")

layout(1:1)
as.vector(AP)
as.vector(Elec)
plot(as.vector(AP), as.vector(Elec),
     xlab = "Air passengers / 1000's",
     ylab = "Electricity production / MWh")
abline(reg = lm(Elec ~ AP))
cor(AP,Elec)

# División por cuartos ------------------------------

www<- "bookwebfiles_Cowpertwait, Metcalfe/pounds_nz.dat"
Z <- read.table(www,header=T)
Z[1:4,]

Z.ts <-ts(Z,st=1991,fr=4)

plot(Z.ts,xlab = "time / years", ylab = "Quarterly exchange rate in $NZ / pound")

Z.92.96 <- window(Z.ts, start = c(1992, 1), end = c(1996, 1))
Z.96.98 <- window(Z.ts, start = c(1996, 1), end = c(1998, 1))

layout (1:2)
plot(Z.92.96,ylab="Exchange rate in $NZ/pound ", xlab="Time (years)")
plot(Z.96.98,ylab="Exchange rate in $NZ/pound ", xlab="Time (years)")

# Temperatura del mundo -----------------------
www <- "bookwebfiles_Cowpertwait, Metcalfe/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),
                  fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
Global.annual
plot(Global.ts)
plot(Global.annual)


New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
New.time <- time(New.series)
New.time
plot(New.series); abline(reg=lm(New.series ~ New.time))
