#---------------------------#
# Ejemplo: modelo Bernoulli, código de el Dr. Arturo Erdely #
#---------------------------#

# Supongamos que se tienen dos monedas dentro de una
# urna, una equilibrada y la otra cargada con 
# probabilidad de que salga "?guila" igual a 3/4. Se
# escoge una de las monedas al azar, y con ella se
# lanzar?n n volados.

# Con base en la muestra resultante se desea hacer
# inferencia bayesiana sobre la moneda utilizada.

# Se aplica el modelo Bernoulli con la convenci?n de que
# el valor 1 corresponde al resultado "?guila" (?xito) y
# el valor 0 corresponde al resultado "sol" (fracaso).


# Función de masa de probabilidades Bernoulli:

?dbinom # documentaci?n para entender esta funci?n

# definir funci?n:
f <- function(x, th) dbinom(x, size = 1, prob = th) 


# Espacio param?trico

Theta <- c(1/2, 3/4)
Theta


# Distribuci?n a priori
?in
# definir funci?n:
priori <- function(th) (1/2)*(th %in% Theta) 

priori(c(1/4, 1/2, 3/4, 7/8)) # ejemplos


# Simular una muestra con theta = 1/2

n <- 30 # tama?o de muestra
theta <- 1/2 # valor del par?metro
# simular muestra:
muestra <- rbinom(n, size = 1, prob = theta) 
muestra # muestra obtenida
sum(muestra) # suma de ?xitos
mean(muestra) # proporci?n de ?xitos


# Verosimilitud

# verosimilitud de que par?metro = 1/2
prod(f(muestra, 1/2)) 
# verosimilitud de que par?metro = 3/4
prod(f(muestra, 3/4)) 

# definir funci?n de verosimilitud
L <- function(th) prod(f(muestra, th)) 


# Distribuci?n a posteriori

Theta # espacio param?trico
sapply(Theta, L) # verosimilitudes
priori(Theta) # probabilidades a priori
sapply(Theta, L)*priori(Theta)

# definir funci?n:
post <- function(th) L(th)*priori(th)/sum(sapply(Theta, L)*priori(Theta))

Theta # espacio param?trico
sapply(Theta, post) # probabilidades a posteriori
sum(sapply(Theta, post)) # verificando que suman 1


# An?lisis secuencial de probabilidad a posteriori
# para theta = 1/2

# crear vector de probabilidades:
post.secuencial <- numeric(n + 1) 
# prob a priori de par?metro = 1/2
post.secuencial[1] <- priori(1/2) 
muestra2 <- muestra # guardando la muestra original
for (i in 2:(n + 1)){
  muestra <- muestra2[1:(i - 1)]
  # prob a posteriori de par?metro = 1/2
  post.secuencial[i] <- post(1/2) 
}
muestra <- muestra2 # restableciendo muestra original
muestra
rm(i, muestra2)
round(post.secuencial, 5) # probabilidades a posteriori

# matriz secuencial de probabilidades a posteriori
matriz.post <- matrix(nrow = n + 1, ncol = 2)
colnames(matriz.post) <- c("muestra", "prob.post.1/2")
# tama?o acumulado de muestra:
rownames(matriz.post) <- as.character(0:n) 
matriz.post[2:(n + 1), 1] <- muestra
matriz.post[ , 2] <- post.secuencial
matriz.post

# gr?fica secuencial de probabilidades a posteriori
plot(c(0, n), c(0, 1), main = "Prob posteriori theta = 1/2",
     xlab = "tama?o de muestra", ylab = "probabilidad",
     type = "n")
abline(h = 0, col = "gray")
abline(h = 1, col = "gray")
points(0:n, post.secuencial)
lines(0:n, post.secuencial)

lineas <- function(k) abline(v = k, col = "gray")
basura <- sapply(0:n, lineas)
# agregar muestra al gr?fico:
text(0:n, 0.4, c(NA, muestra)) 


# Simular una muestra con theta = 3/4

n <- 30 # tama?o de muestra
theta <- 3/4 # valor del par?metro
# simular muestra:
muestra <- rbinom(n, size = 1, prob = theta) 
muestra # muestra obtenida
sum(muestra) # suma de ?xitos
mean(muestra) # proporci?n de ?xitos


# An?lisis secuencial de probabilidad a posteriori
# para theta = 1/2

# crear vector de probabilidades:
post.secuencial <- numeric(n + 1) 
# prob a priori de par?metro = 1/2:
post.secuencial[1] <- priori(1/2) 
muestra2 <- muestra # guardando la muestra original
for (i in 2:(n + 1)){
  muestra <- muestra2[1:(i - 1)]
  # prob a posteriori de par?metro = 1/2:
  post.secuencial[i] <- post(1/2) 
}
muestra <- muestra2 # restableciendo muestra original
rm(i, muestra2)
muestra
round(post.secuencial, 5) # probabilidades a posteriori

# matriz secuencial de probabilidades a posteriori
matriz.post <- matrix(nrow = n + 1, ncol = 2)
colnames(matriz.post) <- c("muestra", "prob.post.1/2")
# tama?o acumulado de muestra:
rownames(matriz.post) <- as.character(0:n) 
matriz.post[2:(n + 1), 1] <- muestra
matriz.post[ , 2] <- post.secuencial
matriz.post

# gr?fica secuencial de probabilidades a posteriori
dev.new()
plot(c(0, n), c(0, 1), main = "Prob posteriori theta = 1/2",
     xlab = "tama?o de muestra", ylab = "probabilidad", 
     type = "n")
abline(h = 0, col = "gray")
abline(h = 1, col = "gray")
points(0:n, post.secuencial)
lines(0:n, post.secuencial)

basura <- sapply(0:n, lineas)
text(0:n, 0.6, c(NA, muestra))

