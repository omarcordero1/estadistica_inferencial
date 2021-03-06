##Explorando Datos Simulados


##Simular dartos normales

Y <- rnorm(100)
plot(density(Y))

##Simular dartos normales con media 5 y sd 3

Y <- rnorm(100, 5, 3)
plot(density(Y))

##Distribuci�n uniforme
Y <- runif(100)
plot(density(Y))

#Distribuci�n uniforme a= 3, b= 8

Y <- runif(100, 3, 8)

plot(density(Y))

###Edad y lugar

data.frame(
  Edad = rnorm(50, 10, 1.2),
  Lugar = "Escuela"
) -> escuela

data.frame(
  Edad = rnorm(45, 15, 1.9),
  Lugar = "Preparatoria"
) -> prepa

data.frame(
  Edad = rnorm(80, 21, 2.5),
  Lugar = "Universidad"
) -> universidad

rbind(escuela, prepa, universidad) -> edad_lugar

##Graficar

boxplot(Edad ~ Lugar, data = edad_lugar)

##Modelo l�neal
X <-seq(0, 3*pi, length.out = 100)
Y <- -0.3*X + 1 + rnorm(100,0,0.5)
Z <- -0.3*X + 1

data.frame(X,Y,Z) -> datos_lineal


###Graficar 
plot(Y ~ X, data = datos_lineal )
lines(Z ~ X, data = datos_lineal, col = 2, lwd = 2)


##Modelo no lineal

X <-seq(0, 3*pi, length.out = 100)
Y <- cos(X) + rnorm(100,0,0.5)
Z <- cos(X)

data.frame(X,Y,Z) -> datos_no_lineal

##Gr�fico

plot(Y ~ X, data = datos_no_lineal )
lines(Z ~ X, data = datos_no_lineal, col = 2, lwd = 2)

