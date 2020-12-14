##Procesos de estimación de parámetros

###Distribución normal

tamaño_muestral <- 35
iteraciones <- 100


medias <- vector()
desv_est <- vector()

media_poblacional <- 3
desv_est_poblacional <- 5

##Comenzar la interación

for (i in seq_len(iteraciones)) {
  muestra <- rnorm(tamaño_muestral, media_poblacional, desv_est_poblacional)
  medias[i] <- mean(muestra)
  desv_est[i] <- sd(muestra)
}


##Graficar

plot(medias, desv_est)
points(media_poblacional, desv_est_poblacional, col = 2, cex = 3, pch = 20)

###Regresión Lineal

tamano_muestral <- 350
iteraciones <- 100

beta_0 <- -1
beta_1 <- -0.3

beta_0_estimado <- vector()
beta_1_estimado <- vector()

generar_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

for (i in seq_len(iteraciones)) {
  X <- seq(-3, 3, length.out = tamano_muestral)
  Y <- generar_y(X, beta_0, beta_1)
  betas_estimados <- coef(lm(Y ~ X))
  beta_0_estimado[i] <- betas_estimados[1]
  beta_1_estimado[i] <- betas_estimados[2]
}

##gráfica
plot(beta_0_estimado, beta_1_estimado)
points(beta_0, beta_1, col = 2, cex = 3, pch = 20)