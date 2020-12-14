###Bootstraping

tamano_muestral <- 23
iteraciones <- 100
beta_0 <- 1
beta_1 <- -0.3
desv_est_error <- 0.5

##Generación de variable independiente

genera_x <- function(n){seq(-3, 3, length.out = n)}
genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, desv_est_error)
}

##Datos

datos_x <- genera_x(tamano_muestral)
datos_y <- genera_y(datos_x, beta_0, beta_1)

##Regresión líneal

modelo <- lm(datos_y ~ datos_x)

##Coeficientes muestrales

coeficientes_muestrales <- coefficients(modelo)

##Intervalo de confianza
confint(modelo)

##Simulación
beta_0_estimado <- beta_0_estimado <- vector()

for(i in seq_len(iteraciones)){
  muestra <- sample(seq_along(datos_x), length(datos_x), replace = TRUE)
  muestra_x <- datos_x[muestra]
  muestra_y <- datos_y[muestra]
  lm(muestra_y ~ muestra_x) -> modelo
  coeficientes <- coefficients(modelo)
  beta_0_estimado[i] <- coeficientes[1]
  beta_1_estimado[i] <- coeficientes[2]
}

data.frame(
  limite = c("LI", "LS"),
  beta_0 = quantile(beta_0_estimado, c(0.025, 0.975)),
  beta_1 = quantile(beta_1_estimado, c(0.025, 0.975))
) -> intervalo_bootstrapping

intervalo_bootstrapping


##Gráfica

plot(beta_0, beta_1)

points(beta_0_estimado, beta_1_estimado)
points(coeficientes_muestrales[1], coeficientes_muestrales[2], pch = 20, col = 4, cex = 3)
points(beta_0, beta_1, pch = 20, col = 2, cex = 3)
rect(
  intervalo_bootstrapping$beta_0[1], 
  intervalo_bootstrapping$beta_1[1],
  intervalo_bootstrapping$beta_0[2],
  intervalo_bootstrapping$beta_1[2],
  border = 4, lwd = 2)


