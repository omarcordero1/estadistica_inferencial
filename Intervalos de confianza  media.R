##Estimación por intervalos

##Intervalos de confianza  media

tamaño_muestral <- 35
iteraciones <- 100
media_poblacional_A <- 5
media_poblacional_B <- 3
desv_est_poblacional <- 2

plot(media_poblacional_A, media_poblacional_B)

for (i in seq_len(iteraciones)) {
  muestra_A <- rnorm(tamaño_muestral, media_poblacional_A, desv_est_poblacional)
  t.test_A <- t.test(muestra_A)
  intervalo_A <- t.test_A$conf.int
  LI_A <- min(intervalo_A)
  LS_A <- max(intervalo_A)
  
  muestra_B <- rnorm(tamaño_muestral, media_poblacional_B, desv_est_poblacional)
  t.test_B <- t.test(muestra_B)
  intervalo_B <- t.test_B$conf.int
  LI_B <- min(intervalo_B)
  LS_B <- max(intervalo_B)
  
  rect(LI_A, LI_B, LS_A, LS_B)
}

abline(0,1, col=2)
points(media_poblacional_A, media_poblacional_B, cex=4, col=2, pch=20)

