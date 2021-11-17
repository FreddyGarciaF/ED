#credito https://rpubs.com/Joaquin_AR/465473
crear_poblacion <- function(n_poblacion, n_variables, limite_inf = NULL,
                            limite_sup = NULL) 
{
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  for (i in 1:n_poblacion) {
    individuo <- rep(NA, times = n_variables)
    for (j in 1:n_variables) {
      individuo[j] <- runif(n = 1, min = limite_inf[j], max = limite_sup[j])
    }
    poblacion[i, ] <- individuo
  }
}



poblacion <- crear_poblacion(
  n_poblacion = 10,
  n_variables = 2,
  limite_inf  = c(-5, -5),
  limite_sup  = c(+5, +5),
)

poblacion



z <- 1
x <- 1:10

sample( x[ x != z ] , 3 , replace= FALSE)
#vdiferencia <- c()
#for (i in vdif) {
# vdiferencia <- rbind(vdiferencia,c(pob_ini[i,]))
#}


#(vu[i] <- vdiferencia[3,i] + FS * ( vdiferencia[1,i] - vdiferencia[2,i]) )#Calcular la mutación















