
library(scales)

#==============================================Función 
#Genera la poblacion inicial 
poblacion <- function(LB,UB,NP){
  
  x=matrix(0,NP,length(LB))
  
  for (i in 1:length(LB)){  
    x[,i] = runif(NP,LB[i],UB[i])
  }
  return(x)
}

#Función Objetivo
fobj <- function(x){
  fxy = 0
  for (i in 1:length(x)) {
    fxy <- fxy + x[i]^2
  }
  return(fxy)
} 

#Funcion Objetivo del vector
eval_fobj <- function(x){
  t <- c()
  for (i in 1:nrow(x) ) 
  {
    t <- rbind(t,c( x[i,] , fobj(x[i,]) ))
  }
  return(t)
}

#Busca la fobj best 
best_obj <- function(x){
  bestObj <- fobj(x[1,])
  for (i in 2:nrow(x)) {
    if( fobj(x[i,]) < bestObj  )
      bestObj <- fobj(x[i,])
  }
  return(bestObj)
}


#==============================================Parámetros iniciales

NP <- 5 #Poblacion en vectores
dimension <- 2
LB <- rep(-5,dimension) #Valor Min
UB <- rep(5,dimension) #valor Max
FS <- 0.7 #factor de escala
CR <- 0.0001 #Cruza


(pob_ini <- poblacion(LB,UB,NP))#Inicializa la población
copy_pob <- pob_ini
BestObj <- c()

#BestObj <- rbind(BestObj, pob_ini(copy_pob))

for ( G in 1:30 ){#N veces
  
  for (z in 1:NP ) {# Inicio de las iteraciones
    
    (vpadre <- pob_ini[z,]) 
    
    x <- 1:NP # Para candidatos
    vdif <- sample( x[ x != z ] , 3 , replace= FALSE)
    (rand01 <- runif(length(LB),0,1)) #Valores aleatorios para mutacion rand(0,1)   
    (jrand <- sample(1:length(LB), size=1, replace= FALSE ))  # jrand
    
    vu <- c() #El vector U
    for( i in  1:dimension ) {
      if (  rand01[i] < CR | i == jrand  ){
        (vu[i] <- pob_ini[vdif[3],i] + FS * ( pob_ini[vdif[1],i] - pob_ini[vdif[2],i]) )#Calcular la mutación
      }else{
        vu[i] <- vpadre[i]
      }
    }
    #Mecanismo de reemplazo
    ifelse( fobj(vu) < fobj(vpadre),  copy_pob[z,] <- vu , copy_pob[z,]  <- vpadre )
  }
  BestObj <- rbind(BestObj, best_obj(copy_pob))
  pob_ini <- copy_pob
}

BestObj
mean(BestObj)
median(BestObj)
sd(BestObj)
plot(BestObj,type = "line")
summary(BestObj)