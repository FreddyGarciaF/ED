

setwd("D:/Documentos/DCC/Dataset")

data <- read.csv("BV_2019_DAIS.csv")
data

summary(data)
dim(data)
names(data)
str(data)


plot(x=data$CrispatusCq,y=data$JenseniiCq)
plot(x=data$InersCq,y=data$GasseriCq)

hist(data$EDADENA.U.00D1.OS)

table(data$EDADENA.U.00D1.OS)

library(RColorBrewer)

paleta <- brewer.pal(3,'Set1')
colores <- paleta[as.numeric(data$EDADENA.U.00D1.OS)]

plot(x=data$CrispatusCq,
     y=data$JenseniiCq,
     col = colores,pch=9,
     main = 'Relación entre Crispatus y Jensenica',
     ylab = 'Jensenica',
     xlab = 'Crispatus')
legend(1,7.9,legend=c('Uno','Dos'),col=paleta,lty=1, pch=16)


data[12:15]
cor(data[,12:15]) 










