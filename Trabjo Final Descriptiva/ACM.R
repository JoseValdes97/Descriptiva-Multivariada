##### ACM
library(FactoClass)
acm <-dudi.acm(Y,scannf = FALSE,nf =3)
# criterio de Benzecri
s <-11; 1/s
# --> se calcula tau para los primeros 12 ejes
eig17 <-acm$eig[1:17]
tau <-(s/(s-1))^2*(eig17-(1/s))^2
ptau <-tau/sum(tau)*100
## data frame con los valores propios
eigtab<-data.frame(valp=acm$eig, porc=acm$eig/sum(acm$eig)*100,
                        pacu = cumsum(acm$eig)/sum(acm$eig)* 100)

########################################

Ys <- clean[,c(11,15:17)]
## cambiar nombre en el plano
names(Ys)[2] <- "M.VIC"

################################
##mai=c (0.55 ,0.5 ,0.1 ,0.1)
par(las =1, mfrow=c(1 ,2))

#### barplot de los valores propios
barplot(acm$eig,las =3, horiz = T, xlim = c(0,0.25),
        main = "Histograma de valores propios")
#abline(v = 0.091,col = 'black', lwd = 2)
# barplot con los valores del criterio de benzecri
barplot(ptau, horiz = T, xlim = c(0,35),
        main = "Histograma del criterio de Benzécri")

#####################

### tabla de alores porpios no de ayudas
xtable(cbind(eje=1:8, eigtab[1:8,], eje=9:16, eigtab[9:16,], eje=17:24, 
             eigtab[17:24,]), digits=c(0,rep(c(0,3,1,1),3)))

#####################
#######################################
###############################

### plano genral colores ############# 
  
######### sorry se me perdio el codigo

#######################
###############################################
###############################


#### primer plano 1 #############

### sacar las coordenadas de los arma empleada
x_1 <-acm[["co"]]$Comp1[45:50]
y_1 <- acm[["co"]]$Comp2[45:50]

plot(x_1,y_1,ylim=c(-2.5,1),xlim=c(-1,1.5),Trow=FALSE,gg=TRUE,cframe=1.1,
     col="red",cex.global=0.8,xlab = "Factor 1: 0.2441(4.6%)",
     ylab = "Factor 2: 0.2258(4.2%)",
     pch = 17, #tipo de punto
)
## malla
grid(nx = NULL, ny = NULL,
     lty = 2,      # Tipo de ínea
     col = "gray", # Color
     lwd = 0.5)
abline(v=0,col = "black",lty=2,lwd=0.7) # eje 1
abline(h=0,col = "black",lty=2,lwd=0.7) #eje 2
## etiquetas
seleccionados <- c(1:6)
text(x_1[seleccionados],y_1[seleccionados],
     labels = row.names(acm[["co"]])[45:50],
     cex = 0.45, pos = 2, col = "black")
############

## sacar los puntos de movil agresor

x_2 <- acm[["co"]]$Comp1[51:55]
y_2 <- acm[["co"]]$Comp2[51:55]

points(x_2,y_2,col = "blue",pch = 17)

## etiquetas
seleccionados_2 <- c(1:5)
text(x_2[seleccionados_2],y_2[seleccionados_2],
     labels = row.names(acm[["co"]])[51:55],
     cex = 0.45, pos = 3, col = "black")

############

## sacar los puntos de hora

x_3 <- acm[["co"]]$Comp1[41:44]
y_3 <- acm[["co"]]$Comp2[41:44]

points(x_3,y_3,col = "green",pch = 17)

## etiquetas
seleccionados_2 <- c(1:4)
text(x_3[seleccionados_2],y_3[seleccionados_2],
     labels = row.names(acm[["co"]])[41:44],
     cex = 0.45, pos = 4, col = "black")

## sacar los puntos de dia

x_3 <- acm[["co"]]$Comp1[34:40]
y_3 <- acm[["co"]]$Comp2[34:40]

points(x_3,y_3,col = "brown",pch = 17)

## etiquetas
seleccionados_3 <- c(1:7)
text(x_3[seleccionados_3],y_3[seleccionados_3],
     labels = c(1:7),
     cex = 0.45, pos = 1, col = "black")
# cuadro de leyenda
legend(locator(1), c("1 = Domingo","2 = Jueves",
                     "3 = Lunes","4 = Martes",
                     "5 = Miercoles","6 = Sábados",
                     "7 = Viernes"),  
       col="brown",lty=c(1,1),lwd=c(2,2), cex = 0.6)


################ primer plano 2 ###########

### sacar las coordenadas de los puntos tematica
x_1 <-acm[["co"]]$Comp1[1:7]
y_1 <- acm[["co"]]$Comp2[1:7]

plot(x_1,y_1,ylim=c(-4,1),xlim=c(-1.5,1.5),Trow=FALSE,gg=TRUE,cframe=1.1,
     col="red",cex.global=0.8,xlab = "Factor 1: 0.2441(4.6%)",
     ylab = "Factor 2: 0.2258(4.2%)",
     pch = 17, #tipo de punto
)
## malla
grid(nx = NULL, ny = NULL,
     lty = 2,      # Tipo de ínea
     col = "gray", # Color
     lwd = 0.5)
abline(v=0,col = "black",lty=2,lwd=0.7) # eje 1
abline(h=0,col = "black",lty=2,lwd=0.7) #eje 2
## etiquetas
seleccionados <- c(1:7)
text(x_1[seleccionados],y_1[seleccionados],
     labels = row.names(acm[["co"]])[seleccionados],
     cex = 0.45, pos = 1, col = "black")
############

## sacar los puntos de año

x_2 <- acm[["co"]]$Comp1[20:26]
y_2 <- acm[["co"]]$Comp2[20:26]

points(x_2,y_2,col = "blue",pch = 17)

## etiquetas
seleccionados_2 <- c(1:7)
text(x_2[seleccionados_2],y_2[seleccionados_2],
     labels = row.names(acm[["co"]])[20:26],
     cex = 0.45, pos = 4, col = "black")

############

## sacar los puntos de mes

x_3 <- acm[["co"]]$Comp1[8:19]
y_3 <- acm[["co"]]$Comp2[8:19]

points(x_3,y_3,col = "green",pch = 17)

############

## sacar los puntos de pais nace

x_4 <- acm[["co"]]$Comp1[61:63]
y_4 <- acm[["co"]]$Comp2[61:63]

points(x_4,y_4,col = "brown",pch = 17)
## etiquetas
seleccionados_4 <- c(1:3)
text(x_4[seleccionados_4],y_4[seleccionados_4],
     labels = row.names(acm[["co"]])[61:63],
     cex = 0.5, pos = 3, col = "black")

##################################################
###########################################

######### ilustrativas

sc<-supqual(acm,Ys)
#plotfp(as.data.frame(sc$coor),col="black",gg=TRUE)

### sacar las coordenadas de los zona
x_1 <-sc[["coor"]][1:2,1]
y_1 <- sc[["coor"]][1:2,2]

plot(x_1,y_1,ylim=c(-1.8,0.6),xlim=c(-1,1),Trow=FALSE,gg=TRUE,cframe=1.1,
     col="red",cex.global=0.8,xlab = "Factor 1: 0.2441(4.6%)",
     ylab = "Factor 2: 0.2258(4.2%)",
     pch = 17, #tipo de punto
)
## malla
grid(nx = NULL, ny = NULL,
     lty = 2,      # Tipo de ínea
     col = "gray", # Color
     lwd = 0.5)
abline(v=0,col = "black",lty=2,lwd=0.7) # eje 1
abline(h=0,col = "black",lty=2,lwd=0.7) #eje 2
## etiquetas
seleccionados <- c(1:2)
text(x_1[seleccionados],y_1[seleccionados],
     labels = row.names(sc[["coor"]])[1:2],
     cex = 0.45, pos = 1, col = "black")

#############################

## sacar los puntos de movil victima

x_2 <- sc[["coor"]][3:7,1]
y_2 <- sc[["coor"]][3:7,2]

points(x_2,y_2,col = "blue",pch = 17)

## etiquetas
seleccionados_2 <- c(1:5)
text(x_2[seleccionados_2],y_2[seleccionados_2],
     labels = row.names(sc[["coor"]])[3:7],
     cex = 0.45, pos = 3, col = "black")
######################
## sacar los puntos de edad

x_3 <- sc[["coor"]][8:11,1]
y_3 <- sc[["coor"]][8:11,2]

points(x_3,y_3,col = "green",pch = 17)

## etiquetas
seleccionados_3 <- c(1:4)
text(x_3[seleccionados_3],y_3[seleccionados_3],
     labels = row.names(sc[["coor"]])[8:11],
     cex = 0.45, pos = 3, col = "black")
###########################

#### sacar los puntos de sexo

x_4 <- sc[["coor"]][12:14,1]
y_4 <- sc[["coor"]][12:14,2]

points(x_4,y_4,col = "brown",pch = 17)
## etiquetas
seleccionados_4 <- c(1:3)
text(x_4[seleccionados_4],y_4[seleccionados_4],
     labels = row.names(sc[["coor"]])[12:14],
     cex = 0.45, pos = 1, col = "black")

#########################################33
plot(acm,Trow=FALSE,gg=TRUE,
     cframe=1.1,col.col="black",cex.global=0.8, cex = 0.5)
#################################

#############
#######################
#####################

#### ayudas ala interpretacion
ayu<-inertia.dudi(acm,,T) 

###############3

########### tabla de inercia
xtable(cbind(ayu[["tot.inertia"]][1:17,],tau), digits=c(0,rep(c(5,3,2,3),1)))
###############
############## coordenadas y ayudas ACM
xtable(cbind(peso=acm$cw*100,acm$co,ayu$col.abs/100,abs(ayu$col.rel)/100))
########### ayudas ilustra
xtable(data.frame(por=sc$ncat/34.76,dis2=sc$dis2,coor=sc$coor,
                  vt=sc$tv,cos2=sc$cos2),digits=c(0,1,rep(3,10)))
