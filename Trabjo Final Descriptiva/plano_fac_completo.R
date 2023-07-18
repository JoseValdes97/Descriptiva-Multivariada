### sacar las coordenadas de los puntos tematica
x_1 <-acm[["co"]]$Comp1[1:7]
y_1 <- acm[["co"]]$Comp2[1:7]

plot(x_1,y_1,ylim=c(-4,1),xlim=c(-1.8,1.3),Trow=FALSE,gg=TRUE,cframe=1.1,
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

### sacar las coordenadas de los puntos mes
x_1 <-acm[["co"]]$Comp1[8:19]
y_1 <- acm[["co"]]$Comp2[8:19]

points(x_1,y_1,col = "blue",pch = 17)

## sacar los puntos de año

x_2 <- acm[["co"]]$Comp1[20:26]
y_2 <- acm[["co"]]$Comp2[20:26]

points(x_2,y_2,col = "green",pch = 17)

## sacar los puntos de region

x_4 <- acm[["co"]]$Comp1[27:33]
y_4 <- acm[["co"]]$Comp2[27:33]

points(x_4,y_4,col = "brown",pch = 17)

## sacar los puntos de Dia

x_2 <- acm[["co"]]$Comp1[34:40]
y_2 <- acm[["co"]]$Comp2[34:40]

points(x_2,y_2,col = "orange",pch = 17)

## sacar los puntos de hora

x_3 <- acm[["co"]]$Comp1[41:44]
y_3 <- acm[["co"]]$Comp2[41:44]

points(x_3,y_3,col = "gray46",pch = 17)

## sacar los puntos de arma empleada

x_4 <- acm[["co"]]$Comp1[45:50]
y_4 <- acm[["co"]]$Comp2[45:50]

points(x_4,y_4,col = "black",pch = 17)

## sacar los puntos de movil agresor

x_2 <- acm[["co"]]$Comp1[51:55]
y_2 <- acm[["co"]]$Comp2[51:55]

points(x_2,y_2,col = "darkorchid1",pch = 17)

## sacar los puntos de estado civil

x_3 <- acm[["co"]]$Comp1[56:60]
y_3 <- acm[["co"]]$Comp2[56:60]

points(x_3,y_3,col = "hotpink",pch = 17)

## sacar los puntos de pais nace

x_4 <- acm[["co"]]$Comp1[61:63]
y_4 <- acm[["co"]]$Comp2[61:63]

points(x_4,y_4,col = "maroon2",pch = 17)

## sacar los puntos de escolaridad

x_2 <- acm[["co"]]$Comp1[64:70]
y_2 <- acm[["co"]]$Comp2[64:70]

points(x_2,y_2,col = "cyan2",pch = 17)


legend(locator(1),c("tematica","mes","año","region","Dia","hora",
                    "arma empleada","movil agresor","estado civil","pais nace",
                    "escolaridad"),
       col=c("red","blue","green","brown","orange","gray","black",
             "darkorchid1","hotpink","gold2","cyan2"),
       lty=c(1,1),lwd=c(2,2),cex = 0.6)
