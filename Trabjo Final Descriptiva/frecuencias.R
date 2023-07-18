####### procederemos al analisis ########################


### variables activas
Y <- clean[,c(1,4:6,8,9,13,14,18,19,22)]
### arreglo nombre regiones
names(Y)[c(1,4,7:11)] <-c("TEM",  # tematica
                         "REG",   #region
                         "AR.EM",  # arma empleada
                         "M.AGR",  # movil agresor
                         "E.CIV",  # estado civil
                         "P.NAC",  # pais nacimiento
                         "ESC") # escolariad


###############################

######### el primero para notar las distribuciones de tematica 

#### 1
### areglaremos las etiquetas
#extraer
#nam_tematica <- Y$TEMATICA
#recodificar
#nam_tematica <- fct_recode(nam_tematica,"1"="ABIGEATO","2"="CABEZA DE GANADO",
#                    "3"="DELITOS SEXUALES","4"="EXTORSION",
#                     "5"="HOMICIDIOS","6"="HURTO A PERSONAS",
#                     "7"="HURTO A RESIDENCIAS","8"="LESIONES PERSONALES",
#                     "9"="PIRATERIA TERRESTRE","10"="SECUESTRO",
#                     "11"="VIOLENCIA INTRAFAMILIAR")
# cat_2 <- attributes (nam_tematica)$levels;
# per <- tabulate(nam_tematica)/nrow(Y)*100;
# pl <- plot(nam_tematica,horiz =TRUE, col=gray(seq(1.0,0.5,
#                                                   length = length(cat_2))),
#            ylim =c(0,13),xlim =c(0,180000),main=colnames(Y)[1]);
# text (1300,pl,round(per,1),cex =0.7, pos =4)
# legend(locator(1), c("1 = ABIGEATO","2 = CABEZA DE GANADO",
#                      "3 = DELITOS SEXUALES","4 = EXTORSION",
#                      "5 = HOMICIDIOS","6 = HURTO A PERSONAS",
#                      "7 = HURTO A RESIDENCIAS","8 = LESIONES PERSONALES",
#                      "9 = PIRATERIA TERRESTRE","10 = SECUESTRO",
#                      "11 = VIOLENCIA INTRAFAMILIAR"),  
#       col=gray(seq(1.0,0.5,length = length(cat_2))),lty=c(1,1),lwd=c(2,2),
#       cex = 0.6)


######### el primero para notar las distribuciones de departamento

#### 2
### areglaremos las etiquetas
#extraer
# nam_depart <- Y$DEPARTAMENTO
# #recodificar
# nam_depart <- fct_recode(nam_depart,"1"="AMAZONAS","2"="ANTIOQUIA",
#                            "3"="ARAUCA","4"="ATLANTICO",
#                            "5"="BOLIVAR","6"="BOYACA",
#                            "7"="CORDOBA","8"="CALDAS",
#                            "9"="CAQUETA","10"="CASANARE",
#                            "11"="CAUCA","12"="CESAR","13"="CHOCO",
#                            "14"="CUNDINAMARCA","15"="GUAINIA",
#                            "16"="GUAJIRA","17"="GUAVIARE",
#                            "18"="HUILA","19"="MAGDALENA",
#                            "20"="META","21"="NARINO","22"="NORTE DE SANTANDER",
#                            "23"="PUTUMAYO","24"="QUINDIO",
#                            "25"="RISARALDA","26"="SAN ANDRES",
#                            "27"="SANTANDER","28"="SUCRE",
#                            "29"="TOLIMA","30"="VALLE",
#                            "31"="VAUPES","32"="VICHADA")
# cat_3 <- attributes (nam_depart)$levels;
# per <- tabulate(nam_depart)/nrow(Y)*100;
# pl <- plot(nam_depart,horiz =TRUE, col=gray(seq(1.0,0.5,
#                                                   length = length(cat_3))),
#            ylim =c(0,38),xlim =c(0,110000),main=colnames(Y)[4]);
# text (12500,pl,round(per,1),cex =0.7, pos =4)
# legend(locator(1), c("1=AMAZONAS","2=ANTIOQUIA","3=ARAUCA",
#                      "4=ATLANTICO","5=BOLIVAR","6=BOYACA","7=CORDOBA",
#                      "8=CALDAS","9=CAQUETA","10=CASANARE","11=CAUCA",
#                      "12=CESAR","13=CHOCO","14=CUNDINAMARCA",
#                      "15=GUAINIA","16=GUAJIRA","17=GUAVIARE",
#                      "18=HUILA","19=MAGDALENA","20=META","21=NARINO",
#                      "22=NORTE DE SANTANDER","23=PUTUMAYO","24=QUINDIO",
#                      "25=RISARALDA","26=SAN ANDRES","27=SANTANDER",
#                      "28=SUCRE","29=TOLIMA","30=VALLE","31=VAUPES",
#                      "32=VICHADA"),  
#        col=gray(seq(1.0,0.5,length = length(cat_3))),lty=c(1,1),lwd=c(2,2),
#        cex = 0.47)

##### plot con frecuencia de las variables activas en 2,3,5,6
#mai=c(0.9,izquierda,0.6,0.6)
par(las=1,mfrow =c(2,2),mai=c(0.9,0.9,0.6,0.6))
for(i in c(2,3,5,6)){
      # nombre de las barras
      cat <- attributes (Y[,i])$levels;
      # porcentaje dentro de las barras
      per <- tabulate(Y[,i])/nrow(Y)*100;
      # barplot
      # ylim siempre dejar 2 extra
     pl <- plot(Y[,i],horiz =TRUE, col=gray(seq(1.0,0.9,length = length(cat))),
                 ylim =c(0,15),xlim =c(0,120000),xlab=colnames(Y)[i]);
     # texto dentro de las barras
     # 1300 posición dentro de las barras
     # cex tamaño de los numeros
      text (1300,pl,round(per,1),cex =0.7, pos =4)
}
###### los nuevos histos de referencia ###############3

######   1

##### plot con frecuencia de las variables activas en 7,9,11,1
### arma escolaridad  estadocivil escolaridad tematica

#mai=c(0.9,izquierda,0.6,0.6)
par(las=1,mfrow =c(2,2),mai=c(1.2,2,0.6,0.6))
for(i in c(7,9,11,1)){
      # nombre de las barras
      cat <- attributes (Y[,i])$levels;
      # porcentaje dentro de las barras
      per <- tabulate(Y[,i])/nrow(Y)*100;
      # barplot
      # ylim siempre dejar 2 extra
      pl <- plot(Y[,i],horiz =TRUE, col=gray(seq(1.0,0.7,length = length(cat))),
                 ylim =c(0,11),xlim =c(0,200000),xlab=colnames(Y)[i]);
      # texto dentro de las barras
      # 1300 posición dentro de las barras
      # cex tamaño de los numeros
      text (23500,pl,round(per,1),cex =0.7, pos =4,col = "midnightblue")
}


############ 2

##### plot con frecuencia de las variables activas en 2,3,5,6
######## mes año  dia hora
#mai=c(0.9,izquierda,0.6,0.6)

par(las=1,mfrow =c(2,2),mai=c(0.9,0.9,0.6,0.6))
for(i in c(2,3,5,6)){
      # nombre de las barras
      cat <- attributes (Y[,i])$levels;
      # porcentaje dentro de las barras
      per <- tabulate(Y[,i])/nrow(Y)*100;
      # barplot
      # ylim siempre dejar 2 extra
      pl <- plot(Y[,i],horiz =TRUE, col=gray(seq(1.0,0.8,length = length(cat))),
                 ylim =c(0,15),xlim =c(0,120000),xlab=colnames(Y)[i]);
      # texto dentro de las barras
      # 1300 posición dentro de las barras
      # cex tamaño de los numeros
      text (1300,pl,round(per,1),cex =0.7, pos =4,col = "midnightblue")
}

##########  3

##### plot con frecuencia de las variables activas en 8,4,10
####### movil agresosr region pasi nace

#mai=c(0.9,izquierda,0.6,0.6)
par(las=1,mfrow =c(2,2),mai=c(1.2,1.3,0.6,0.6))
for(i in c(8,4,10)){
      # nombre de las barras
      cat <- attributes (Y[,i])$levels;
      # porcentaje dentro de las barras
      per <- tabulate(Y[,i])/nrow(Y)*100;
      # barplot
      # ylim siempre dejar 2 extra
      pl <- plot(Y[,i],horiz =TRUE, col=gray(seq(1.0,0.8,length = length(cat))),
                 ylim =c(0,11),xlim =c(0,350000),xlab=colnames(Y)[i]);
      # texto dentro de las barras
      # 1300 posición dentro de las barras
      # cex tamaño de los numeros
      text (22000,pl,round(per,1),cex =0.7, pos =4,col = "midnightblue")
}

##################################33
##########################################
##########################################


###histograma con las ilustrativas 


##### plot con frecuencia de las variables ilustrativas  en clean
### como algunas no han sido tocadas las modificaremos y dejaremos en clean

##### zona 11
clean$ZONA <- factor(clean$ZONA)
###### movil victimas 15   edad 16
##### sexo 17
clean$SEXO <- factor(clean$SEXO)

####### movil victima sexo edad zona

#mai=c(0.9,izquierda,0.6,0.6)
par(las=1,mfrow =c(2,2),mai=c(1.2,1.3,0.6,0.6))
for(i in c(11,15,16,17)){
      # nombre de las barras
      cat <- attributes (clean[,i])$levels;
      # porcentaje dentro de las barras
      per <- tabulate(clean[,i])/nrow(clean)*100;
      # barplot
      # ylim siempre dejar 2 extra
      pl <- plot(clean[,i],horiz =TRUE, col=gray(seq(1.0,0.8,length = length(cat))),
                 ylim =c(0,11),xlim =c(0,350000),xlab=colnames(clean)[i]);
      # texto dentro de las barras
      # 1300 posición dentro de las barras
      # cex tamaño de los numeros
      text (30000,pl,round(per,1),cex =0.7, pos =4,col = "midnightblue")
}

