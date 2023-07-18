library(FactoClass)
library(haven)
Uhogar <- read_dta("C:/Users/pepal/OneDrive/Documentos/Dani/U/Estad韘tica Multivariada/Uhogar.dta")
base1<-Uhogar[,c("tipo_vivienda", "material_paredes", "material_pisos", "sp_energia", "sp_gasnatural", "sp_acueducto", "sp_alcantarillado", "tenencia_vivienda","n_internet","sp_telefono")]
base2<-Uhogar[,c("valor_arriendo_pagado", "n_neveras", "n_hornos", "n_licuadoras", "n_lavadoras", "n_microondas", "n_televisores", "n_radios", "n_equipos_sonido", "n_computadores", "n_celular", "motocicletas", "automoviles", "transporte", "n_bicicletas", "lotes", "casas", "semovientes", "ing_trabajo", "ing_pensiones", "ing_arriendos", "ing_intereses_div", "ing_ayudas", "vr_gtos_mensuales", "vr_gtos_mens_alim", "sp_estrato", "t_personas")]
base2$valor_arriendo_pagado[is.na(base2$valor_arriendo_pagado)]<-0
#Funci贸n para descategorizar una variable
descl<-function(x,y){
  u<-c()
  for(i in 1:length(x)){
    u <- c(u,y[as.integer(x[i])])
  }
  return(as.factor(u))
}
#Asignaci贸n de nombres de las clases en la base1
ntvv<-c("Casa","Apartamento","Cuarto","Otro_tipo","Casa_Indigena")
base1$tipo_vivienda<-descl(base1$tipo_vivienda,ntvv)
nmp<-c("Bloque","Adobe","Bahareque","Prefabricado","Madera","Guadua","Precario","No_paredes")
base1$material_paredes<-descl(base1$material_paredes,nmp)
nmpi<-c("M谩rmol","Baldosa","Cemento","Madera_mala","Tierra","Otro")
base1$material_pisos<-descl(base1$material_pisos,nmpi)
nSN<-c("Si","No")
base1$sp_energia<-descl(base1$sp_energia,nSN)
base1$sp_gasnatural<-descl(base1$sp_gasnatural,nSN)
base1$sp_acueducto<-descl(base1$sp_acueducto,nSN)
base1$sp_alcantarillado<-descl(base1$sp_alcantarillado,nSN)
base1$n_internet<-descl(base1$n_internet,nSN)
base1$sp_telefono<-descl(base1$sp_telefono,nSN)
nViv<-c("Propia_pagada","Propia_pagando","Arriendo","Usufructo","De_Hecho")
base1$tenencia_vivienda<-descl(base1$tenencia_vivienda,nViv)

#ACM
Y<-as.data.frame(base1)
Z<-acm.disjonctif(Y)#TDC
B<-acm.burt(Y,Y)#Tabla de Burt
D<-diag(diag(as.matrix(B)))#Diagonal de tablas de contingencia
n<-nrow(Y);s<-ncol(Y);g<-colSums(Z)/(n*s)
acm<-dudi.acm(Y, scannf=FALSE,nf=3)
#N煤mero de ejes a retener usando el criterio de Benzecri
s=ncol(base1)
lambdaz<-acm$eig[which(acm$eig>1/s)]
tlz<-(s/(s-1))^2*(lambdaz-(1/s))^2
barplot(tlz,main="Histograma del criterio de Benz猫cri")
#Primer plano factorial con plot_ly
acm$li<-acm$li[,1:2]
acm$co<-acm$co[,1:2]
acm$li<-cbind(acm$li,"Individuo")
acm$co<-cbind(acm$co,"Variable")
colnames(acm$li)<-c("Eje1","Eje2","Tipo")
colnames(acm$co)<-c("Eje1","Eje2","Tipo")
ppf<-rbind(acm$li,acm$co)

library(plotly)
plot_ly(data=ppf,x=~Eje1,y=~Eje2,type="scatter",mode="markers",
        symbol = ~Tipo, symbols = c('circle','x'), color = ~Tipo, colors=c('black','red'),
        text = ~paste("Id: ",rownames(ppf)))
#Agrupamiento
facto<-FactoClass(Y,dudi.acm,nf=3,nfcl=6,k.clust=8,scanFC=FALSE)
library(colorspace)
acm$li<-acm$li[,1:2]
colnames(acm$li)<-c("Eje1","Eje2")
acmclases<-cbind(acm$li,Grupo=facto$cluster)
head(acmclases)
#Representaci贸n de los grupos en el primer plano factorial
plot_ly(data=acmclases,x=~Eje1,y=~Eje2,type="scatter",mode="markers",
        color = ~Grupo, colors= rainbow(8))
#Caracterizaci贸n de los grupos
facto$carac.cate



#ACP
Y<-as.data.frame(base2)
acp<-dudi.pca(Y,scannf = FALSE,nf=4)
acp$li<-acp$li[,1:2]
acp$co<-acp$co[,1:2]
acp$li<-cbind(acp$li,"Individuo")
acp$co<-cbind(acp$co,"Variable")
colnames(acp$li)<-c("Eje1","Eje2","Tipo")
colnames(acp$co)<-c("Eje1","Eje2","Tipo")
ppf<-rbind(acp$li,acp$co)

plot_ly(data=ppf,x=~Eje1,y=~Eje2,type="scatter",mode="markers",
        symbol = ~Tipo, symbols = c('circle','x'), color = ~Tipo, colors=c('black','red'),
        text = ~paste("Id: ",rownames(ppf)))
facto1<-FactoClass(Y,dudi.pca,nf=4,nfcl=10,k.clust=6,scanFC=FALSE)
acp$li<-acp$li[,1:2]
colnames(acp$li)<-c("Eje1","Eje2")
acpclases<-cbind(acp$li,Grupo=facto1$cluster)
head(acpclases,n=20)
#Representaci贸n de las clases en el primer plano factorial
plot_ly(data=acpclases,x=~Eje1,y=~Eje2,type="scatter",mode="markers",
        color = ~Grupo, colors= rainbow(8))
#Caracterizaci贸n de los grupos
facto1$carac.cont