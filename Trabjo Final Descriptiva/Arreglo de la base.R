#### pulimos y limpiamos la base #########

## limpiamos los valores faltantes
clean <- na.omit(delitos)
## tener acceso facila las variables
attach(clean) ### por si la cago 
## poner mejor el nombre de tematica
names(clean)[1] <- "TEMATICA"

##########################################
#### revisando variables #########
#########################################
## libreria for cats

#### tematica
clean$TEMATICA <- factor(clean$TEMATICA)
attributes(clean$TEMATICA)
## renombrando el error
clean$TEMATICA <- fct_recode(clean$TEMATICA,
                                    "EXTORSION"="EXTORCIÃN")

###### arreglando algunas categorias

clean$TEMATICA <- fct_recode(clean$TEMATICA,
                             "OTRO"="ABIGEATO", "OTRO"="SECUESTRO",
                             "OTRO"="PIRATERIA TERRESTRE",
                             "OTRO"="CABEZA DE GANADO", 
                             "OTRO"="EXTORSION")

##########################################

#### departamento
clean$DEPARTAMENTO <- factor(clean$DEPARTAMENTO)
attributes(clean$DEPARTAMENTO)
## renombrando el error
clean$DEPARTAMENTO <- fct_recode(clean$DEPARTAMENTO,
                           "ATLANTICO"="ATLÃ\u0081NTICO",
                           "BOLIVAR"="BOLÃ\u008dVAR", "BOYACA"="BOYACÃ\u0081",
                           "CORDOBA"="CÃRDOBA", "CAQUETA"="CAQUETÃ\u0081",
                           "CHOCO"="CHOCÃ","GUAINIA"="GUAINÃ\u008dA",
                           "NARINO"="NARIÃ'O", "QUINDIO"="QUINDÃ\u008dO",
                           "SAN ANDRES"="SAN ANDRÃ???S", "VAUPES"="VAUPÃ???S")

###### arreglando por regiones

clean$DEPARTAMENTO <- fct_recode(clean$DEPARTAMENTO,
                                 "ANDINA" = "ANTIOQUIA", "ANDINA" = "BOYACA",
                                 "ANDINA" = "HUILA", "ANDINA" = "QUINDIO",
                                 "ANDINA" = "NORTE DE SANTANDER",
                                 "ANDINA" = "RISARALDA", "ANDINA" = "SANTANDER",
                                 "ANDINA" = "TOLIMA",
                                 "ANDINA" = "CALDAS",
                                 "CARIBE" = "ATLANTICO", "CARIBE" = "BOLIVAR",
                                 "CARIBE" = "CORDOBA", "CARIBE" = "GUAJIRA",
                                 "CARIBE" = "SAN ANDRES",
                                 "CARIBE" = "CESAR", "CARIBE" = "SUCRE",
                                 "CARIBE" = "MAGDALENA",
                                 "PACIFICO" = "CHOCO", "PACIFICO" = "CAUCA",
                                 "PACIFICO" = "NARINO",
                                 "ORINOQUIA" = "META", "ORINOQUIA" = "VICHADA",
                                 "ORINOQUIA" = "CASANARE", 
                                 "ORINOQUIA" = "ARAUCA",
                                 "AMAZONIA" = "GUAINIA", "AMAZONIA" = "PUTUMAYO",
                                 "AMAZONIA" = "VAUPES", 
                                 "AMAZONIA" = "GUAVIARE","AMAZONIA" = "CAQUETA",
                                 "AMAZONIA" = "AMAZONAS")
                                 
                                 
##########################################

#### mes
clean$MES <- factor(clean$MES)
attributes(clean$MES)


##########################################

#### año
clean$AÑO <- factor(clean$AÑO)
attributes(clean$AÑO)

##########################################

#### DIA
clean$DIA <- factor(clean$DIA)
attributes(clean$DIA)
## renombrando el error
clean$DIA <- fct_recode(clean$DIA,
                             "Miercoles"="MiÃ©rcoles", "Sabado"="SÃ¡bado")

##########################################

#### HORA
## poner la hora como se trabajara
# libreria lubridate
clean$HORA <- hour(clean$HORA)
# recodificaremos hora para su manejo
summary(clean$HORA)
clean$HORA <- recode(clean$HORA, ' 0:5 = "Madrugada"; 
                           6:12 = "Mañana"; 13:18 = "Tarde"; 19:23 = "Noche"')
clean$HORA <- factor(clean$HORA)
attributes(clean$HORA)

##########################################

#### Arma empleada
clean$`ARMA EMPLEADA` <- factor(clean$`ARMA EMPLEADA`)
attributes(clean$`ARMA EMPLEADA`)
# como son demasidas las recodificaremos y miraremos para ello su frecuencia
table(clean$`ARMA EMPLEADA`)
# primero unimos los no reportados
clean$`ARMA EMPLEADA` <- fct_recode(clean$`ARMA EMPLEADA`,
                              "NO REPORTA" = "NO REPORTADO", 
                              "NO REPORTA" = "NO REPORTADA")
# ahora intentaremos unir las armas blancas
clean$`ARMA EMPLEADA` <- fct_recode(clean$`ARMA EMPLEADA`,
                              "ARMA BLANCA" = "ARMA BLANCA / CORTOPUNZANTE", 
                              "ARMA BLANCA" = "CORTANTES",
                              "ARMA BLANCA" = "CUCHILLA",
                              "ARMA BLANCA" = "PUNZANTES",
                              "ARMA BLANCA" = "CORTOPUNZANTES")
# ahora intentaremos unir los explosivos
clean$`ARMA EMPLEADA` <- fct_recode(clean$`ARMA EMPLEADA`,
                              "ARTEFACTO EXPLOSIVO" = "ARTEFACTO INCENDIARIO", 
                              "ARTEFACTO EXPLOSIVO" = "CARRO BOMBA",
                              "ARTEFACTO EXPLOSIVO" = "CILINDRO BOMBA",
                              "ARTEFACTO EXPLOSIVO" = "GASOLINA",
                              "ARTEFACTO EXPLOSIVO" = "MOTO BOMBA",
                              "ARTEFACTO EXPLOSIVO" = "OLLA BOMBA",
                              "ARTEFACTO EXPLOSIVO" = "PAQUETE BOMBA",
                              "ARTEFACTO EXPLOSIVO" = "PERSONA BOMBA",
                              "ARTEFACTO EXPLOSIVO" = "ARTEFACTO EXPLOSIVO/CARGA DINAMITA",
                              "ARTEFACTO EXPLOSIVO" = "CARTA EXTORSIVA",
                              "ARTEFACTO EXPLOSIVO" = "GRANADA DE MANO",
                              "ARTEFACTO EXPLOSIVO" = "MINA ANTIPERSONA",
                              "ARTEFACTO EXPLOSIVO" = "PAPA EXPLOSIVA",
                              "ARTEFACTO EXPLOSIVO" = "POLVORA(FUEGOS PIROTECNICOS)")

# ahora intentaremos unir las sustancia toxicas
clean$`ARMA EMPLEADA` <- fct_recode(clean$`ARMA EMPLEADA`,
                              "SUSTANCIAS TOXICAS" = "ACIDO", 
                              "SUSTANCIAS TOXICAS" = "GASES",
                              "SUSTANCIAS TOXICAS" = "VENENO",
                              "SUSTANCIAS TOXICAS" = "ALUCINOGENOS",
                              "SUSTANCIAS TOXICAS" = "ESCOPOLAMINA",
                              "SUSTANCIAS TOXICAS" = "QUIMICOS",
                              "SUSTANCIAS TOXICAS" = "ALIMENTOS VENCIDOS",
                              "SUSTANCIAS TOXICAS" = "LICOR ADULTERADO",
                              "SUSTANCIAS TOXICAS" = "MEDICAMENTOS",
                              "SUSTANCIAS TOXICAS" = "JERINGA")

# unimos los robos en automotor
clean$`ARMA EMPLEADA` <- fct_recode(clean$`ARMA EMPLEADA`,
                                          "VEHICULO" = "MOTO")

# categoria de otros
clean$`ARMA EMPLEADA` <- fct_recode(clean$`ARMA EMPLEADA`,
                              "OTROS" = "AGUA CALIENTE",
                              "OTROS" = "ALMOHADA", 
                              "OTROS" = "BOLSA PLASTICA",
                              "OTROS" = "CINTAS",
                              "OTROS" = "CUERDA/SOGA/CADENA",
                              "OTROS" = "ESPOSAS",
                              "OTROS" = "LLAVE MAESTRA",
                              "OTROS" = "PERRO",
                              "OTROS" = "MIXTA",
                              "OTROS" = "DIRECTA",
                              "OTROS" = "VEHICULO",
                              "OTROS" = "REDES SOCIALES",
                              # lo nuevos otros
                              "OTROS" = "SUSTANCIAS TOXICAS",
                              "OTROS" = "ARTEFACTO EXPLOSIVO",
                              "OTROS" = "LLAMADA TELEFONICA")


##########################################

#### movil agresor
clean$`MOVIL AGRESOR` <- factor(clean$`MOVIL AGRESOR`)
attributes(clean$`MOVIL AGRESOR`)
# primero unimos los no reportados
clean$`MOVIL AGRESOR` <- fct_recode(clean$`MOVIL AGRESOR`,
                                          "NO REPORTA" = "NO REPORTADO", 
                                          "NO REPORTA" = "NO REPORTADA")

# ahora intentaremos unir los conductores
clean$`MOVIL AGRESOR` <- fct_recode(clean$`MOVIL AGRESOR`,
                                    "CONDUCTOR" = "CONDUCTOR BUS",
                                    "CONDUCTOR" = "CONDUCTOR TAXI", 
                                    "CONDUCTOR" = "CONDUCTOR MOTOCICLETA")

# ahora intentaremos unir los pasajeros
clean$`MOVIL AGRESOR` <- fct_recode(clean$`MOVIL AGRESOR`,
                                    "PASAJERO" = "PASAJERO BUS",
                                    "PASAJERO" = "PASAJERO MOTOCICLETA", 
                                    "PASAJERO" = "PASAJERO METRO",
                                    "PASAJERO" = "PASAJERO TAXI")

# ahora intentaremos unir los vehiculo
clean$`MOVIL AGRESOR` <- fct_recode(clean$`MOVIL AGRESOR`,
                                    "VEHICULO" = "BICICLETA")

##########################################

#### movil victima
clean$`MOVIL VICTIMA` <- factor(clean$`MOVIL VICTIMA`)
attributes(clean$`MOVIL VICTIMA`)
# primero unimos los no reportados
clean$`MOVIL VICTIMA` <- fct_recode(clean$`MOVIL VICTIMA`,
                                          "NO REPORTA" = "NO REPORTADO", 
                                          "NO REPORTA" = "NO REPORTADA")

# ahora intentaremos unir los conductores
clean$`MOVIL VICTIMA` <- fct_recode(clean$`MOVIL VICTIMA`,
                                    "CONDUCTOR" = "CONDUCTOR BUS",
                                    "CONDUCTOR" = "CONDUCTOR TAXI", 
                                    "CONDUCTOR" = "CONDUCTOR MOTOCICLETA")

# ahora intentaremos unir los pasajeros
clean$`MOVIL VICTIMA`<- fct_recode(clean$`MOVIL VICTIMA`,
                                    "PASAJERO" = "PASAJERO BUS",
                                    "PASAJERO" = "PASAJERO MOTOCICLETA", 
                                    "PASAJERO" = "PASAJERO METRO",
                                    "PASAJERO" = "PASAJERO TAXI")

# ahora intentaremos unir los vehiculo
clean$`MOVIL VICTIMA` <- fct_recode(clean$`MOVIL VICTIMA`,
                                    "VEHICULO" = "BICICLETA")

##########################################

#### edad
## poner la hora como se trabajara
summary(clean$EDAD)
clean$EDAD <- recode(clean$EDAD, ' 0:17 = "Menor"; 
                           18:28 = "Joven"; 28:60 = "Adulto"; 60:99 = "Mayor"')
clean$EDAD <- factor(clean$EDAD)
attributes(clean$EDAD)

##########################################

#### estado civil
clean$`ESTADO CIVIL` <- factor(clean$`ESTADO CIVIL`)
attributes(clean$`ESTADO CIVIL`)
# primero unimos los no reportados
clean$`ESTADO CIVIL` <- fct_recode(clean$`ESTADO CIVIL`,
                                    #"NO REPORTA" = "NO REPORTADO", 
                                    #"NO REPORTA" = "NO REPORTADA",
                                   # lo nuevos otros
                                   "OTROS" = "VIUDO",
                                   "OTROS" = "SEPARADO",
                                   "OTROS" = "DIVORCIADO")


##########################################

#### pais nace
clean$`PAIS NACE` <- factor(clean$`PAIS NACE`)
attributes(clean$`PAIS NACE`)
# como son demasidas las recodificaremos y miraremos para ello su frecuencia
table(clean$`PAIS NACE`)
# primero unimos los no reportados
clean$`PAIS NACE` <- fct_recode(clean$`PAIS NACE`,
                                   "NO REPORTA" = "NO REPORTADO", 
                                   "NO REPORTA" = "NO REPORTADA")
# vamos recodificar
clean$`PAIS NACE` <- fct_lump(clean$`PAIS NACE`, n=2)

##########################################

#### escolaridad
clean$ESCOLARIDAD <- factor(clean$ESCOLARIDAD)
attributes(clean$ESCOLARIDAD)
# primero unimos los no reportados
clean$ESCOLARIDAD <- fct_recode(clean$ESCOLARIDAD,
                                "NO REPORTA" = "NO REPORTADO", 
                                "NO REPORTA" = "NO REPORTADA")


##### estos datos solo sirven para estar en un excel y manejarlos en otro
### programa si se quiere no sirven pa traerlos de nuevo a R no se por qué
write.csv2(clean, file = "Datos.csv",row.names = FALSE)

################################
#################################
####################################






