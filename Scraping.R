rm(list=ls())

## llamar la librería pacman: contiene la función p_load()
install.packages("pacman")
install.packages("psych")
library("psych")
require(pacman)

## p_load llama/instala-llama las librerías que se enlistan:
p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest)# web-scraping

# robots.txt for https://ignaciomsarmiento.github.io/GEIH2018_sample/
## Acceder al robots.txt de Ignacio Sarmiento 
browseURL("https://ignaciomsarmiento.github.io/GEIH2018_sample/robots.txt")
##Se puede observar que la pagina no presenta ninguna restricción

my_url = "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
browseURL(my_url) ## Ir a la página

## Obtener las tablas de la página

datos_geih <- data.frame()
for (i in 1:10) {
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i ,".html")
  temporal <- read_html(url) %>%
    html_table () # guarda cada temporal como una tabla en formato lista
  
  temporal <- as.data.frame(temporal) # Convierte cada temporal en data.frame (hoja de datos)
  
  datos_geih <- rbind(datos_geih, temporal) # Guarda los temporales de cada iteración formado la base de datos completa
}

str(datos_geih)

#Corregir las clases de las variables
unique(datos_geih["p6240"])
table(datos_geih["p6240"])

# Utilizando el diccionario, identificamos variables categóricas
variables_categoricas <- c(
  "cclasnr11", "cclasnr2", "cclasnr3", "cclasnr4", "cclasnr5",
  "cclasnr6", "cclasnr7", "cclasnr8", "clase", "college",
  "cotPension", "cuentaPropia", "depto", "directorio", "dominio",
  "dsi", "estrato1", "formal", "ina", "inac", "informal",
  "maxEducLevel", "p6050", "microEmpresa", "ocu", "oficio", 
  "orden", "p6090", "p6100", "p6210", "p6210s1", "p6240", "p6510",
  "p6510s2", "p6545", "p6545s2", "p6580", "p6580s2", "p6585s1",
  "p6585s1a2", "p6585s2", "p6585s2a2", "p6585s4", "p6585s4a2",
  "p6590", "p6610", "p6620", "p6630s1", "p6630s2", "p6630s3",
  "p6630s4", "p6630s6", "p6920", "p7040", "p7050", "p7090",
  "p7110", "p7120", "p7140s1", "p7140s2", "p7150", "p7160",
  "p7310", "p7350", "p7422", "p7472", "p7495", "p7500s1",
  "p7500s2", "p7500s3", "p7505", "p7510s1", "p7510s2",
  "p7510s3", "p7510s5", "p7510s6", "p7510s7", "pea", "pet", 
  "regSalud", "relab", "secuencia_p", "sex", "sizeFirm", "wap"
)

# Volvemos las variables categoricas a tipo factor
datos_geih <- datos_geih %>%
  mutate_at(.vars = variables_categoricas,
            .funs = factor)


#Nos quedamos con las personas que son mayores de 18 y estan ocupadas
datos_geih_ocu <- datos_geih %>%
  filter(ocu==1 & age > 18)
summary(datos_geih_ocu)

str(datos_geih_ocu)



########Limpieza de Base############
###################################

# Eliminamos la primera columna que no contiene ninguna información,
## solo nos indica el número de la observación.

datos_geih_ocu <- datos_geih_ocu %>%
  select(-"Var.1")

cant_na <- colSums(is.na(datos_geih_ocu)) #Se guarda la cantidad de missing values por variables
class(cant_na) # se verifica la clase de "cant_na", lo queiro volver data frame para poderlo analizar

## cant_na se vuelve un data frame, enumeramos las variables y ponemos el titulo "variable"
cant_na <- data.frame(cant_na) %>%
  rownames_to_column("variable")

## Se organizan la variables, en orden descendente (desde la que mas tiene valores missing)
cant_na <- cant_na %>%
  arrange(desc(cant_na))

## Creo una columna que se llame porcentaje_na que me indica qel porcentaje de missing que hay en esa variable
cant_na$porcentaje_na <- cant_na$cant_na/nrow(datos_geih)

######## Graficamos el procentaje de missing
p_load("ggplot2")

# Eliminamos del data frame cant_na las variables que no tienen missing
filtro <- cant_na$cant_na != 0
cant_na <- cant_na[filtro,]

#Por visualización graficamos las primeras 50 variables
cant_na_1_50 <- cant_na[1:50,]
cant_na_1_50$variable <- factor(cant_na_1_50$variable,
                                levels = cant_na_1_50$variable) ## para mostrar el diagrama de barras de menor a mayor

# Graficamos
ggplot(data = cant_na_1_50, aes(x = porcentaje_na, y = variable)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Porcentaje de NAs", y = "Variable",
       title = "Porcentaje de NAs para las primeras 50 variables")

#Grafica de la variable 51 a la 100
cant_na_51_100 <- cant_na[51:100,]
cant_na_51_100$variable <- factor(cant_na_51_100$variable,
                                levels = cant_na_51_100$variable) ## para mostrar el diagrama de barras de menor a mayor

# Graficamos
ggplot(data = cant_na_51_100, aes(x = porcentaje_na, y = variable)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Porcentaje de NAs", y = "Variable",
       title = "Porcentaje de NAs para las variables 51 a 100")

#Grafica de la variable 101 a la 119
cant_na_101_119 <- cant_na[101:119,]
cant_na_101_119$variable <- factor(cant_na_101_119$variable,
                                  levels = cant_na_101_119$variable) ## para mostrar el diagrama de barras de menor a mayor

# Graficamos
ggplot(data = cant_na_101_119, aes(x = porcentaje_na, y = variable)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Porcentaje de NAs", y = "Variable",
       title = "Porcentaje de NAs para las variables 101 a 119")




###### Eliminaremos las variables que tienen un porcentaje de missing mayor al 5%
filtro2 <- cant_na$porcentaje_na > 0.05
borrar_variables <- cant_na$variable[filtro2]

###### Guardamos una nueva base de datos solo con las variables que tienen un porcentaje de missing
###### menor al 5%
datos_geih_final <- datos_geih_ocu %>%
  select(-borrar_variables)

## Analizando las variables con menos del 5% de valores missing 
tail(cant_na)
datos_geih_final$maxEducLevel
table(datos_geih_final["maxEducLevel"])

## teniendo en cuenta que maxEducLevel es una variable categorica, imputaremos por la moda
moda_maxEducLevel <- which(table(datos_geih_final$maxEducLevel) ==
                      max(table(datos_geih_final$maxEducLevel)))

## Ponemos el valor de la moda "7" en los NA
filtro3 <- is.na(datos_geih_final$maxEducLevel)
datos_geih_final$maxEducLevel[filtro3] <- moda_maxEducLevel
table(datos_geih_final$maxEducLevel)

### se elimina p6210 y se cambia por maxEducLevel
datos_geih_final <- datos_geih_final %>%
  select(-"p6210") 





## Analizando las variables con menos del 5% de valores missing 
tail(cant_na)
datos_geih_final$isa
table(datos_geih_final["isa"])

## teniendo en cuenta que isa es una variable numerica, imputaremos por la media
mean_isa <- mean(datos_geih_final$isa, na.rm = T)

## Ponemos el valor de la media 19294.83
filtro3 <- is.na(datos_geih_final$isa)
datos_geih_final$isa[filtro3] <- mean_isa
table(datos_geih_final$isa)



## Analizando las variables con menos del 5% de valores missing 
tail(cant_na)
datos_geih_final$p7070
table(datos_geih_final["p7070"])

## teniendo en cuenta que p7070 es una variable numerica, imputaremos por la media
mean_p7070 <- mean(datos_geih_final$p7070, na.rm = T)

## Ponemos el valor de la media 19294.83
filtro3 <- is.na(datos_geih_final$p7070)
datos_geih_final$p7070[filtro3] <- mean_p7070
table(datos_geih_final$p7070)



## Analizando las variables con menos del 5% de valores missing 
tail(cant_na)
datos_geih_final$impa
table(datos_geih_final["impa"])

## teniendo en cuenta que impa es una variable numerica, imputaremos por la media
mean_impa <- mean(datos_geih_final$impa, na.rm = T)

## Ponemos el valor de la media 1457754
filtro3 <- is.na(datos_geih_final$impa)
datos_geih_final$impa[filtro3] <- mean_impa
table(datos_geih_final$impa)





## Analizando las variables con menos del 5% de valores missing 
tail(cant_na)
datos_geih_final$p6100
table(datos_geih_final["p6100"])

## teniendo en cuenta que p6100 es una variable categorica, imputaremos por la moda
moda_p6100 <- which(table(datos_geih_final$p6100) ==
                             max(table(datos_geih_final$p6100)))

## Ponemos el valor de la moda "1"
filtro3 <- is.na(datos_geih_final$p6100)
datos_geih_final$p6100[filtro3] <- moda_p6100
table(datos_geih_final$p6100)




## Analizando las variables con menos del 5% de valores missing 
tail(cant_na)
datos_geih_final$regSalud
table(datos_geih_final["regSalud"])

## teniendo en cuenta que regSalud es una variable categorica, imputaremos por la moda
moda_regSalud <- which(table(datos_geih_final$regSalud) ==
                      max(table(datos_geih_final$regSalud)))

## Ponemos el valor de la moda "1"
filtro3 <- is.na(datos_geih_final$regSalud)
datos_geih_final$regSalud[filtro3] <- moda_regSalud
table(datos_geih_final$regSalud)


datos_geih_final %>%
  is.na() %>%
  sum()


##convertir 99 en años de educacuión a na
datos_geih_final$anios_educ<-as.numeric(levels(datos_geih_final$p6210s1))[datos_geih_final$p6210s1]
datos_geih_final <- datos_geih_final %>%
  mutate(anios_educ = na_if(anios_educ,99))

##imputamos el valor de NA para anios_educ
datos_geih_final$anios_educ
table(datos_geih_final["anios_educ"])

## teniendo en cuenta que anios_educ es una variable categorica, imputaremos por la moda
moda_anios_educ <- which(table(datos_geih_final$anios_educ) ==
                         max(table(datos_geih_final$anios_educ)))

## Ponemos el valor de la moda "11"
filtro3 <- is.na(datos_geih_final$anios_educ)
datos_geih_final$anios_educ[filtro3] <- moda_anios_educ
table(datos_geih_final$anios_educ)


####Creamos variable de experiencia con base en la literatura (age minus anios educ minus 6)
datos_geih_final <- datos_geih_final %>% 
  mutate(exper=age-anios_educ-6)

table(datos_geih_final["exper"])




####Análisis descriptivo de variables#####
##########################################

estadisticas <- as.data.frame(psych::describe(datos_geih_final[,c('age','totalHoursWorked','exper','anios_educ','ingtot')]))
estadisticas2 <- as.data.frame(summary(datos_geih_final[,c('sex','maxEducLevel','estrato1','formal','informal')]))



######Skew########
ggplot(datos_geih_final, aes(x = log(ingtot))) + 
  geom_histogram(fill = "darkblue", alpha = 0.3)

p_load(e1071)
skewness(datos_geih_final$ingtot)
skewness(sqrt(datos_geih_final$ingtot))
skewness(log(datos_geih_final$ingtot))
skewness(1/(datos_geih_final$ingtot))

p_load(EnvStats)
lambda <- boxcox(datos_geih_final$ingtot, objective.name = "log-likelihood",
       optimize = T)$lambda






################## NORMALIZACIÓN DE DATOS #########################
## Verificamos cuales variables son numericas
filtro4 <- sapply(datos_geih_final, is.numeric)
variables_numericas <- names(datos_geih_final)[filtro4]

## normalizando por z-core, media cero y desviación estandar 1
datos_geih_escalada <- scale(datos_geih_final[,variables_numericas])
summary(datos_geih_escalada) ##todas las medias estan en cero

apply(datos_geih_escalada, MARGIN = 2, FUN = function(x) sd(x, na.rm = T)) ## Desvest de todas las variables es igual a 1 

sigma <- attributes(datos_geih_escalada)$`scaled:scale` ## Desviación estandar
media <- attributes(datos_geih_escalada)$`scaled:center` ##media

datos_geih_escalada[,"mes"]*sigma["mes"]+media["mes"] ## Verificando y devolviendome a la base original por medio de los atributos
datos_geih[,"mes"]




install.packages("gtsummary")


