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

str(datos_geih)

########Limpieza de Base############
###################################

# Eliminamos la primera columna que no contiene ninguna información,
## solo nos indica el número de la observación.

datos_geih <- datos_geih %>%
  select(-"Var.1")

cant_na <- colSums(is.na(datos_geih)) #Se guarda la cantidad de missing values por variables
class(cant_na) # se verifica la clase de "cant_na", lo queiro volver data frame para poderlo analizar

## cant_na se vuelve un data frame, enumeramos las variables y ponemos el titulo "variable"
cant_na <- data.frame(cant_na) %>%
  rownames_to_column("variable")

## Se organizan la variables, en orden descendencte (desde la que mas tiene valores missing)
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

ggplot(data = cant_na_1_50, aes(x = porcentaje_na, y = variable)) +
  geom_bar(stat = "identity", fill = "darkblue")

#Eliminamos las observaciones de las personas menores de 18 años 
datos_geih<-datos_geih %>%
  filter(wap==1 & age > 18)

##convertir 99 en años de educacuión a na

datos_geih$anios_educ<-as.numeric(levels(datos_geih$p6210s1))[datos_geih$p6210s1]
datos_geih<-datos_geih %>% mutate(anios_educ=na_if(anios_educ,99))

#Creamos variable de experiencia con base en la literatura (age minus anios educ minus 6)
datos_geih<-datos_geih %>% mutate(exper=age-anios_educ-6)
summary(datos_geih$exper)


view(datos_geih[datos_geih$exper==100,])#hay alguien con 106 años en la base

####Análisis descriptivo de variables#####
##########################################

psych::describe(datos_geih[,c('age','totalHoursWorked','exper','anios_educ')])

