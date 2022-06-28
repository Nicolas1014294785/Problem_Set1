rm(list=ls())

## llamar la librería pacman: contiene la función p_load()
install.packages("pacman")
install.packages("psych")
library("psych")
library(dplyr)
library(tidyr)
library(tidyverse)
library(mosaic)
require(pacman)
install.packages("stargazer")
require("stargazer")
install.packages('jmv')
library(jmv)
install.packages("scales")
library(mosaic)
require("mosaic")
install.packages("metrics")
require("metrics")
install.packages("boot")
install.packages("pacman")
install.packages("psych")
require("tidyverse")

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

estadisticas <- psych::describe(datos_geih_final[,c('age','totalHoursWorked','exper','anios_educ','ingtot')])
estadisticas2 <- as.data.frame(summary(datos_geih_final[,c('sex','maxEducLevel','estrato1','formal','informal')]))



ggplot(datos_geih_final) +
  geom_point(aes(x=ingtot,y=totalHoursWorked))

ggplot(datos_geih_final) +
  geom_point(aes(x=ingtot,y=exper))

ggplot(datos_geih_final) +
  geom_point(aes(x=ingtot,y=estrato1))

pairs(datos_geih_final [,c(44,49,65)], pch="*")
pairs(datos_geih_final [,c(44,58,65)], pch="*")





#### para pasar a tabla en formato texto
stargazer(estadisticas,type="text")
stargazer(estadisticas2,type="text")




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


################################ PUNTO 3 ##########################################
###################################################################################

#################Perfil de Edad e Ingresos#############
#Crear variable de edad al cuadrado
datos_geih_final$agesqr<- datos_geih_final$age**2

#Corremos la regresión con la ecuación del perfil de edad-ingresos:
reg1<-lm(ingtot~age+agesqr, data=datos_geih_final)
summary(reg1)

#Tabla de regresión
stargazer(reg1,type="text")

#Gráfico de predicción de Ingresos vs. Edad
ggplot() +
  geom_point(data=datos_geih_final,aes(x = age, y=ingtot),alpha=0.3,size=0.3)+
  geom_smooth(method = "lm",formula=ingtot~poly(age,2))+geom_line(aes(x=age,y=ingpred))

ggplot() +
  geom_point(data=datos_geih_final,aes(x = age, y=ingtot),alpha=0.3,size=0.3)+
  geom_smooth(method = "lm",formula=ingtot~poly(age,2))+geom_line(aes(x=age,y=ingpred))+
  scale_y_continuous(labels=dollar_format())


#Gráfico de predicción con ajustes de escala
ggplot() +
  geom_point(data=datos_geih_final,aes(x = age, y=ingtot),alpha=0.3,size=0.3)
geom_line(aes(x=age,y=ingpred))+
  scale_y_continuous(labels=dollar_format(),limits=c(0,15000000))

###############Bootstrap##############
require("tidyverse")
set.seed(112)
R<-1000 #Number of Repetitions 
eta_mod1<-rep(0,R)
for (i in 1:R) {
  db_sample<- sample_frac(c, size=1, replace = TRUE)
  f<-lm(ingtot~age+agesqr, db_sample)
  coefs<-f$coefficients
  eta_mod1[i]<-coefs[2]
}
plot(hist(eta_mod1))


quantile(eta_mod1,c(0.025,0.975))

require("boot")


eta.fn<-function(datos_geih_final,index){
  coef(lm(ingtot~age+agesqr, data=datos_geih_final, subset=index))
  
}

############################################ PUNTO 4 ##############################
###################################################################################

### los valores de cero de ingtot los cambio a 1 para poder calcular el log
filtro4 <- datos_geih_final$ingtot == 0
datos_geih_final$ingtot[filtro4] <- 1
table(datos_geih_final$ingtot)

####### Estimación
reg_4<-lm(log(ingtot)~sex+age+agesqr,data=datos_geih_final)
summary(reg_4)
stargazer(reg_4,type="text")

ggplot(datos_geih_final , mapping = aes(x = age , y = predict(reg_4))) +
  geom_point(col = "red" , size = 1) ###peak_age se da sobre los 42 años


######## base de datos solo con mujeres######
datos_geih_mujeres <- datos_geih_final %>%
  filter(sex==1)
reg_5<-lm(log(ingtot)~age+agesqr,data=datos_geih_mujeres)
summary(reg_5)
stargazer(reg_5,type="text")

ggplot(datos_geih_mujeres , mapping = aes(x = age , y = predict(reg_5))) +
  geom_point(col = "red" , size = 1) ####peak_age se da sobre los 46 años


######## Utilizando bootstrap
##Esta es la función y el bootstrap:
SE_5 <- function(datos_geih_mujeres, index){
  coef(lm(log(ingtot)~age+agesqr, data=datos_geih_mujeres, subset =  index))
}
boot(data=datos_geih_mujeres, SE_5, R=1000)

coefreg5<-reg_5$coefficients
b0<-coefreg5[1]
b1<-coefreg5[2]
b2<-coefreg5[3]

peak_age<- -(b1/(2*b2)) #####peak_age 46.063


######## base de datos solo con hombres######
datos_geih_hombres <- datos_geih_final %>%
  filter(sex==0)
reg_6<-lm(log(ingtot)~age+agesqr,data=datos_geih_hombres)
summary(reg_6)
stargazer(reg_6,type="text")

ggplot(datos_geih_hombres , mapping = aes(x = age , y = predict(reg_6))) +
  geom_point(col = "red" , size = 1) ####peak_age se da sobre los 37 años



######## Utilizando bootstrap
##Esta es la función y el bootstrap:
SE_6 <- function(datos_geih_hombres, index){
  coef(lm(log(ingtot)~age+agesqr, data=datos_geih_hombres, subset =  index))
}
boot(data=datos_geih_hombres, SE_6, R=1000)



coefreg6<-reg_6$coefficients
b0<-coefreg6[1]
b1<-coefreg6[2]
b2<-coefreg6[3]

peak_age<- -(b1/(2*b2)) #####peak_age 37.499


stargazer(reg_4,reg_5,reg_6,type="text")

### De acuerdo con lo anterior se puede observar que el peak_age es diferente
## para los hombres y para las mujeres, siendo mas alto para las mujeres



##########Introduciendo variables de control en el modelo#############
reg_4<-lm(log(ingtot)~sex+age+agesqr,data=datos_geih_final)
summary(reg_4)

reg_7<-lm(log(ingtot)~sex+age+agesqr+totalHoursWorked+anios_educ,data=datos_geih_final)
summary(reg_7)


stargazer(reg_4,reg_7,type="text")



### FWL #######
head(datos_geih_final)
tail(datos_geih_final)

datos_geih_final <- datos_geih_final %>% mutate(ej=c(rep(0,16395),1))
tail(datos_geih_final)

reg_8<-lm(log(ingtot)~sex+age+agesqr+totalHoursWorked+anios_educ+ej,
          data=datos_geih_final)
summary(reg_7)

stargazer(reg_4,reg_7,reg_8,type="text")


datos_geih_final<-datos_geih_final %>%
  mutate(res_y_e=lm(log(ingtot)~ej,datos_geih_final)$residuals, 
         res_x_e=lm(sex~ej,datos_geih_final)$residuals)

reg_9<-lm(res_y_e~res_x_e+age+agesqr+totalHoursWorked+anios_educ,datos_geih_final)
stargazer(reg_4,reg_7,reg_8,reg_9,type="text")


#datos_geih_final <- datos_geih_final %>%
  #select(-"ej")


#################################### PUNTO 5 ######################################
###################################################################################

set.seed(10101)
datos_geih_final5 <- datos_geih_final %>%
  mutate(holdout = as.logical(1:nrow(datos_geih_final) %in%
                                sample(nrow(datos_geih_final), nrow(datos_geih_final)*.3)))

### Dividimos la muestra entre tratamiento y prueba 
test<-datos_geih_final5[datos_geih_final5$holdout==T,]
train<-datos_geih_final5[datos_geih_final5$holdout==F,]

# Realizamos una regresión que sera nuestro modelo base solo con una constante
model1<-lm(ingtot~1,data=train)
summary(model1)
test$model1<-predict(model1,newdata=test)
with(test,mean((ingtot-model1)^2))# calculamos el MSE (error cuadrático medio) MSE=7.624e+12

# Estimammos nuvamente los modelos anteriores
model2<-lm(ingtot~age+agesqr,data=train)
summary(model2)
test$model2<-predict(model2,newdata=test)
with(test,mean((ingtot-model2)^2))# calculamos el MSE (error cuadrático medio) MSE=7.514e+12

model3<-lm(ingtot~age+agesqr+sex,data=train)
summary(model3)
test$model3<-predict(model3,newdata=test)
with(test,mean((ingtot-model3)^2))# calculamos el MSE (error cuadrático medio) MSE=7.485e+12

model4<-lm(ingtot~age+agesqr+sex+totalHoursWorked+exper+anios_educ,data=train)
summary(model4)
test$model4<-predict(model4,newdata=test)
with(test,mean((ingtot-model4)^2))# calculamos el MSE (error cuadrático medio) MSE=7.481e+12

model5<-lm(ingtot~age+agesqr+sex+totalHoursWorked+exper+poly(exper,2)+anios_educ,data=train)
summary(model5)
test$model5<-predict(model5,newdata=test)
with(test,mean((ingtot-model5)^2))# calculamos el MSE (error cuadrático medio) MSE=7.431e+12

######## leverage ########
##########################

## realizamos un loop para mirar leverage en todas las observaciones
aux <- c()
for (i in 1:nrow(test)) {
  u<-lm(ingtot~age+agesqr+sex+totalHoursWorked+exper+poly(exper,2)+anios_educ,data=test)$residual[i]
  u
  
  h<-lm.influence(lm(ingtot~age+agesqr+sex+totalHoursWorked+exper+poly(exper,2)+anios_educ,data=test))$hat[i]
  h
  
  alpha<-u/(1-h)
  alpha
  
  aux[i] <- alpha
}

mean(aux) ###-107.8124
max(aux) ###67501189
min(aux) ##-3150525
###### de acuerdo con lo anterior se puede decir que si existen valores atipicos en esta muestra,
###### es decir, existen observaciones con un alto apalancamiento que pueden estar impulsando los datos

###### Lo anterior puede ser porducto de un modelo defectuoso, sin embargo al analizar la complejidad del 
###### del modelo analizado, se puede decir que estas observaciones pueden ser valores atipicos que podria 
###### la DIAN, debido a que se esta reportando información anormal.


######## k-fold #########
#########################
install.packages("caret")
library("caret")


model2<-train(ingtot~age+agesqr,
              data=datos_geih_final,
              trControl=trainControl(method = "cv",number = 10),
              method="lm")# calculamos el RMSE (error cuadrático medio) MSE=26522990

model3<-train(ingtot~age+agesqr+sex,
              data=datos_geih_final,
              trControl=trainControl(method = "cv",number = 10),
              method="lm")# calculamos el RMSE (error cuadrático medio) MSE=26337652

model4<-train(ingtot~age+agesqr+sex+totalHoursWorked+exper+anios_educ,
              data=datos_geih_final,
              trControl=trainControl(method = "cv",number = 10),
              method="lm")# calculamos el RMSE (error cuadrático medio) MSE=2640825

model5<-train(ingtot~age+agesqr+sex+totalHoursWorked+exper+poly(exper,2)+anios_educ,
              data=datos_geih_final,
              trControl=trainControl(method = "cv",number = 10),
              method="lm")# calculamos el RMSE (error cuadrático medio) MSE=2634156





