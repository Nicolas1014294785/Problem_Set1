rm(list=ls())

## llamar la librería pacman: contiene la función p_load()
install.packages("pacman")
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

pagina = read_html(my_url) ## leer el html de la página
class(pagina) ## ver la clase del objeto

View(pagina)

## Obtener las tablas de la página

datos_geih <- data.frame()
for (i in 1:10) {
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i ,".html")
  temporal <- read_html(url) %>%
    html_table ()
  datos_geih <- rbind(datos_geih, temporal)
}