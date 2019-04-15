library(ade4)
library(readxl)
library(tidyverse) # Paquete para manipulación y consulta.
library(lubridate)
library(highcharter)
library(ggplot2)
library(htmlwidgets)
library(xlsx)
library(FactoMineR)
library(MASS)


tipovar <- c("text", "text", "date", "date","text" , "text", "text", "text", "numeric", "numeric",
             "numeric", "text",	"text",
             "text", "text", "text") # Especificar tipo de variables del Dataset


tipovar2 <- c("text", "text", "date", "date","text" , "text", "text", "text", "numeric", "numeric",
             "numeric", "text",	"text",
             "text", "text", "text") # Especificar tipo de variables del Dataset



#Lectura de bases de datos

uso <- read_excel("Uso2.xlsx", 
                       sheet = 1,  col_types = tipovar)

d2 <- uso  %>% select( "vereda", "uso", "pecuario", "agricola", "año de construccion")



acm <- mca(d2)
acm$eig

detach("package:FactoMineR", unload=TRUE)
detach("package:MASS", unload=TRUE)


tipovar3 <- c("text", "text", "text" , "text", "text", "text",
              "text",	"text","text", "text", "text", "text",
              "numeric", "numeric", "text", "text")

index <- read_excel("Uso.xlsx", 
                    sheet = 1,  col_types = tipovar3)

names(index)
var <- index  %>% dplyr::select( "Área protegida", "cuenca", "Tipo comunidad", "grupoComunitario", "Permanencia")

var <- as.data.frame(var)

var$cuenca <- as.factor(var$cuenca)
var$`Área protegida` <- as.factor(var$`Área protegida`)
var$`Tipo comunidad` <- as.factor(var$`Tipo comunidad`)
var$grupoComunitario <- as.factor(var$grupoComunitario)
var$Permanencia <- as.factor(var$Permanencia)

acm <- mca(var)
newacm <- dudi.acm(var)
s.corcircle(newacm$co, 1, 3, clabel = 0.8)
boxplot(newacm, 2)

par(mfrow = c(2, 2))
for (i in 1:3){
  barplot(newacm$cr[, i], names.arg = row.names(newacm$cr), 
                       las = 2, main = paste("Axe", i))
}

par(mfrow = c(1, 1))

s.label(newacm$co, clabel = 0.7)
s.label(newacm$co, clabel = 0.7, boxes = F)
s.label(newacm$li, clabel = 0, pch = 17)

library(devtools)
install_github("larmarange/JLutils")
library(JLutils)
s.freq(newacm$li)
s.value(newacm$li, newacm$li[, 3], 1, 2, csi = 0.5)
s.arrow(newacm$co, clabel = 0.7)
s.hist(newacm$li, clabel = 0, pch = 15)
library(RColorBrewer)
s.class(newacm$li, var$Permanencia, col = brewer.pal(4, "Set1"))
s.chull(newacm$li, var$Permanencia, col = brewer.pal(4, "Set1"))
s.class(newacm$li, d$trav.imp, col = brewer.pal(4, "Set1"))

scatter(newacm, col = brewer.pal(4, "Set1"))
