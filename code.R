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
library(devtools)
install_github("larmarange/JLutils")
library(JLutils)
library(RColorBrewer)
library("factoextra")
library(ggplot2)



detach("package:devtools", unload=TRUE)
detach("package:MASS", unload=TRUE)


tipovar <- c("text", "text", "text" , "text", "text", "text",
              "text",	"text","text", "text", "text", "text", "text",
              "numeric", "numeric", "text", "text")

index <- read_excel("Uso.xlsx", 
                    sheet = 1,  col_types = tipovar)

names(index)
var <- index  %>% dplyr::select( "Área protegida", "cuenca", "Adquisición", "Comunidad", "grupoComunitario", "Permanencia")

var <- as.data.frame(var)

var$cuenca <- as.factor(var$cuenca)
var$`Área protegida` <- as.factor(var$`Área protegida`)
var$Comunidad <- as.factor(var$Comunidad)
var$grupoComunitario <- as.factor(var$grupoComunitario)
var$Permanencia <- as.factor(var$Permanencia)
#var$'Año' <- as.factor(var$'Año')
var$Adquisición <- as.factor(var$Adquisición)

# Ánalisis de correspondencias múltiples
newacm <- dudi.acm(var)

# Círculos de correalción
s.corcircle(newacm$co, 1, 2, clabel = 0.6)
s.corcircle(newacm$co, 1, 3, clabel = 0.6)
s.corcircle(newacm$co, 2, 3, clabel = 0.6)

#Frecuencias modalidades
boxplot(newacm, 2)

par(mfrow = c(2, 2))
for (i in 1:4){
  barplot(newacm$cr[, i], names.arg = row.names(newacm$cr), 
                       las = 2, main = paste("Axe", i))
}

par(mfrow = c(1, 1))

s.label(newacm$co,1,2, clabel = 0.7)
s.label(newacm$co,1,2, clabel = 0.6, boxes = F)
s.label(newacm$li,1,2, clabel = 0, pch = 17)

par(mfrow = c(1, 2))
s.freq(newacm$li, 1, 2)
s.label(newacm$co,1,2, clabel = 0.6, boxes = F)

s.value(newacm$li, newacm$li[, 3], 1, 2, csi = 0.5)
s.arrow(newacm$co, clabel = 0.7)
s.hist(newacm$li, 1, 2, clabel = 0, pch = 15)

par(mfrow = c(1, 2))
s.class(newacm$li, var$Permanencia, col = brewer.pal(4, "Set1"))
s.arrow(newacm$co, clabel = 0.7)

s.arrow(newacm$co, clabel = 0.7)
s.class(newacm$li, var$`Área protegida`, col = brewer.pal(4, "Set1"))

par(mfrow = c(1, 2))

s.class(newacm$li, var$grupoComunitario, col = brewer.pal(4, "Set1"))
s.arrow(newacm$co, clabel = 0.7)


s.chull(newacm$li, var$Permanencia, col = brewer.pal(4, "Set1"))
s.class(newacm$li, d$trav.imp, col = brewer.pal(4, "Set1"))

scatter(newacm, col = brewer.pal(4, "Set1"))


index <- index %>% filter(Permanencia != "Pendiente")
index$Permanencia <- as.factor(index$Permanencia)
table(index$Permanencia)
index$Año <- as.numeric(index$Año)



ggplot(index, aes(x = Año, y = Comunidad, color = Permanencia)) + geom_point()
ggplot(index, aes(x = Año, y = vereda, color = Permanencia)) + geom_point()
ggplot(index, aes(x = Año, y = index$`Área protegida`, color = Permanencia)) + geom_point()

ggplot(index, aes(index$Años, y =  index$vereda, color = Permanencia)) + geom_point()
ggplot(index, aes(index$Años, y = index$`Área protegida` , color = Permanencia)) + geom_point()




tipovar <- c("text", "text", "date", "date","text" , "text", "text", "text", "numeric", "numeric",
             "numeric", "text",	"text",
             "text", "text", "text") # Especificar tipo de variables del Dataset


tipovar2 <- c("text", "text", "date", "date","text" , "text", "text", "text", "numeric", "numeric",
              "numeric", "text",	"text",
              "text", "text", "text") # Especificar tipo de variables del Dataset


uso2 <- read_excel("Uso.xlsx", 
                    sheet = 8,  col_types = tipovar2)

uso2 <- as.data.frame(uso2)
uso2$`Área protegida` <- as.factor(uso2$`Área protegida`)


ggplot(uso2, aes(x = uso['Área Ha'] , y = uso$`Área protegida` , color = uso)) + geom_point()


tipovar2 <- c("text", "text", "text" , "text", "text", "text",
              "text",	"text","text", "text", "text", "text", "text",
              "numeric", "numeric", "text", "text" ,"numeric", "text", "numeric", "text", "text", "text", "text")    # Especificar tipo de variables del Dataset


uso3 <- read_excel("Uso.xlsx", 
                   sheet = 2,  col_types = tipovar2)

uso3 <- as.data.frame(uso3)



var <- uso3  %>% dplyr::select( "Área protegida", "cuenca", "Comunidad", "uso")

var <- as.data.frame(var)

var$cuenca <- as.factor(var$cuenca)
var$`Área protegida` <- as.factor(var$`Área protegida`)
var$Comunidad <- as.factor(var$Comunidad)
#var$grupoComunitario <- as.factor(var$grupoComunitario)
#var$Permanencia <- as.factor(var$Permanencia)
#var$'Año' <- as.factor(var$'Año')
#var$Adquisición <- as.factor(var$Adquisición)
var$uso <- as.factor(var$uso)

newacm <- dudi.acm(var)

par(mfrow = c(1, 1))
# Círculos de correalción
s.corcircle(newacm$co, 1, 2, clabel = 0.6)
s.corcircle(newacm$co, 1, 3, clabel = 0.6)
s.corcircle(newacm$co, 2, 3, clabel = 0.6)

#Frecuencias modalidades
boxplot(newacm, 2)

par(mfrow = c(2, 2))
for (i in 1:4){
  barplot(newacm$cr[, i], names.arg = row.names(newacm$cr), 
          las = 2, main = paste("Axe", i))
}

par(mfrow = c(1, 1))

s.label(newacm$co,1,2, clabel = 0.7)
s.label(newacm$co,1,2, clabel = 0.6, boxes = F)
s.label(newacm$li,1,2, clabel = 0, pch = 17)

par(mfrow = c(1, 2))
s.freq(newacm$li, 1, 2)
s.label(newacm$co,1,2, clabel = 0.6, boxes = F)

s.value(newacm$li, newacm$li[, 3], 1, 2, csi = 0.5)
s.arrow(newacm$co, clabel = 0.7)
s.hist(newacm$li, 1, 2, clabel = 0, pch = 15)

par(mfrow = c(1, 2))
s.class(newacm$li, var$Permanencia, col = brewer.pal(4, "Set1"))
s.arrow(newacm$co, clabel = 0.7)

s.arrow(newacm$co, clabel = 0.7)
s.class(newacm$li, var$`Área protegida`, col = brewer.pal(4, "Set1"))

par(mfrow = c(1, 2))

s.class(newacm$li, var$uso, col = brewer.pal(4, "Set1"))
s.arrow(newacm$co, clabel = 0.7)


s.chull(newacm$li, var$Permanencia, col = brewer.pal(4, "Set1"))
s.class(newacm$li, d$trav.imp, col = brewer.pal(4, "Set1"))

scatter(newacm, col = brewer.pal(4, "Set1"))




var <- uso3  %>% dplyr::select( "Área protegida", "Comunidad", "uso")

var <- as.data.frame(var)

#var$cuenca <- as.factor(var$cuenca)
var$`Área protegida` <- as.factor(var$`Área protegida`)
var$Comunidad <- as.factor(var$Comunidad)
#var$grupoComunitario <- as.factor(var$grupoComunitario)
#var$Permanencia <- as.factor(var$Permanencia)
#var$'Año' <- as.factor(var$'Año')
#var$Adquisición <- as.factor(var$Adquisición)
var$uso <- as.factor(var$uso)

newacm <- dudi.acm(var)

par(mfrow = c(1, 1))
# Círculos de correalción
s.corcircle(newacm$co, 1, 2, clabel = 0.6)
s.corcircle(newacm$co, 1, 3, clabel = 0.6)
s.corcircle(newacm$co, 2, 3, clabel = 0.6)

#Frecuencias modalidades
boxplot(newacm, 2)

par(mfrow = c(2, 2))
for (i in 1:4){
  barplot(newacm$cr[, i], names.arg = row.names(newacm$cr), 
          las = 2, main = paste("Axe", i))
}

par(mfrow = c(1, 1))

s.label(newacm$co,1,2, clabel = 0.7)
s.label(newacm$co,1,2, clabel = 0.6, boxes = F)
s.label(newacm$li,1,2, clabel = 0, pch = 17)

par(mfrow = c(1, 2))
s.freq(newacm$li, 1, 2)
s.label(newacm$co,1,2, clabel = 0.6, boxes = F)

s.value(newacm$li, newacm$li[, 3], 1, 2, csi = 0.5)
s.arrow(newacm$co, clabel = 0.7)
s.hist(newacm$li, 1, 2, clabel = 0, pch = 15)

par(mfrow = c(1, 2))
s.class(newacm$li, var$Permanencia, col = brewer.pal(4, "Set1"))
s.arrow(newacm$co, clabel = 0.7)

s.arrow(newacm$co,1,2, clabel = 0.7)
s.class(newacm$li, var$`Área protegida`, col = brewer.pal(4, "Set1"))

par(mfrow = c(1, 2))

s.class(newacm$li, var$uso, col = brewer.pal(4, "Set1"))
s.arrow(newacm$co, clabel = 0.7)


scatter(newacm, col = brewer.pal(4, "Set1"))





