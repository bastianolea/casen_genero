library(tidyverse)
load("~/Casen/Casen 2017.Rdata")

glimpse(casen)

# r9h. Discriminado por su orientación sexual o identidad
de géner

# r9m. Discriminado por participar o no
# en sindicatos u organizaciones gremiales


casen %>%
  group_by(sexo, r9s) %>%
  summarise(cantidad = n(),
            expc = mean(expc)) %>%
  mutate(cantidad_w = cantidad*expc) %>%
  group_by(sexo) %>%
  mutate(porcentaje_w = cantidad_w/sum(cantidad_w))

#Función que calcula la variable elegida y le aplica expansión
casen2 <- function(x) {

  casen %>%
    rename(variable = as.character(x)) %>%
    group_by(sexo, variable) %>%
    summarise(expc = mean(expc),
              cantidad = n()) %>%
    mutate(cantidad_w = cantidad*expc) %>%
    group_by(sexo) %>%
    mutate(porcentaje = cantidad/sum(cantidad),
           porcentaje_w = cantidad_w/sum(cantidad_w))
}

#Hacer función que filtre algo o que saque el porcentaje de una sola categoría (por ejemplo el 50% del 1%)

#Hacer función que saque medias


# r9q. Discriminado por su condición de salud o discapacidad
casen2("r9q")
 
# r9p. Discriminado por pertenecer a un pueblo indígena
casen2("r9p")


# r9s. No ha sido tratado
# injustamente o discrimi
casen2("r9s")

# r9f. Discriminado por ser extranjero/a
casen2("r9f") 

# r9b. Discriminado por ser mujer/ser hombre
casen2("r9b")
# 
# r9a. Discriminado por nivel socioeconómico
casen2("r9a")
# 
# pobreza
casen2("pobreza")

# indmat
casen2("indmat") 


# pco1:1 jefe de hogar
casen2("pco1") %>%
  filter(variable == "Jefe(a) de hogar")


# hacinamiento
casen2("hacinamiento")

# iae allegamiento externo
#Se refiere a la presencia en una vivienda de más de un hogar, evidenciándose con la figura de más de una vivienda, físicamente, dentro del predio. 
casen2("iae")

# iai allegamiento interno
#Es la situación donde se constata la presencia de más de un núcleo familiar dentro de la vivienda.
casen2("iai")
# 
# activ condición de actividad
casen2("activ")

# esc escolaridad
casen2("esc")

# qaut quintil autonomo nacional
casen2("qaut")
# qautr quintil autonomo regional
casen2("qautr")
# 
# 
# ytrabajocor ingreso del trabajo
# 
# ytrabajocorh ingreso del trabajo del hogar
# 
# y13a pension de alimentos
# 
# y26_2bm1 Monto pensión de vejez (monto 1)
# 
# y26_2bm2 Monto aporte previsional solidario (monto 2)
# 
# y26_2c Mes pasado monto jubilación o pensión de vejez
# y26_3c_in same institución
