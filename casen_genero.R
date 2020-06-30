library(tidyverse)

load("~/Casen/Casen 2017.Rdata")

#Crear nuevas variables de pobreza
casen <- casen %>%
  mutate(pobreza_simple = case_when(pobreza == "No pobres" ~ "No pobres",
                                 is.na(pobreza) ~ NA_character_,
                                 TRUE ~ "Pobres")) %>%
  mutate(pobreza_extrema = case_when(pobreza == "Pobres extremos" ~ "Pobreza extrema",
                                    is.na(pobreza) ~ NA_character_,
                                    TRUE ~ "Fuera de pobreza extrema")) %>%
  mutate(pobreza_no_extrema = case_when(pobreza == "Pobres no extremos" ~ "Pobreza no extrema",
                                     is.na(pobreza) ~ NA_character_,
                                     TRUE ~ "Fuera de pobreza extrema")) %>%
  mutate(nacionalidad = case_when(r1a == "Otra nacionalidad. Especifique país" ~ "Extranjero",
                                  TRUE ~ "Chileno")) %>%
  mutate(nacionalidad_pobreza = fct_cross(pobreza_simple, nacionalidad, sep="-"))

glimpse(casen)





casen_sum("pobreza", filter=T, category="No pobres")

# library(mefa)
# casen_w <- rep(casen, times=casen$expc)

glimpse(casen)
glimpse(casen_w)


casen %>%
  group_by(sexo, r9s) %>%
  summarise(cantidad = n(),
            expc = mean(expc)) %>%
  mutate(cantidad_w = cantidad*expc) %>%
  group_by(sexo) %>%
  mutate(porcentaje_w = cantidad_w/sum(cantidad_w))



#Sumar ----
#Función que calcula la variable elegida y le aplica expansión

casen_sum <- function(variable, filter=FALSE, category="Sí") {
  #Variable: variable a sumar
  #Filter: FALSE para calcular todas las categorías (default), TRUE para filtrar y sacar proporción
  #las columnas con _w tienen aplicado factor de expansión regional
  if (filter == FALSE) {
  casen %>%
    rename(variable = as.character(variable)) %>%
    group_by(sexo, variable) %>%
    filter(!is.na(variable)) %>%
    summarise(expr = mean(expr),
              cantidad = n()) %>%
    mutate(cantidad_w = cantidad*expr) %>%
    group_by(sexo) %>%
    mutate(porcentaje = cantidad/sum(cantidad),
           porcentaje_w = cantidad_w/sum(cantidad_w)) %>%
      select(-expr)
  } else { #Hacer función que filtre algo o que saque el porcentaje de una sola categoría
    casen %>%
      rename(variable = as.character(variable)) %>%
      group_by(sexo, variable) %>%
      filter(!is.na(variable)) %>%
      summarise(expr = mean(expr),
                cantidad = n()) %>%
      mutate(cantidad_w = cantidad*expr) %>%
      group_by(sexo) %>%
      mutate(porcentaje = cantidad/sum(cantidad),
             porcentaje_w = cantidad_w/sum(cantidad_w)) %>%
      filter(variable == as.character(category)) %>%
      ungroup() %>%
      mutate(proporcion_fem = cantidad_w/sum(cantidad_w)) %>%
      select(-expr)
    #La interpretación de proporción es: del total de personas que cumplen el filtro ("Sí", por defecto), un x% son hombres y un x% mujeres
  }
}

casen_sum("r9f", filter=TRUE)


#Medias y medianas----
casen_num <- function(variable, func="mean") {
  #func: puede ser "mean" o "median"
  casen %>%
    rename(variable = as.character(variable)) %>%
    group_by(sexo) %>%
    mutate(variable = replace(variable, variable=="No sabe", NA)) %>%
    mutate(variable = as.numeric(as.character(variable))) %>%
    filter(!is.na(variable)) %>%
    summarise(cantidad = ifelse(func=="mean", mean(as.numeric(variable)), median(as.numeric(variable)))) %>%
    #porcentaje de los hombres
    mutate(proporcion = cantidad[sexo=="Mujer"]/cantidad[sexo=="Hombre"]) 
}

casen_num("ytrabajocor", func="mean")
casen_num("ytrabajocor", func="median")







# r9h. Discriminado por su orientación sexual o identidad de género
casen_sum("r9h", filter=T)

# r9m. Discriminado por participar o no en sindicatos u organizaciones gremiales
casen_sum("r9m", filter=T)


# r9q. Discriminado por su condición de salud o discapacidad
casen_sum("r9q", filter=T)
 
# r9p. Discriminado por pertenecer a un pueblo indígena
casen_sum("r9p", filter=T)


# r9s. No ha sido tratado injustamente o discriminado
casen_sum("r9s", filter=T, category="No")

# r9f. Discriminado por ser extranjero/a
casen_sum("r9f", filter=T) 

# r9b. Discriminado por ser mujer/ser hombre
casen_sum("r9b", filter=T)
 
# r9a. Discriminado por nivel socioeconómico
casen_sum("r9a", filter=T)
# 
# pobreza
casen_sum("pobreza_simple", filter=T, category="Pobres")

casen_sum("pobreza_extrema", filter=T, category="Pobreza extrema")

casen_sum("pobreza_multi_4d", filter=T, category="Pobre")

casen_sum("pobreza_multi_5d", filter=T, category="Pobre")



# indmat
casen_sum("indmat") 


# pco1:1 jefe de hogar
casen_sum("pco1") %>%
  filter(variable == "Jefe(a) de hogar")


# hacinamiento
casen_sum("hacinamiento")

#Hogar carente en hacinamiento
casen_sum("hh_d_hacina", TRUE, "1")

#Hogar carente en servicios básicos
casen_sum("hh_d_servbas", TRUE, "1")

#Hogar carente en jubilaciones
casen_sum("hh_d_jub", TRUE, "1")
#Hogar carente en seguridad social
casen_sum("hh_d_cot", TRUE, "1")
#Hogar carente en escolaridad
casen_sum("hh_d_esc", TRUE, "1")
#Hogar carente en medio ambiente
casen_sum("hh_d_medio", TRUE, "1")
#Hogar carente en trato igualitario
casen_sum("hh_d_tsocial", TRUE, "1")
#Hogar carente en seguridad
casen_sum("hh_d_seg", TRUE, "1")


casen %>%
  select(starts_with("hh")) %>%
  glimpse()

# iae allegamiento externo
#Se refiere a la presencia en una vivienda de más de un hogar, evidenciándose con la figura de más de una vivienda, físicamente, dentro del predio. 
casen_sum("iae")

# iai allegamiento interno
#Es la situación donde se constata la presencia de más de un núcleo familiar dentro de la vivienda.
casen_sum("iai")
# 
# activ condición de actividad
casen_sum("activ", TRUE, "Desocupados")

# esc escolaridad
casen_num("esc", func="mean")
casen_num("esc", func="median")


# qaut quintil autonomo nacional
casen_sum("qaut")
# qautr quintil autonomo regional
casen_sum("qautr")
# 
# 
# ytrabajocor ingreso del trabajo
casen_num("ytrabajocor", func="mean")

casen_num("ytrabajocor", func="median")

# ytrabajocorh ingreso del trabajo del hogar
casen_num("ytrabajocorh", func="mean")

casen_num("ytrabajocorh", func="median")

# y13a pension de alimentos
# 
# y26_2bm1 Monto pensión de vejez (monto 1)
casen_num("y26_2bm1", func="mean")


# y26_2bm2 Monto aporte previsional solidario (monto 2)
casen_num("y26_2bm2", func="mean") 

# y26_2c Mes pasado monto jubilación o pensión de vejez
casen_num("y26_2c", func="mean")


# y26_3c_in same institución
casen_sum("y26_3c_in")

glimpse(casen %>% select(y26_2c))

levels(casen$y26_2c)


#r1a #nacionalidad

casen_sum("r1a", filter=TRUE, category="Otra nacionalidad. Especifique país")

#pobres extranjeros
casen_sum("nacionalidad_pobreza", filter=T, category="Pobres-Extranjero")
