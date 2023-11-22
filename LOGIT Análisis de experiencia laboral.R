#0 Preparar el entorno de trabajo
rm(list=ls())                  
options(warn=-1) 

#0.1 Cargar Librerias
library(foreign)                 
library(dplyr) 
library(stats) 
paquetes <- c("tidyverse", "foreign", "factoextra", "ggplot2", "psych", "htmltools", "klaR")
for (i in paquetes) {if (!require(i, character.only = TRUE)) {install.packages(i);library(i, character.only = TRUE)} else {library(i, character.only = TRUE)}}

#1 Cargar Base

# Varibles a utilizar
#Sexo
#           1 = Hombre 
#           2 = Mujer 
#Edad
#           22-31= Personas con una edad de 22 a 31 aÃ±os
#           32-42= Personas con una edad de 32 a 42 aÃ±os
#           43-52= Personas con una edad de 42 a 52 aÃ±os
#Nivel de escolaridad
#           1 = Posgrado 
#           2 = Licenciatura
#NÃºmero de habilidades Cognitivas (aprender, resolver problemas)
#           Deficiente= Personas con Habilidades cognitivas de 0 a 7
#           Promedio= Personas con Habilidades cognitivas de 8 a 14
#           Sobresaliente= Personas con Habilidades cognitivas de 15 a 21
#NÃºmero de habilidades Sociales (trabajar en equipo, socializar)
#           Deficiente= Personas con Habilidades cognitivas de 0 a 7
#           Promedio= Personas con Habilidades cognitivas de 8 a 14
#           Sobresaliente= Personas con Habilidades cognitivas de 15 a 21
#NÃºmero de habilidades TÃ©cnicas (usar R, usar Excel)
#           Deficiente= Personas con Habilidades cognitivas de 0 a 7
#           Promedio= Personas con Habilidades cognitivas de 8 a 14
#           Sobresaliente= Personas con Habilidades cognitivas de 15 a 21
#NÃºmero de trabajos
#           0-9  = Personas que han tenido de 0 a 9 trabajos
#           10-19= Personas que han tenido de 10 a 19 trabajos
#           20-29= Personas que han tenido de 20 a 29 trabajos 
#           30-39= Personas que han tenido de 30 a 39 trabajos
#Carrera
#           1= EducaciÃ³n= Personas con un Ã¡rea de estudio en educaciÃ³n
#           2= A.H.= Personas con un Ã¡rea de estudio en artes y humanidades
#           3= C.S.D.= Personas con un Ã¡rea de estudio en ciencias sociales y derecho
#           4= A.N.= Personas con un Ã¡rea de estudio en administraciÃ³n y negocios
#           5= C.N.M.= Personas con un Ã¡rea de estudio en ciencias naturales, matemÃ¡ticas
#           6= T.I.= Personas con un Ã¡rea de estudio en tecnologÃ­as de la informaciÃ³n
#           7= I.M.C.= Personas con un Ã¡rea de estudio en ingenierÃ­a, manufactura y construcciÃ³n
#           8= A.V.= Personas con un Ã¡rea de estudio en agronomÃ­a y veterinaria
#           9= C.S.= Personas con un Ã¡rea de estudio en ciencias de la salud
#           10= Servicios= Personas con un Ã¡rea de estudio en servicios
#           11= TÃ©cnico= Personas con un Ã¡rea de estudio en tÃ©cnicos
#Sostenimiento
#           0= Privado
#           1= PÃºblico
#Experiencia
#           0= Sin experiencia
#           1= Con experiencia
#Ingreso
#           Muy bajo= 0 a 9,999
#           Bajo= 10,000 a 19,999
#           Medio= 20,000 a 29,999
#           Alto= 30,000 a 39,999
#           Muy alto= 40,000 a 50,000

#1.1 Modificamos la base para las variables a tomar en cuenta
v_seleccionadas=c("sexo","edad", "nivel_esc","cognitivas", "sociales","tecnicas","trabajos_n","carrera_cve","sostenimiento","experiencia","p3")
base_modelo=Base[,v_seleccionadas]

base_modelo$sexo[base_modelo$sexo != "2"] <- NA

#2. Arreglo de la base de datos
# Quitar respuestas inv?lidas
Base_limpia<-base_modelo%>%drop_na()
Base_limpia= na.omit(base_modelo)


#2.1 Convertir las variables (agrupar)
#Edades
intervalos <- cut(Base_limpia$edad, breaks = c(21, 32, 42, 52), labels = c("22-31", "32-42", "43-52"))
edades_y_grupos <- data.frame(Edad = Base_limpia$edad, Grupo = intervalos)
print(edades_y_grupos)
Base_limpia$edad <- edades_y_grupos$Grupo
#NÃºmero de habilidades Cognitivas
intervalos <- cut(Base_limpia$cognitivas, breaks = c(-Inf,3, 7, 14, 21), labels = c("Muy Deficiente", "Deficiente", "Promedio", "Sobresaliente"))
habilidadesc_y_grupos <- data.frame(Habilidades = Base_limpia$cognitivas, Grupo = intervalos)
print(habilidadesc_y_grupos)
Base_limpia$cognitivas <- habilidadesc_y_grupos$Grupo
#NÃºmero de habilidades Sociales
intervalos <- cut(Base_limpia$sociales, breaks = c(-Inf, 3, 7, 14, 21), labels = c("Muy Deficiente","Deficiente", "Promedio", "Sobresaliente"))
habilidadess_y_grupos <- data.frame(Habilidades = Base_limpia$sociales, Grupo = intervalos)
print(habilidadess_y_grupos)
Base_limpia$sociales <- habilidadess_y_grupos$Grupo
#NÃºmero de habilidades TÃ©cnicas
intervalos <- cut(Base_limpia$tecnicas, breaks = c(-Inf,3, 7, 14, 21), labels = c("Muy Deficiente","Deficiente", "Promedio", "Sobresaliente"))
habilidadest_y_grupos <- data.frame(Habilidades = Base_limpia$tecnicas, Grupo = intervalos)
print(habilidadest_y_grupos)
Base_limpia$tecnicas <- habilidadest_y_grupos$Grupo
#NÃºmero de trabajos
intervalos <- cut(Base_limpia$trabajos_n, breaks = c(-Inf, 9, 19, 29, 39), labels = c("0-9", "10-19", "20-29", "30-39"))
trabajos_y_grupos <- data.frame(N.Trabajos = Base_limpia$trabajos_n, Grupo = intervalos)
print(trabajos_y_grupos)
Base_limpia$trabajos_n <- trabajos_y_grupos$Grupo
#Ingreso
intervalos <- cut(Base_limpia$p3, breaks = c(-Inf, 9999, 19999, 29999, 39999, 50000), labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
ingresos_y_grupos <- data.frame(Ingresos = Base_limpia$p3, Grupo = intervalos)
print(ingresos_y_grupos)
Base_limpia$p3 <- ingresos_y_grupos$Grupo

#Etiquetado de variables
Base_limpia$sexo<-factor(Base_limpia$sexo, levels = c(2), labels = c("Mujer"))
Base_limpia$nivel_esc<-factor(Base_limpia$nivel_esc, levels = c(1,2), labels = c("Posgrado","Licenciatura"))
Base_limpia$carrera_cve<-factor(Base_limpia$carrera_cve, levels = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("EducaciÃ³n","Artes y Humanidades","Ciencias Sociales y Derecho", "AdministraciÃ³n y Negocios","Ciencias Naturales, MatemÃ¡tica","TecnologÃ­as de la InformaciÃ³n","IngenierÃ­a, Manufactura y ConstrucciÃ³n","AgronomÃ­a y Veterinaria","Ciencias de la Salud","Servicios","TÃ©cnico"))
Base_limpia$sostenimiento<-factor(Base_limpia$sostenimiento, levels = c(0,1), labels = c("Privado","PÃºblico"))
Base_limpia$experiencia<-factor(Base_limpia$experiencia, levels = c(0,1), labels = c("Sin experiencia","Con experiencia"))

#3. Ajuste del modelo
regresion <- glm(experiencia ~ edad + nivel_esc + cognitivas + tecnicas + sociales + trabajos_n + carrera_cve + sostenimiento + p3, 
                 data = Base_limpia, family = "binomial")
summary(regresion)


#4. Interpretacion
momios<-exp(coefficients(regresion))%>%round(digits = 4)%>%data.frame()
momios

