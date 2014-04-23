######################################################
# Udacity Course: EDA - Exploratory Data Analysis
# Lesson 2: R Basics
######################################################
# Funciones básicas interesantes:
#
# nchar() -> contar el número de caracteres de un objeto.
# data()  -> cargar un dataset.
# names() -> obtener el nombre de las variables que forman parte del dataset.
# row.names(my.dataset) = c("algo") -> asignar nombres a las filas.
# head/tail(my.data, 10) -> Mostrar los primeros/últimos 10 datos del dataset.
# my.data$... -> acceder a una variable en concreto.
# mean(my.data$...) -> obtener el valor medio de la variable en cuestión "..."
#
######################################################
setwd("C:/Users/Gorka/Dropbox/Proyecto_MSI/WORKSPACE/EDA_Course_Materials/lesson2")
######################################################
statesInfo = read.csv('stateData.csv')

## Elegir componentes que cumplen cierta condicion:
subset(statesInfo, state.region == 1)
statesInfo[statesInfo$state.region == 1, ]

subset(mtcars, mpg >= 30)
mtcars[mtcars$mpg >= 30, ]

## Añadir y eliminar
mtcars$gorka = c("hola")  #añadir una nueva variable
mtcars = subset(mtcars, select = -gorka)
mtcars[mtcars$mpg >= 30 | mtcars$hp < 60, ]
######################################################

reddit = read.csv('reddit.csv')

## Hacer una tabla con aquellas variables "factor" resumiendo la suma total.
table(reddit$employment.status)
summary(reddit)
dim(reddit)

## con str() vemos que variables son "factor" y cuantos nieveles tiene.
str(reddit)

## Para conocer los niveles concretos e una variable "factor":
levels(reddit$age.range)

## Para saber si una variable es del tipo "factor":
is.factor(reddit$income.range)  # Answer: [1] TRUE

library(ggplot2)
qplot(data = reddit, x = age.range)
qplot(data = reddit, x = income.range)

## Ordenar los niveles de la variable tipo "factor" para que se vean mejor los graficos:
## @source: http://statistics.ats.ucla.edu/stat/r/modules/factor_variables.htm
reddit$income.range = ordered(reddit$income.range, levels = c("Under $20,000", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $69,999", "$70,000 - $99,999", "$100,000 - $149,999", "$150,000 or more"))
reddit$age.range = ordered(reddit$age.range, levels = c("Under 18","18-24","25-34","35-44","45-54","55-64","65 or Above"))

## Otra forma:
reddit$age.range = factor(reddit$age.range, levels = c("Under 18","18-24","25-34","35-44",
                                                       "45-54","55-64","65 or Above"), ordered=T)

qplot(data = reddit, x = age.range)
qplot(data = reddit, x = income.range)

