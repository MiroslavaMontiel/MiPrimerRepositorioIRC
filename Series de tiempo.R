#####Cargar librerías
library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(gganimate)
library(gifski)
library(RColorBrewer)
library(gapminder)
library(xlsx)
library(data.table)
library(readxl)
library(readr)
library(trelliscopejs)
library(forecast)
library(fpp2)
library(lubridate)
library(DecomposeR)

llamadas_dia_gam<-servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO", municipio_usuaria=="GUSTAVO A. MADERO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  mutate(llamadasGAM=n) 

llamadas_dia_gam


llamadas_mes_gam<-llamadas_dia_gam %>% 
  mutate(mes=format(fecha_alta, "%m"),año=format(fecha_alta, "%Y"))%>% 
  group_by(mes, año) %>% 
  summarise(llamadas= sum(n) )

   
llamadas_mes_gam

############## SERIE TEMPORAL
Y<-ts(llamadas_mes_gam$llamadas, start=c(2019,1),end = c(2021,12), frequency = 12)
Y

############## EXPLORACIÓN DE LOS DATOS
autoplot(Y)+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por mes")


############# estacionalidad  de la serie

descom= decompose(Y)

autoplot(descom)
acf(Y)
pacf(Y)



######### Revisar las diferencias de la serie
DY <-diff(Y)

############# observar las diferencias

autoplot(DY)+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por mes")

############# Elaboración de modelo ARIMA
modelo_arima <- auto.arima(Y,d=1, D=1, stepwise= FALSE, approximation=FALSE, trace= TRUE)
print(modelo_arima) #Modelo que más se ajusta a los datos, revisó 61 combinaciones para ver cuál se ajustaba más a los datos


####### Realizamos una revision de los residuos del modelo
checkresiduals(modelo_arima)

####### Se calcula el forecast de llamadas para próximos 6 meses
fcst <- forecast(modelo_arima, h=12, level= c(95))
autoplot(fcst)+
  labs(title="No. de llamadas pronosticadas a Línea Mujeres en la Alcaldía GAM \r\n para los próximos doce meses
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por mes")

# *************INICIO mapa de árbol por alcaldía DESDE 2019-2021**
head(servicios_integrales_2019_2021)

lm2021 <- servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO") %>%
  group_by(municipio_usuaria) %>%
  count(estado_usuaria, sort = TRUE) %>%
  summarise(llamadas_totales=n) %>% 
  arrange(-llamadas_totales) %>%
  mutate(parents = "Llamadas a línea Mujeres 2019-2021") %>%
  ungroup() 

plot_ly(data = lm2021,
        type= "treemap",
        values = ~llamadas_totales,
        labels= ~ municipio_usuaria,
        parents=  ~parents,
        domain = list(column=0),
        name = "Llamadas a línea Mujeres 2019-2021",
        textinfo="label+value+percent parent")

# *************FIN mapa de árbol por alcaldía 2022**
# *************INICIO mapa de árbol por tematica 1 marzo 2020**
head(servicios_integrales_2019_2021)

lm2021 <- servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO", año_alta==2020, mes_alta=="Abril") %>%
  group_by(tematica_1) %>%
  count(año_alta, sort = TRUE) %>%
  summarise(llamadas_totales=n) %>% 
  arrange(-llamadas_totales) %>%
  mutate(parents = "Llamadas a línea Mujeres 2021") %>%
  ungroup() 

plot_ly(data = lm2021,
        type= "treemap",
        values = ~llamadas_totales,
        labels= ~ tematica_1,
        parents=  ~parents,
        domain = list(column=0),
        name = "Llamadas a línea Mujeres 2021",
        textinfo="label+value+percent parent")

# *************FIN mapa  mapa de árbol por tematica 1 marzo 2020**


# *************INICIO mapa de árbol por tematica 2 marzo 2020**
head(servicios_integrales_2019_2021)

lm2021 <- servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO", año_alta==2020, mes_alta=="Marzo") %>%
  group_by(tematica_2) %>%
  count(año_alta, sort = TRUE) %>%
  summarise(llamadas_totales=n) %>% 
  arrange(-llamadas_totales) %>%
  mutate(parents = "Temática 2 de Llamadas a línea Mujeres Marzo 2020") %>%
  ungroup() 

plot_ly(data = lm2021,
        type= "treemap",
        values = ~llamadas_totales,
        labels= ~ tematica_2,
        parents=  ~parents,
        domain = list(column=0),
        name = "Temática 2 de Llamadas a línea Mujeres Marzo 2020",
        textinfo="label+value+percent parent")

# *************FIN mapa  mapa de árbol por tematica 2 marzo 2020**



