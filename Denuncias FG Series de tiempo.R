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
library(lubridate)
library(tidyr)
####Importar base de datos csv
library(readr)
file.choose()#buscar ruta del archivo csv
ruta_csv <-"C:\\Users\\Miroslava\\Documents\\IRC\\Proyecto Prototípico\\Denuncias FG\\da_victimas_completa_septiembre_2022.csv"
denuncias_victimas_fgj<-read_csv(ruta_csv)



 
########Pasando dataframe original a copia
denuncias_victimas_fgj_c<-denuncias_victimas_fgj


######### Vizualizando formato de fecha
names(denuncias_victimas_fgj_c)
attach(denuncias_victimas_fgj_c)
str(FechaInicio)

######## contar nas
sum(is.na(denuncias_victimas_fgj_c$FechaInicio))
sum(is.na(denuncias_victimas_fgj_c$alcaldia_hechos))
sum(is.na(denuncias_victimas_fgj_c$Delito))



# ********************Ensayo nuevo forecast INICIO BUENO BUENO JA

#Filtro de tipo de violencia
denuncias_desde2019<-denuncias_victimas_fgj_c %>% 
  filter(Delito=="VIOLENCIA FAMILIAR") %>% 
  group_by(FechaInicio) %>% 
  arrange(FechaInicio) 

denuncias_desde2019

denuncias_diacdmx<-denuncias_desde2019 %>%
  group_by(FechaInicio) %>%
  arrange(FechaInicio) %>% 
  count(FechaInicio, sort = TRUE) %>%
  mutate(denunciasdcdmx=n) 

denuncias_diacdmx


denuncias_mescdmx<-denuncias_diacdmx %>% 
  mutate(mes=format(FechaInicio, "%m"),año=format(FechaInicio, "%Y"))%>% 
  group_by(año,mes) %>%
  arrange(año) %>% 
  summarise(denuncias= sum(n) )


denuncias_mescdmx

############## SERIE TEMPORAL
Y<-ts(denuncias_mescdmx$denuncias, start=c(2019,1), frequency = 12)
Y

frequency(Y)
############## EXPLORACIÓN DE LOS DATOS
autoplot(Y)+
  labs(title="No. de denuncias por violencia familiar realizadas en cdmx 2018-2019",
       x="\r\nFecha de denuncia", 
       y= "\r\nNúmero de denuncias recibidas por mes")


############# estacionalidad  de la serie

descom= decompose(Y) 
autoplot(descom)
acf(Y)
pacf(Y)



######### Revisar las diferencias de la serie
DY <-diff(Y)

############# observar las diferencias

autoplot(DY)+
  labs(Title="No. de denuncias por violencia familiar realizadas en cdmx 2018-2019",
       x="\r\nFecha de denuncia", 
       y= "\r\nNúmero de denuncias recibidas por mes")

############# Elaboración de modelo ARIMA
modelo_arima <- auto.arima(Y,d=1, D=1, stepwise= FALSE, approximation=FALSE, trace= TRUE)
print(modelo_arima) #Modelo que más se ajusta a los datos, revisó 61 combinaciones para ver cuál se ajustaba más a los datos


####### Realizamos una revision de los residuos del modelo
checkresiduals(modelo_arima)

####### Se calcula el forecast de denuncias para el próximo año
fcst <- forecast(modelo_arima, h=12, level= c(95))
autoplot(fcst)+
  labs(title="No. de denuncias por violencia familiar realizadas en cdmx 2019-2022 \r\n para los próximos doce meses
       ",
       x="\r\nFecha de denuncia", 
       y= "\r\nNúmero de denuncias recibidas por mes")


# ********************Ensayo nuevo forecast FIN


# *************INICIO trelliscope por dia
denuncias_victimas_fgj_c %>% 
  filter(Delito=="VIOLENCIA FAMILIAR") %>% 
  group_by(FechaInicio, alcaldia_hechos) %>% 
  count(FechaInicio, sort = TRUE) %>%
  summarise(m=n)%>%
  ggplot(aes(FechaInicio, m,))+
  geom_point(size = 0.3) +
  geom_smooth(se=FALSE, span=1) +
  labs(title="No. de denuncias por violencia familiar realizadas en cdmx 2019-2022 \r\n para los próximos doce meses
       ",
       x="\r\nFecha de denuncia", 
       y= "\r\nNúmero de denuncias recibidas por mes")+
  facet_trelliscope(
    ~alcaldia_hechos,
    ncol = 2,
    nrow = 2,
    
    as_plotly = TRUE)

# *************fin trelliscopepor dia



# *************INICIO trelliscope VIOLENCIA FAMILIAR por mes*** se ve TENDENCIA A LA BAJA EN PANDEMIA
denuncias_victimas_fgj_c %>% 
  filter(Delito=="VIOLENCIA FAMILIAR") %>% 
  group_by(mes=floor_date(FechaInicio, unit = "month"), alcaldia_hechos) %>% 
  count(mes, sort = TRUE) %>%
  summarise(m=n)%>%
  ggplot(aes(mes, m,))+
  theme_grey()+
  geom_point(size = 0.3) +
  geom_smooth(se=FALSE, span=1) +
  labs(title="Denuncias de violencia familiar \r\n  FGJ 2019-2022
       ",
       x="Fecha de denuncia", 
       y= "\r\n Número de denuncias recibidas por mes")+
  facet_trelliscope(
    ~alcaldia_hechos,
    ncol = 2,
    nrow = 2,
    
    as_plotly = TRUE)

# *************FIN trelliscope VIOLENCIA FAMILIAR por mes*** 




# *************INICIO mapa de árbol por alcaldía 2022**


dvf2022 <- denuncias_victimas_fgj_c %>% 
  filter(Delito=="VIOLENCIA FAMILIAR", Año_inicio==2022) %>%
  group_by(alcaldia_hechos) %>%
  count(Año_inicio, sort = TRUE) %>%
  summarise(denuncias_totales=n) %>% 
  arrange(-denuncias_totales) %>%
  mutate(parents = "Denuncias 2022") %>%
  ungroup() 

plot_ly(data = dvf2022,
        type= "treemap",
        values = ~denuncias_totales,
        labels= ~ alcaldia_hechos,
        parents=  ~parents,
        domain = list(column=0),
        name = "Denuncias por Violencia Familiar 2022",
        textinfo="label+value+percent parent")

# *************FIN mapa de árbol por alcaldía 2022**

# *************INICIO mapa de árbol por alcaldía total**


dvf2022 <- denuncias_victimas_fgj_c %>% 
  filter(Delito=="VIOLENCIA FAMILIAR", Año_inicio>=2019) %>%
  group_by(alcaldia_hechos) %>%
  count(Delito, sort = TRUE) %>%
  summarise(denuncias_totales=n) %>% 
  arrange(-denuncias_totales) %>%
  mutate(parents = "Denuncias por Violencia Familiar totales") %>%
  ungroup() 

plot_ly(data = dvf2022,
        type= "treemap",
        values = ~denuncias_totales,
        labels= ~ alcaldia_hechos,
        parents=  ~parents,
        domain = list(column=0),
        name = "Denuncias por Violencia Familiar totales",
        textinfo="label+value+percent parent")

# *************FIN mapa de árbol por alcaldía total**


# *************INICIO trelliscope TOTAL DENUNCIAS por mes*** se ve TENDENCIA A LA BAJA EN PANDEMIA
denuncias_victimas_fgj_c %>%  
  group_by(mes=floor_date(FechaInicio, unit = "month"), alcaldia_hechos) %>% 
  count(mes, sort = TRUE) %>%
  summarise(m=n)%>%
  ggplot(aes(mes, m,))+
  theme_grey()+
  geom_point(size = 0.3) +
  geom_smooth(se=FALSE, span=1) +
  labs(title="Denuncias de totales \r\n  FGJ 2019-2022
       ",
       x="Fecha de denuncia", 
       y= "\r\n Número de denuncias recibidas por mes")+
  facet_trelliscope(
    ~alcaldia_hechos,
    ncol = 2,
    nrow = 2,
    
    as_plotly = TRUE)

# *************FIN trelliscope VIOLENCIA FAMILIAR por mes*** se ve más claro








