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
####Importar base de datos csv
library(readr)
file.choose()#buscar ruta del archivo csv
ruta_csv <-"C:\\Users\\Miroslava\\Documents\\GitHub\\MiPrimerRepositorioIRC\\servicios_integrales_2019-2021.csv"
servicios_integrales_2019_2021<-read_csv(ruta_csv)




servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO") %>% 
  group_by(mes=floor_date(fecha_alta, unit = "month"), municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>%
  ggplot(aes(mes, m,))+
  geom_point(size = 0.3) +
  geom_smooth(se=FALSE, span=1) +
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")+
  facet_trelliscope(
    ~municipio_usuaria,
    ncol = 2,
    nrow = 2,
    
    as_plotly = TRUE)


