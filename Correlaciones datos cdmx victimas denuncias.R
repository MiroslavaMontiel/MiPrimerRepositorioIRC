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
####Importar base de datos csv
library(readr)
file.choose()#buscar ruta del archivo csv
ruta_csv <-"C:\\Users\\Miroslava\\Downloads\\denuncias-victimas-pgj (1).csv"
denuncias_victimas_pgj<-read_csv(ruta_csv)



# denuncias_victimas_pgj_h<-denuncias_victimas_pgj %>% 
#   filter(estado_usuaria=="CIUDAD DE MÉXICO", municipio_usuaria=="GUSTAVO A. MADERO") %>% 
#   group_by(fecha_alta, municipio_usuaria) %>% 
#   count(municipio_usuaria, sort = TRUE) %>%
#   mutate(llamadasGAM=n) 
# 
########Pasando dataframe original a copia
denuncias_victimas_pgj_h<-denuncias_victimas_pgj


######### Vizualizando formato de fecha
names(denuncias_victimas_pgj_h)
attach(denuncias_victimas_pgj_h)
str(fechahecho)
str(ao)
# as.Date(fechahecho,origin="1964-02-29")
###as.num(fechahecho)

######### Corrigiendo fecha posixct a solo fecha
denuncias_victimas_pgj_h$fechahecho<-as.Date(denuncias_victimas_pgj_h$fechahecho,origin="1964-02-29" ) 
denuncias_victimas_pgj_h

denuncias_victimas_pgj_hf<-denuncias_victimas_pgj_h

######## contar nas
sum(is.na(denuncias_victimas_pgj_hf$fechahecho))
sum(is.na(denuncias_victimas_pgj_hf$calidadjuridica))
sum(complete.cases(denuncias_victimas_pgj_h$fechahecho))
##apply(denuncias_victimas_pgj_h, MARGIN = 2, function(x) sum(is.na(x)))

########## quitar nAS de la fecha solamente
# denuncias_victimas_pgj_hf<-denuncias_victimas_pgj_h[!is.na(denuncias_victimas_pgj_h$fechahecho)]
# denuncias_victimas_pgj_hf<-drop_na(denuncias_victimas_pgj_h$fechahecho)
denuncias_victimas_pgj_hf<-denuncias_victimas_pgj_h[complete.cases(denuncias_victimas_pgj_h$fechahecho),]
denuncias_victimas_pgj_hf
#reemplazar na con ceros
#denuncias_victimas_pgj_h <- mutate_all(denuncias_victimas_pgj_h, ~replace(., is.na(.), 0))



#

#reemplazar na con muestreo aleatorio simple
# denuncias_victimas_pgj_h <- mutate_all(denuncias_victimas_pgj_hf, ~replace(., is.na(.), 0))
# # 
# denuncias_victimas_pgj_h

######### reemplazar na con muestreo aleatorio simple con reemplazo


# rand.impute<-function(x){# x es un vector de datos que puede contener NA
# 
#   missing<-is.na(x)# missing es un vector que contiene valores true or false dependiendo si es na
# 
#   n.missing<-sum(missing)# n.missing al sumar un vector de numero aleatorios, devuelve el número de verdaderos
# 
#   x.obs<-x[!missing]# x.obs son los valores que tienen dato diferente de NA en x
#   imputed<-x #por defecto devolverá lo mismo que había entrado por parámetro
#   imputed[missing]<-sample(x.obs,n.missing, replace=TRUE) #en los valores que faltaban se reeeemplazan por una muestra de los que si se conocen
#   return(imputed)
# }
# 
# random.impute.data.frame<-function(dataframe, cols){
#   names<-names(dataframe)
#   for(col in cols){
#     name<-paste(names[col], "imputed", sep=".")
#     dataframe[name]=rand.impute(dataframe[,col])
#   }
#   dataframe
# }
# 
# denuncias_victimas_pgj_hf<-random.impute.data.frame(denuncias_victimas_pgj_hf,c(8,9))
# #####no sale da error 



# ********************Ensayo nuevo forecast INICIO BUENO BUENO JA

#Filtro de año 2019
#Comentario q sirve por si quiero Modificar fecha ,fechahecho>="2019-08-01", fechahecho<="2019-08-31"
denuncias_desde2018<-denuncias_victimas_pgj_hf %>% 
  filter(delito=="VIOLENCIA FAMILIAR", ao>="2018") %>% 
  group_by(fechahecho) %>% 
  arrange(fechahecho) 

denuncias_desde2018

denuncias_diacdmx<-denuncias_desde2018 %>%
  group_by(fechahecho) %>%
  arrange(fechahecho) %>% 
  count(fechahecho, sort = TRUE) %>%
  mutate(denunciasdcdmx=n) 

denuncias_diacdmx


denuncias_mescdmx<-denuncias_diacdmx %>% 
  mutate(mes=format(fechahecho, "%m"),año=format(fechahecho, "%Y"))%>% 
  group_by(año,mes) %>%
  arrange(año) %>% 
  summarise(denuncias= sum(n) )


denuncias_mescdmx

############## SERIE TEMPORAL
Y<-ts(denuncias_mescdmx$denuncias, start=c(2018,1), frequency = 12)
Y

frequency(Y)
############## EXPLORACIÓN DE LOS DATOS
autoplot(Y)+
  labs(title="No. de denuncias por violencia familiar realizadas en cdmx 2018-2019",
       x="\r\nFecha de denuncia", 
       y= "\r\nNúmero de denuncias recibidas por mes")


############# estacionalidad  de la serie

descom= decompose(Y) #Ocurre un error en descompose puesto que el número de observaciones es menor a 24 es decir los dosperiodos requeridos

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

####### Se calcula el forecast de llamadas para próximos 6 meses
fcst <- forecast(modelo_arima, h=12, level= c(95))
autoplot(fcst)+
  labs(title="No. de denuncias por violencia familiar realizadas en cdmx 2018-2019 \r\n para los próximos doce meses
       ",
       x="\r\nFecha de denuncia", 
       y= "\r\nNúmero de denuncias recibidas por mes")



# ********************Ensayo nuevo forecast FIN




#Filtro de año
#Comentario q sirve por si quiero Modificar fecha ,fechahecho>="2019-08-01", fechahecho<="2019-08-31"
denuncias_dia<-denuncias_victimas_pgj_hf %>% 
  filter(delito=="VIOLENCIA FAMILIAR", ao>="2018") %>% 
  group_by(fechahecho) %>% 
  count(fechahecho, sort = TRUE) %>%
  arrange(fechahecho) %>%
  mutate(denuncias_violenciafamiliar=n) 

denuncias_dia



library(forecast)


############## SERIE TEMPORAL
Y<-ts(denuncias_dia$n, start=c(2019,01,01), frequency =365)
Y
autoplot(Y)
ajuste<-auto.arima(y=denuncias_dia$n)
summary(ajuste)
predicciones<-forecast(ajuste)
predicciones


p_predict<-autoplot(predicciones)
p_predict #La banda azul es el error, se da un valor en intervalo.

############## EXPLORACIÓN DE LOS DATOS
autoplot(Y)+
  labs(title="No. de denuncias de violencia familiar diarias \r\n 2019
       ",
       x="\r\nFecha de denuncia", 
       y= "\r\nNúmero de denuncias hechas por día")


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












#Quitar NA
denuncias_victimas_pgj[is.na(denuncias_victimas_pgj)] <- 0



#CORRELACIONES
D_v1<-denuncias_victimas_pgj %>%
  filter(sexo == "Femenino") %>%
  group_by(fechahecho) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  mutate(calidadjuridica <- denuncias_victimas_pgj[str_detect("VICTIMA", denuncias_victimas_pgj$calidadjuridica), ])%>% 
  count(calidadjuridica) #%>% 
 
  #mutate(n1 = as.double(n),)
calidadjuridica <- denuncias_victimas_pgj[str_detect("VICTIMA", denuncias_victimas_pgj$calidadjuridica), ]
calidadjuridica
D_v1

#names(V_n2)
#attach(V_n2)
str(V_n1$n1)
str(V_n2$n2)
plot(V_n2$n2,V_n1$n1)
cor.test(V_n2$n2,V_n1$n1)

#P value es 0.06576 y es menor a 0.1