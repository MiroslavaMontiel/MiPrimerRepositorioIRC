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
#Base de datos INEGI Número 12
TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO", P12_4 != "3") %>%
  select(NOM_MUN, P12_4) %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  summarise(count = n() )  %>%
  arrange(count)  %>%
  mutate(parents="Casos totales de violencia en la niñez por")  %>%
  ungroup()

V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO", P12_4 != "3") %>%
  select(NOM_MUN, P12_4) %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  summarise(count = n() )  %>%
  arrange(count)  %>%
  mutate(parents="Casos totales donde la entrevistada presenció golpes en su casa durante su niñez")  %>%
  ungroup()
V_n
plot_ly(data =V_n,
        type="treemap",
        values = ~count,
        labels= ~ NOM_MUN,
        parents= ~parents,
        domain= list(colum=0),
        name="Casos totales de violencia en la niñez por alcaldía",
        textinfo= "label+value+ parent"
        )


#Mujeres que si insultan a sus hijos, tienen antecedentes de insultos hacía su persona en la niñez? no, 53, si 47
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_12!= "3", P12_12!= "4" ) %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_7) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_7), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n

#Mujeres que no insultan a sus hijos, tienen antecedentes de insultos hacía su persona en la niñez? no, 84.8 si 15.2
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_12!= "1", P12_12!= "2" ) %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_7) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_7), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n

#Mujeres que golpean a sus hijos, tienen antecedentes de golpes hacía su persona en la niñez? no, 43.4 si 56.7
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_13!= "3",P12_13!= "4") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_6) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_6), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n

#Mujeres no que golpean a sus hijos, tienen antecedentes de golpes hacía su persona en la niñez? no, 74 si 26
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_13!= "1",P12_13!= "2",P12_13!= "4") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_6) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_6), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n


#Parejas de mujeres que golpean o insultan a sus hijos, tienen antecedentes de golpes o insultos hacía su persona en la niñez? no, 42.9 si 57.1
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_8!= "8",P12_8!= "b",P12_10!= "3",P12_10!= "4", P12_10!= "b") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_8) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_8), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n


#Parejas de mujeres que NO golpean o insultan a sus hijos, tienen antecedentes de golpes o insultos hacía su persona en la niñez? no, 73 si 28
V_n<-TB_SEC_XII %>%
  
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_8!= "8",P12_8!= "b",P12_10!= "1",P12_10!= "2",P12_10!= "4", P12_10!= "b") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_8) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_8), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n



#Base de datos línea Mujeres
head(servicios_integrales_2019_2021)


servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>% 
  ggplot(aes(x=fecha_alta,
             y= m,
             color= municipio_usuaria))+
  geom_line()+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")+
  transition_reveal(fecha_alta)



servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>% 
  ggplot(aes(x=fecha_alta,
             y= m,
             color= municipio_usuaria))+
  geom_line()+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")+
  transition_reveal(fecha_alta)

servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>%
  ggplot(aes(fecha_alta, m,))+
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

#Gráfica de lineas de la GAM
servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO", municipio_usuaria=="GUSTAVO A. MADERO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>%
  ggplot(aes(x=fecha_alta,
             y= m,
             color= municipio_usuaria))+
  geom_line()+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")

#Serie de Tiempo "
llamadas_dia_gam<-servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO", municipio_usuaria=="GUSTAVO A. MADERO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  mutate(m=n)
ggplot(llamadas_dia_gam, aes(x=fecha_alta,
           y= m,
           color= municipio_usuaria))+
  geom_line()+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")
 
llamadas_dia_gam

#c(2021,01,01)
llamdg<-ts(llamadas_dia_gam$m, start=c(2019), frequency = 365)


llamdg
  
ajuste<-auto.arima((y=llamdg))
summary(ajuste)
autoplot(llamdg)



predicciones<-forecast(ajuste)
min(predicciones[["lower"]])
max(predicciones[["upper"]])
p_predict<-autoplot(predicciones)
p_predict

#CORRELACIONES
names(TB_SEC_XII)
attach(TB_SEC_XII)
str(P12_13)
str(P12_6)
plot(P12_13,P12_6)
cor.test(P12_13,P12_6)

names(TB_SEC_XII)
attach(TB_SEC_XII)
str(P12_12)
str(P12_7)
plot(P12_12,P12_7)
cor.test(P12_12,P12_7)



#limpiando la base de datos
#Insultos a sus hijos, tienen antecedentes de insultos hacía su persona en la niñez? no, 84.8 si 15.2
V_n <- TB_SEC_XII[!(TB_SEC_XII$P12_12 == "4"),]

V_n2<-V_n %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  mutate(P12_7=case_when(P12_7=="2"~"1", P12_7=="3"~"3",TRUE~"1.1")) %>% 
  count(P12_7) %>% 
  mutate(n = as.double(n),) 
  
V_n2

names(V_n2)
attach(V_n2)
str(P12_12)
str(P12_7)
plot(P12_12,P12_7)
cor.test(P12_12,P12_7)

#P value es 0.07171 y es menor a 0.1
####
###
###
####Correlación Buena, PERSONAS ENCUESTADAS QUE FUERON INSULTADAS EN LA NIÑEZ VS, LAS MISMAS PERSONAS ENCUESTADAS QUE INSULTAN A SUS HIJOS



V_n <- TB_SEC_XII[!(TB_SEC_XII$P12_12 == "4"),]

V_n2<-V_n %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  mutate(P12_12=case_when(P12_2=="2"~"1", P12_12=="3"~"3",TRUE~"1.1")) %>% 
  count(P12_12) %>% 
  mutate(n2 = as.double(n),) 
  

V_n2
V_n <- TB_SEC_XII[!(TB_SEC_XII$P12_12 == "4"),]

V_n1<-V_n %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  mutate(P12_7=case_when(P12_7=="2"~"1", P12_7=="3"~"3",TRUE~"1.1")) %>% 
  count(P12_7) %>% 
  mutate(n1 = as.double(n),)
  
V_n1

#names(V_n2)
#attach(V_n2)
str(V_n1$n1)
str(V_n2$n2)
plot(V_n2$n2,V_n1$n1)
cor.test(V_n2$n2,V_n1$n1)

#P value es 0.06576 y es menor a 0.1